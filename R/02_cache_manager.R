# Cache Manager for Premier Podcast Summary
# Manages episode processing state to avoid reprocessing

#' Initialize cache directory and files
#' @return Invisible TRUE on success
init_cache <- function() {
cache_path <- file.path(get_project_root(), CONFIG$cache_dir)
ensure_dir(cache_path)

cache_file <- file.path(cache_path, CONFIG$episodes_cache_file)
if (!file.exists(cache_file)) {
  initial_cache <- list(
    last_updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    episodes = list()
  )
  safe_write_json(initial_cache, cache_file)
  log_msg("INFO", "Initialized empty episodes cache")
}

invisible(TRUE)
}

#' Load episodes cache
#' @return tibble of cached episode states
load_episodes_cache <- function() {
cache_file <- file.path(get_project_root(), CONFIG$cache_dir, CONFIG$episodes_cache_file)
cache_data <- safe_read_json(cache_file)

if (is.null(cache_data) || length(cache_data$episodes) == 0) {
  return(tibble::tibble(
    guid = character(),
    status = character(),
    processed_at = character(),
    mp3_path = character(),
    transcript_id = character(),
    error_message = character()
  ))
}

# Convert list of episodes to tibble
purrr::map_dfr(cache_data$episodes, function(ep) {
  tibble::tibble(
    guid = ep$guid %||% NA_character_,
    status = ep$status %||% EPISODE_STATUS$pending,
    processed_at = ep$processed_at %||% NA_character_,
    mp3_path = ep$mp3_path %||% NA_character_,
    transcript_id = ep$transcript_id %||% NA_character_,
    error_message = ep$error_message %||% NA_character_
  )
})
}

#' Save episodes cache
#' @param cache_df tibble of episode states
#' @return Invisible TRUE on success
save_episodes_cache <- function(cache_df) {
cache_file <- file.path(get_project_root(), CONFIG$cache_dir, CONFIG$episodes_cache_file)

# Convert tibble to list format
episodes_list <- purrr::map(seq_len(nrow(cache_df)), function(i) {
  row <- cache_df[i, ]
  list(
    guid = row$guid,
    status = row$status,
    processed_at = row$processed_at,
    mp3_path = row$mp3_path,
    transcript_id = row$transcript_id,
    error_message = row$error_message
  )
})

cache_data <- list(
  last_updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  episodes = episodes_list
)

safe_write_json(cache_data, cache_file)
invisible(TRUE)
}

#' Check if an episode needs processing
#' @param guid Episode GUID
#' @param cache_df Cache tibble
#' @return TRUE if episode needs processing
needs_processing <- function(guid, cache_df) {
if (nrow(cache_df) == 0) {
  return(TRUE)
}

cached <- cache_df |> dplyr::filter(guid == !!guid)

if (nrow(cached) == 0) {
  return(TRUE)
}

# Episode needs processing if not complete and not in error state
status <- cached$status[1]
!(status %in% c(EPISODE_STATUS$complete, EPISODE_STATUS$error))
}

#' Update episode status in cache
#' @param guid Episode GUID
#' @param status New status
#' @param cache_df Cache tibble
#' @param extra_fields Named list of additional fields to update
#' @return Updated cache tibble
update_episode_status <- function(guid, status, cache_df, extra_fields = list()) {
now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

if (nrow(cache_df) == 0 || !guid %in% cache_df$guid) {
  # Add new episode to cache
  new_row <- tibble::tibble(
    guid = guid,
    status = status,
    processed_at = now,
    mp3_path = extra_fields$mp3_path %||% NA_character_,
    transcript_id = extra_fields$transcript_id %||% NA_character_,
    error_message = extra_fields$error_message %||% NA_character_
  )
  cache_df <- dplyr::bind_rows(cache_df, new_row)
} else {
  # Update existing episode
  cache_df <- cache_df |>
    dplyr::mutate(
      status = dplyr::if_else(guid == !!guid, !!status, status),
      processed_at = dplyr::if_else(guid == !!guid, !!now, processed_at)
    )

  # Update extra fields if provided
  if (!is.null(extra_fields$mp3_path)) {
    cache_df <- cache_df |>
      dplyr::mutate(mp3_path = dplyr::if_else(guid == !!guid, !!extra_fields$mp3_path, mp3_path))
  }
  if (!is.null(extra_fields$transcript_id)) {
    cache_df <- cache_df |>
      dplyr::mutate(transcript_id = dplyr::if_else(guid == !!guid, !!extra_fields$transcript_id, transcript_id))
  }
  if (!is.null(extra_fields$error_message)) {
    cache_df <- cache_df |>
      dplyr::mutate(error_message = dplyr::if_else(guid == !!guid, !!extra_fields$error_message, error_message))
  }
}

# Save updated cache
save_episodes_cache(cache_df)
cache_df
}

#' Get episodes that need processing
#' @param all_episodes tibble of all episodes from RSS
#' @param cache_df Cache tibble
#' @param max_episodes Maximum number of episodes to process
#' @return tibble of episodes to process, with cache status merged
get_episodes_to_process <- function(all_episodes, cache_df, max_episodes = CONFIG$max_episodes) {
# Get the most recent episodes
recent <- all_episodes |>
  dplyr::arrange(dplyr::desc(pub_date)) |>
  dplyr::slice_head(n = max_episodes)

# Filter to those needing processing
to_process <- recent |>
  dplyr::filter(purrr::map_lgl(guid, ~needs_processing(.x, cache_df)))

# Add episode directory path
to_process <- to_process |>
  dplyr::mutate(
    episode_dir = file.path(get_project_root(), CONFIG$data_dir, guid)
  )

log_msg("INFO", "Found {nrow(to_process)} episodes needing processing out of {nrow(recent)} recent")
to_process
}

#' Get the current status of an episode
#' @param guid Episode GUID
#' @param cache_df Cache tibble
#' @return Status string or NULL if not in cache
get_episode_status <- function(guid, cache_df) {
if (nrow(cache_df) == 0) {
  return(NULL)
}

cached <- cache_df |> dplyr::filter(guid == !!guid)

if (nrow(cached) == 0) {
  return(NULL)
}

cached$status[1]
}

#' Clear cache for an episode (for reprocessing)
#' @param guid Episode GUID
#' @param cache_df Cache tibble
#' @return Updated cache tibble
clear_episode_cache <- function(guid, cache_df) {
cache_df <- cache_df |>
  dplyr::filter(guid != !!guid)

save_episodes_cache(cache_df)
log_msg("INFO", "Cleared cache for episode {guid}")
cache_df
}
