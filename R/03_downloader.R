# MP3 Downloader for Premier Podcast Summary
# Downloads podcast episodes from RSS feed URLs

#' Download MP3 file for an episode
#' @param episode tibble row with episode data (must have mp3_url and guid)
#' @param data_dir Base data directory
#' @return Path to downloaded file, or NULL on failure
download_episode_mp3 <- function(episode, data_dir = CONFIG$data_dir) {
# Create episode directory
episode_dir <- file.path(get_project_root(), data_dir, episode$guid)
ensure_dir(episode_dir)

# Target file path
mp3_path <- file.path(episode_dir, "episode.mp3")

# Skip if already downloaded
if (file.exists(mp3_path)) {
  file_size <- file.info(mp3_path)$size
  log_msg("INFO", "MP3 already exists: {mp3_path} ({round(file_size/1024/1024, 1)} MB)")
  return(mp3_path)
}

log_msg("INFO", "Downloading: {episode$title}")
log_msg("INFO", "URL: {episode$mp3_url}")

tryCatch({
  # Download with progress
  response <- httr2::request(episode$mp3_url) |>
    httr2::req_progress() |>
    httr2::req_timeout(600) |>  # 10 minute timeout for large files
    httr2::req_perform()

  # Write to file
  writeBin(httr2::resp_body_raw(response), mp3_path)

  # Verify file was written
  if (file.exists(mp3_path)) {
    file_size <- file.info(mp3_path)$size
    log_msg("INFO", "Downloaded successfully: {round(file_size/1024/1024, 1)} MB")
    return(mp3_path)
  } else {
    log_msg("ERROR", "File was not written: {mp3_path}")
    return(NULL)
  }
}, error = function(e) {
  log_msg("ERROR", "Download failed: {e$message}")
  # Clean up partial download
  if (file.exists(mp3_path)) {
    file.remove(mp3_path)
  }
  return(NULL)
})
}

#' Batch download multiple episodes
#' @param episodes_df tibble of episodes to download
#' @param data_dir Base data directory
#' @return tibble with download status for each episode
download_episodes <- function(episodes_df, data_dir = CONFIG$data_dir) {
log_msg("INFO", "Starting download of {nrow(episodes_df)} episodes")

results <- purrr::map_dfr(seq_len(nrow(episodes_df)), function(i) {
  episode <- episodes_df[i, ]
  log_msg("INFO", "Downloading episode {i}/{nrow(episodes_df)}: {episode$title}")

  mp3_path <- download_episode_mp3(episode, data_dir)

  tibble::tibble(
    guid = episode$guid,
    title = episode$title,
    mp3_path = mp3_path %||% NA_character_,
    success = !is.null(mp3_path),
    downloaded_at = if (!is.null(mp3_path)) format(Sys.time(), "%Y-%m-%dT%H:%M:%S") else NA_character_
  )
})

# Summary
n_success <- sum(results$success)
n_failed <- sum(!results$success)
log_msg("INFO", "Download complete: {n_success} succeeded, {n_failed} failed")

results
}

#' Check if episode MP3 exists
#' @param episode_guid Episode GUID
#' @param data_dir Base data directory
#' @return TRUE if MP3 file exists
mp3_exists <- function(episode_guid, data_dir = CONFIG$data_dir) {
mp3_path <- file.path(get_project_root(), data_dir, episode_guid, "episode.mp3")
file.exists(mp3_path)
}

#' Get MP3 file path for episode
#' @param episode_guid Episode GUID
#' @param data_dir Base data directory
#' @return Path to MP3 file (may not exist)
get_mp3_path <- function(episode_guid, data_dir = CONFIG$data_dir) {
file.path(get_project_root(), data_dir, episode_guid, "episode.mp3")
}

#' Get file size of episode MP3
#' @param episode_guid Episode GUID
#' @param data_dir Base data directory
#' @return File size in bytes, or NA if file doesn't exist
get_mp3_size <- function(episode_guid, data_dir = CONFIG$data_dir) {
mp3_path <- get_mp3_path(episode_guid, data_dir)
if (file.exists(mp3_path)) {
  file.info(mp3_path)$size
} else {
  NA_real_
}
}
