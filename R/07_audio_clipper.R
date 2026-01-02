# Audio Clipper for Premier Podcast Summary
# Uses FFmpeg to extract audio clips for highlights

#' Check if FFmpeg is available
#' @return TRUE if FFmpeg is available
check_ffmpeg <- function() {
result <- tryCatch({
  system2("ffmpeg", "-version", stdout = TRUE, stderr = TRUE)
  TRUE
}, error = function(e) {
  FALSE
}, warning = function(w) {
  TRUE  # Warnings are OK, means ffmpeg exists
})

if (!result) {
  log_msg("WARN", "FFmpeg not found. Audio clip extraction will be skipped.")
  log_msg("INFO", "Install FFmpeg: https://ffmpeg.org/download.html")
}

result
}

#' Extract audio clip using FFmpeg
#' @param input_file Path to source audio file
#' @param output_file Path to output clip file
#' @param start_seconds Start time in seconds
#' @param duration_seconds Duration in seconds
#' @param buffer_before Seconds to add before start
#' @param buffer_after Seconds to add after end
#' @return Path to output file, or NULL on failure
extract_clip <- function(input_file,
                       output_file,
                       start_seconds,
                       duration_seconds,
                       buffer_before = CONFIG$clip_buffer_before,
                       buffer_after = CONFIG$clip_buffer_after) {

if (!file.exists(input_file)) {
  log_msg("ERROR", "Input file not found: {input_file}")
  return(NULL)
}

# Apply buffer and ensure non-negative start
actual_start <- max(0, start_seconds - buffer_before)
actual_duration <- duration_seconds + buffer_before + buffer_after

# Ensure output directory exists
ensure_dir(dirname(output_file))

# Build FFmpeg command
# -ss: start time (before -i for fast seeking)
# -t: duration
# -c:a libmp3lame: encode as MP3
# -q:a 2: high quality (VBR)
# -y: overwrite output

args <- c(
  "-ss", as.character(actual_start),
  "-i", input_file,
  "-t", as.character(actual_duration),
  "-c:a", "libmp3lame",
  "-q:a", "2",
  "-y",
  output_file
)

log_msg("INFO", "Extracting clip: {seconds_to_timestamp(start_seconds)} - {seconds_to_timestamp(start_seconds + duration_seconds)}")

result <- tryCatch({
  system2("ffmpeg", args, stdout = FALSE, stderr = FALSE)
  0
}, error = function(e) {
  log_msg("ERROR", "FFmpeg failed: {e$message}")
  1
})

if (result == 0 && file.exists(output_file)) {
  file_size <- file.info(output_file)$size
  log_msg("INFO", "Clip extracted: {output_file} ({round(file_size/1024, 1)} KB)")
  return(output_file)
} else {
  log_msg("ERROR", "Clip extraction failed for {output_file}")
  return(NULL)
}
}

#' Extract all clips for an episode based on analysis
#' @param episode_dir Episode directory containing episode.mp3 and analysis.json
#' @param analysis Analysis results (or NULL to load from file)
#' @param max_clips Maximum number of clips to extract
#' @return tibble of extracted clips
extract_episode_clips <- function(episode_dir,
                                analysis = NULL,
                                max_clips = CONFIG$max_clips_per_episode) {

# Check FFmpeg
if (!check_ffmpeg()) {
  return(tibble::tibble(
    clip_path = character(),
    timestamp_start = character(),
    timestamp_end = character(),
    summary = character(),
    success = logical()
  ))
}

# Load analysis if not provided
if (is.null(analysis)) {
  analysis <- load_analysis(episode_dir)
}

if (is.null(analysis) || length(analysis$highlights) == 0) {
  log_msg("INFO", "No highlights to extract clips for")
  return(tibble::tibble(
    clip_path = character(),
    timestamp_start = character(),
    timestamp_end = character(),
    summary = character(),
    success = logical()
  ))
}

# Path to source MP3
mp3_path <- file.path(episode_dir, "episode.mp3")
if (!file.exists(mp3_path)) {
  log_msg("ERROR", "MP3 not found: {mp3_path}")
  return(NULL)
}

# Create clips directory
clips_dir <- file.path(episode_dir, "clips")
ensure_dir(clips_dir)

# Filter to high/medium relevance and limit count
highlights <- analysis$highlights

# Sort by relevance (High first, then Medium)
relevance_order <- c("High" = 1, "Medium" = 2, "Low" = 3)
highlights <- highlights[order(sapply(highlights, function(h) relevance_order[h$relevance] %||% 3))]

# Take top N
if (length(highlights) > max_clips) {
  highlights <- highlights[1:max_clips]
}

log_msg("INFO", "Extracting {length(highlights)} clips from episode")

# Extract each clip
results <- purrr::map_dfr(seq_along(highlights), function(i) {
  h <- highlights[[i]]

  # Parse timestamps - prefer quote_timestamp for clip start if available
  start_seconds <- tryCatch({
    # Use quote_timestamp if available (exact moment of the quote)
    if (!is.null(h$quote_timestamp) && nchar(h$quote_timestamp) > 0) {
      timestamp_to_seconds(h$quote_timestamp)
    } else {
      timestamp_to_seconds(h$timestamp_start)
    }
  }, error = function(e) {
    log_msg("WARN", "Invalid start timestamp: {h$timestamp_start}")
    NA_real_
  })

  end_seconds <- tryCatch({
    # Use quote_end_timestamp if available (exact end of quote)
    if (!is.null(h$quote_end_timestamp) && nchar(h$quote_end_timestamp) > 0) {
      timestamp_to_seconds(h$quote_end_timestamp)
    } else {
      timestamp_to_seconds(h$timestamp_end)
    }
  }, error = function(e) {
    log_msg("WARN", "Invalid end timestamp: {h$timestamp_end}")
    NA_real_
  })

  if (is.na(start_seconds) || is.na(end_seconds)) {
    return(tibble::tibble(
      clip_path = NA_character_,
      timestamp_start = h$timestamp_start,
      timestamp_end = h$timestamp_end,
      summary = h$summary %||% "",
      success = FALSE
    ))
  }

  duration <- end_seconds - start_seconds

  # Generate clip filename
  clip_filename <- sprintf("clip_%03d_%s.mp3",
                           i,
                           gsub(":", "-", h$timestamp_start))
  clip_path <- file.path(clips_dir, clip_filename)

  # Extract clip
  result <- extract_clip(
    mp3_path,
    clip_path,
    start_seconds,
    duration
  )

  tibble::tibble(
    clip_path = if (!is.null(result)) result else NA_character_,
    timestamp_start = h$timestamp_start,
    timestamp_end = h$timestamp_end,
    summary = h$summary %||% "",
    success = !is.null(result)
  )
})

# Save clips manifest
clips_manifest <- list(
  extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  total_clips = sum(results$success),
  clips = purrr::map(seq_len(nrow(results)), function(i) {
    as.list(results[i, ])
  })
)

manifest_path <- file.path(clips_dir, "clips_manifest.json")
safe_write_json(clips_manifest, manifest_path)

n_success <- sum(results$success)
log_msg("INFO", "Extracted {n_success}/{nrow(results)} clips successfully")

results
}

#' Get relative path for clip (for embedding in Quarto)
#' @param clip_path Absolute clip path
#' @param from_dir Directory the relative path should be from
#' @return Relative path string
get_relative_clip_path <- function(clip_path, from_dir) {
# Calculate relative path from episodes/ to data/episodes/{guid}/clips/
# Quarto pages are in episodes/ directory
# Clips are in data/episodes/{guid}/clips/

if (is.na(clip_path)) {
  return(NA_character_)
}

# Extract the GUID from the clip path
parts <- strsplit(clip_path, "/")[[1]]
guid_idx <- which(parts == "episodes") + 1  # Position after "episodes" in data/episodes/{guid}

if (length(guid_idx) > 0 && guid_idx <= length(parts)) {
  # Construct path from project root
  guid <- parts[guid_idx[length(guid_idx)]]  # Take the last match (in data/episodes/)
  file.path("..", "data", "episodes", guid, "clips", basename(clip_path))
} else {
  clip_path
}
}

#' Load clips manifest for an episode
#' @param episode_dir Episode directory
#' @return List with clips info, or NULL if not found
load_clips_manifest <- function(episode_dir) {
manifest_path <- file.path(episode_dir, "clips", "clips_manifest.json")
if (file.exists(manifest_path)) {
  safe_read_json(manifest_path)
} else {
  NULL
}
}
