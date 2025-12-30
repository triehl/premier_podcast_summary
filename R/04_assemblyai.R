# AssemblyAI Integration for Premier Podcast Summary
# Handles audio transcription with speaker diarization

#' Create AssemblyAI API request builder
#' @param endpoint API endpoint (e.g., "/upload", "/transcript")
#' @return httr2_request object
assemblyai_request <- function(endpoint) {
api_key <- get_env_var("ASSEMBLYAI_API_KEY")
base_url <- CONFIG$assemblyai_base_url

httr2::request(paste0(base_url, endpoint)) |>
  httr2::req_headers(
    "Authorization" = api_key,
    "Content-Type" = "application/json"
  ) |>
  httr2::req_throttle(rate = CONFIG$assemblyai_rate_limit / 60) |>
  httr2::req_retry(
    max_tries = 3,
    backoff = ~2^.x,
    is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503)
  )
}

#' Upload audio file to AssemblyAI
#' @param file_path Path to local audio file
#' @return Upload URL from AssemblyAI
upload_audio <- function(file_path) {
if (!file.exists(file_path)) {
  stop("Audio file not found: ", file_path)
}

log_msg("INFO", "Uploading audio to AssemblyAI: {file_path}")
file_size <- file.info(file_path)$size
log_msg("INFO", "File size: {round(file_size/1024/1024, 1)} MB")

# Read file as raw binary
audio_data <- readBin(file_path, "raw", n = file_size)

response <- httr2::request(paste0(CONFIG$assemblyai_base_url, "/upload")) |>
  httr2::req_headers(
    "Authorization" = get_env_var("ASSEMBLYAI_API_KEY"),
    "Content-Type" = "application/octet-stream"
  ) |>
  httr2::req_body_raw(audio_data) |>
  httr2::req_timeout(600) |>  # 10 minute timeout
  httr2::req_perform()

result <- httr2::resp_body_json(response)
upload_url <- result$upload_url

log_msg("INFO", "Upload complete. URL: {substr(upload_url, 1, 50)}...")
upload_url
}

#' Submit transcription job with speaker diarization
#' @param audio_url URL of uploaded audio (from upload_audio)
#' @param speakers_expected Expected number of speakers
#' @return Transcript ID for polling
submit_transcription <- function(audio_url, speakers_expected = CONFIG$speakers_expected) {
log_msg("INFO", "Submitting transcription job with {speakers_expected} expected speakers")

body <- list(
  audio_url = audio_url,
  speaker_labels = TRUE,
  speakers_expected = speakers_expected
)

response <- assemblyai_request("/transcript") |>
  httr2::req_body_json(body) |>
  httr2::req_perform()

result <- httr2::resp_body_json(response)
transcript_id <- result$id

log_msg("INFO", "Transcription job submitted. ID: {transcript_id}")
transcript_id
}

#' Poll for transcription completion
#' @param transcript_id Transcript ID from submit_transcription
#' @param poll_interval Seconds between status checks
#' @param max_attempts Maximum number of polling attempts
#' @return Full transcript response
poll_transcription <- function(transcript_id,
                             poll_interval = CONFIG$assemblyai_poll_interval,
                             max_attempts = CONFIG$assemblyai_max_poll_attempts) {
log_msg("INFO", "Polling for transcription completion...")

attempt <- 0
while (attempt < max_attempts) {
  attempt <- attempt + 1

  response <- assemblyai_request(paste0("/transcript/", transcript_id)) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(response)
  status <- result$status

  if (status == "completed") {
    log_msg("INFO", "Transcription completed after {attempt} polls")
    return(result)
  } else if (status == "error") {
    error_msg <- result$error %||% "Unknown error"
    log_msg("ERROR", "Transcription failed: {error_msg}")
    stop("Transcription failed: ", error_msg)
  } else {
    # Still processing
    if (attempt %% 10 == 0) {
      log_msg("INFO", "Still processing... (attempt {attempt}, status: {status})")
    }
    Sys.sleep(poll_interval)
  }
}

stop("Transcription polling timed out after ", max_attempts, " attempts")
}

#' Full transcription pipeline for one episode
#' @param episode_dir Directory containing episode.mp3
#' @param force Re-transcribe even if already exists
#' @return Transcript data as list
transcribe_episode <- function(episode_dir, force = FALSE) {
# Check for existing transcription
raw_transcript_path <- file.path(episode_dir, "assemblyai_raw.json")
if (file.exists(raw_transcript_path) && !force) {
  log_msg("INFO", "Transcription already exists, loading from cache")
  return(safe_read_json(raw_transcript_path))
}

# Check for MP3 file
mp3_path <- file.path(episode_dir, "episode.mp3")
if (!file.exists(mp3_path)) {
  stop("MP3 file not found: ", mp3_path)
}

log_msg("INFO", "Starting transcription pipeline for: {episode_dir}")

# Step 1: Upload
upload_url <- upload_audio(mp3_path)

# Step 2: Submit
transcript_id <- submit_transcription(upload_url)

# Step 3: Poll
result <- poll_transcription(transcript_id)

# Step 4: Save raw result
safe_write_json(result, raw_transcript_path)
log_msg("INFO", "Saved raw transcription to {raw_transcript_path}")

result
}

#' Extract utterances from AssemblyAI response
#' @param raw_transcript Raw AssemblyAI response
#' @return tibble of utterances with speaker, text, start, end
extract_utterances <- function(raw_transcript) {
utterances <- raw_transcript$utterances

if (is.null(utterances) || length(utterances) == 0) {
  log_msg("WARN", "No utterances found in transcript")
  return(tibble::tibble(
    speaker = character(),
    text = character(),
    start_ms = numeric(),
    end_ms = numeric()
  ))
}

purrr::map_dfr(utterances, function(u) {
  tibble::tibble(
    speaker = u$speaker %||% "Unknown",
    text = u$text %||% "",
    start_ms = u$start %||% 0,
    end_ms = u$end %||% 0
  )
})
}

#' Get transcript text without speaker labels
#' @param raw_transcript Raw AssemblyAI response
#' @return Plain text transcript
get_plain_text <- function(raw_transcript) {
raw_transcript$text %||% ""
}

#' Get transcript duration
#' @param raw_transcript Raw AssemblyAI response
#' @return Duration in seconds
get_transcript_duration <- function(raw_transcript) {
if (!is.null(raw_transcript$audio_duration)) {
  raw_transcript$audio_duration
} else {
  # Calculate from last utterance
  utterances <- raw_transcript$utterances
  if (!is.null(utterances) && length(utterances) > 0) {
    last <- utterances[[length(utterances)]]
    last$end / 1000
  } else {
    NA_real_
  }
}
}
