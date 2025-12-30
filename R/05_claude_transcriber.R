# Claude Transcript Formatter for Premier Podcast Summary
# Uses Claude API to format raw transcription into clean, readable format

# System prompt for transcript formatting agent
TRANSCRIBER_SYSTEM_PROMPT <- "You are an expert podcast transcriber specializing in political interview podcasts. Your task is to format raw transcription data with speaker diarization into a clean, readable transcript.

This podcast is 'Your Province. Your Premier.' featuring host Wayne Nelson interviewing Alberta Premier Danielle Smith.

## Speaker Identification
- Speaker A or the first speaker is typically the HOST (Wayne Nelson)
- Speaker B or the second speaker is typically the GUEST (Premier Danielle Smith)
- Use these labels consistently: 'WAYNE NELSON:' and 'PREMIER DANIELLE SMITH:'

## Formatting Requirements
1. Add timestamps at the start of each major topic change or every 2-3 minutes minimum
2. Format timestamps as [MM:SS] or [HH:MM:SS] for episodes over 1 hour
3. Group utterances into logical paragraphs (typically 2-4 sentences per paragraph)
4. Clean up obvious transcription errors, filler words (um, uh), and false starts
5. Preserve the exact meaning, context, and key quotes
6. Mark genuinely unclear audio as [inaudible]
7. Use proper punctuation and capitalization

## Output Format
Output clean Markdown suitable for a Quarto document. Use this structure:

```
## [00:00] Opening

**WAYNE NELSON:** [Opening remarks...]

**PREMIER DANIELLE SMITH:** [Response...]

## [05:30] Topic Name

**WAYNE NELSON:** [Question or transition...]
```

Do not include any preamble or explanation - output only the formatted transcript."

#' Create Claude API request
#' @return httr2_request object
claude_request <- function() {
api_key <- get_env_var("ANTHROPIC_API_KEY")

httr2::request(paste0(CONFIG$anthropic_base_url, "/messages")) |>
  httr2::req_headers(
    `x-api-key` = api_key,
    `anthropic-version` = CONFIG$anthropic_version,
    `content-type` = "application/json"
  ) |>
  httr2::req_throttle(rate = CONFIG$anthropic_rate_limit / 60) |>
  httr2::req_retry(
    max_tries = 3,
    backoff = ~2^.x,
    is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529)
  ) |>
  httr2::req_timeout(300)  # 5 minute timeout for long transcripts
}

#' Format raw transcript using Claude
#' @param raw_transcript Raw AssemblyAI response
#' @param episode_metadata Episode metadata (title, pub_date, etc.)
#' @return Formatted transcript as markdown string
format_transcript <- function(raw_transcript, episode_metadata) {
# Extract utterances
utterances <- extract_utterances(raw_transcript)

if (nrow(utterances) == 0) {
  log_msg("WARN", "No utterances to format")
  return("")
}

# Prepare utterances text for Claude
# Format: [TIMESTAMP] Speaker: Text
utterance_text <- purrr::map_chr(seq_len(nrow(utterances)), function(i) {
  u <- utterances[i, ]
  timestamp <- ms_to_timestamp(u$start_ms)
  sprintf("[%s] %s: %s", timestamp, u$speaker, u$text)
}) |>
  paste(collapse = "\n\n")

# Build the prompt
user_message <- sprintf(
  "Format the following raw transcript from the podcast episode '%s' (aired %s). The transcript has speaker diarization with timestamps.\n\nRAW TRANSCRIPT:\n%s",
  episode_metadata$title,
  format(episode_metadata$pub_date, "%B %d, %Y"),
  utterance_text
)

log_msg("INFO", "Sending transcript to Claude for formatting ({nrow(utterances)} utterances)")

# Make API call
body <- list(
  model = CONFIG$anthropic_model,
  max_tokens = CONFIG$anthropic_max_tokens,
  system = TRANSCRIBER_SYSTEM_PROMPT,
  messages = list(
    list(role = "user", content = user_message)
  )
)

response <- claude_request() |>
  httr2::req_body_json(body) |>
  httr2::req_perform()

result <- httr2::resp_body_json(response)

# Extract text from response
formatted_text <- result$content[[1]]$text

log_msg("INFO", "Received formatted transcript ({nchar(formatted_text)} characters)")
formatted_text
}

#' Save formatted transcript
#' @param transcript_md Formatted markdown transcript
#' @param episode_dir Episode directory
#' @param episode_metadata Episode metadata
#' @return Path to saved file
save_formatted_transcript <- function(transcript_md, episode_dir, episode_metadata) {
# Save markdown file
md_path <- file.path(episode_dir, "transcript.md")
writeLines(transcript_md, md_path)
log_msg("INFO", "Saved formatted transcript to {md_path}")

# Also save as JSON with metadata
transcript_data <- list(
  episode_guid = episode_metadata$guid,
  episode_title = episode_metadata$title,
  episode_date = format(episode_metadata$pub_date, "%Y-%m-%d"),
  formatted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  transcript_markdown = transcript_md
)

json_path <- file.path(episode_dir, "transcript.json")
safe_write_json(transcript_data, json_path)

md_path
}

#' Full transcript formatting pipeline for one episode
#' @param episode_dir Episode directory
#' @param episode_metadata Episode metadata
#' @param force Re-format even if already exists
#' @return Path to formatted transcript
format_episode_transcript <- function(episode_dir, episode_metadata, force = FALSE) {
# Check for existing formatted transcript
md_path <- file.path(episode_dir, "transcript.md")
if (file.exists(md_path) && !force) {
  log_msg("INFO", "Formatted transcript already exists: {md_path}")
  return(md_path)
}

# Load raw transcript
raw_path <- file.path(episode_dir, "assemblyai_raw.json")
if (!file.exists(raw_path)) {
  stop("Raw transcript not found: ", raw_path)
}

raw_transcript <- safe_read_json(raw_path)

# Format with Claude
formatted_md <- format_transcript(raw_transcript, episode_metadata)

# Save
save_formatted_transcript(formatted_md, episode_dir, episode_metadata)
}

#' Load formatted transcript from episode directory
#' @param episode_dir Episode directory
#' @return Markdown transcript or NULL if not found
load_formatted_transcript <- function(episode_dir) {
md_path <- file.path(episode_dir, "transcript.md")
if (file.exists(md_path)) {
  paste(readLines(md_path), collapse = "\n")
} else {
  NULL
}
}
