# Claude Healthcare Content Analyzer for Premier Podcast Summary
# Uses Claude API to identify healthcare-related content in transcripts

# System prompt for healthcare content analysis
ANALYZER_SYSTEM_PROMPT <- "You are an expert podcast content analyst specializing in healthcare policy. You are analyzing transcripts from 'Your Province. Your Premier.' - a podcast featuring Alberta Premier Danielle Smith discussing provincial issues.

## Your Task
Analyze the provided transcript and identify all content related to:
- Physicians, doctors, and medical professionals
- Healthcare system and health policy
- Hospitals, clinics, and healthcare facilities
- Alberta Health Services (AHS)
- Medical services, access, and wait times
- Healthcare funding and reform
- Patient care and outcomes
- Nursing and allied health professions
- Emergency services and ambulance
- Mental health services
- Primary care and family medicine

## Analysis Requirements
For each healthcare-related segment you identify:
1. Provide the timestamp range [MM:SS - MM:SS]
2. Write a concise summary (1-2 sentences)
3. Extract a notable direct quote if available (use exact words from transcript)
4. Assign a relevance score: High, Medium, or Low
   - High: Direct policy announcements, specific healthcare initiatives, detailed discussion
   - Medium: General healthcare mentions, context for healthcare topics
   - Low: Brief or tangential healthcare references
5. List the specific healthcare topics covered

## Output Format
You must output valid JSON with this exact structure:
```json
{
  \"overall_summary\": \"2-3 sentence summary of healthcare content in this episode\",
  \"healthcare_focus_score\": 75,
  \"total_healthcare_minutes\": 12.5,
  \"highlights\": [
    {
      \"timestamp_start\": \"05:30\",
      \"timestamp_end\": \"08:45\",
      \"summary\": \"Premier discusses new physician recruitment initiative for rural Alberta\",
      \"quote\": \"We are committed to bringing 500 new family doctors to underserved communities over the next three years.\",
      \"relevance\": \"High\",
      \"topics\": [\"physician recruitment\", \"rural healthcare\", \"primary care\"]
    }
  ]
}
```

## Important Notes
- Only output the JSON, no additional text or explanation
- healthcare_focus_score is 0-100 representing percentage of episode discussing healthcare
- Include ALL healthcare-related segments, even brief mentions (with Low relevance)
- Quotes must be exact text from the transcript
- If no healthcare content is found, return empty highlights array with score 0"

#' Analyze transcript for healthcare content
#' @param transcript_data Either markdown text or path to transcript.json
#' @param episode_metadata Episode metadata
#' @return Analysis results as list
analyze_transcript <- function(transcript_data, episode_metadata) {
# Handle both markdown text and JSON data
if (is.character(transcript_data) && file.exists(transcript_data)) {
  transcript_json <- safe_read_json(transcript_data)
  transcript_text <- transcript_json$transcript_markdown
} else if (is.list(transcript_data) && !is.null(transcript_data$transcript_markdown)) {
  transcript_text <- transcript_data$transcript_markdown
} else {
  transcript_text <- as.character(transcript_data)
}

if (is.null(transcript_text) || nchar(transcript_text) < 100) {
  log_msg("WARN", "Transcript too short for analysis")
  return(list(
    overall_summary = "Transcript too short for analysis",
    healthcare_focus_score = 0,
    total_healthcare_minutes = 0,
    highlights = list()
  ))
}

# Build prompt
user_message <- sprintf(
  "Analyze the following transcript from the podcast episode '%s' (aired %s) for healthcare-related content.\n\nTRANSCRIPT:\n%s",
  episode_metadata$title,
  format(episode_metadata$pub_date, "%B %d, %Y"),
  transcript_text
)

log_msg("INFO", "Sending transcript to Claude for healthcare analysis")

# Make API call
body <- list(
  model = CONFIG$anthropic_model,
  max_tokens = CONFIG$anthropic_max_tokens,
  system = ANALYZER_SYSTEM_PROMPT,
  messages = list(
    list(role = "user", content = user_message)
  )
)

response <- claude_request() |>
  httr2::req_body_json(body) |>
  httr2::req_perform()

result <- httr2::resp_body_json(response)

# Extract JSON from response
response_text <- result$content[[1]]$text

# Clean up response - remove markdown code fences if present
clean_json <- response_text

# Remove ```json ... ``` or ``` ... ``` wrapper
if (grepl("^\\s*```", clean_json)) {
  # Remove opening fence (```json or ```)
  clean_json <- sub("^\\s*```(json)?\\s*\n?", "", clean_json)
  # Remove closing fence
  clean_json <- sub("\\s*```\\s*$", "", clean_json)
}

# Trim whitespace
clean_json <- trimws(clean_json)

# Parse JSON response
analysis <- tryCatch({
  jsonlite::fromJSON(clean_json, simplifyVector = FALSE)
}, error = function(e) {
  log_msg("WARN", "Failed to parse analysis JSON: {e$message}")
  # Try to extract JSON object from text as fallback
  json_match <- regmatches(clean_json, regexpr("\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}", clean_json, perl = TRUE))
  if (length(json_match) > 0 && nchar(json_match[1]) > 10) {
    tryCatch({
      jsonlite::fromJSON(json_match[1], simplifyVector = FALSE)
    }, error = function(e2) {
      list(
        overall_summary = "Failed to parse analysis",
        healthcare_focus_score = 0,
        total_healthcare_minutes = 0,
        highlights = list(),
        raw_response = response_text
      )
    })
  } else {
    list(
      overall_summary = "Failed to parse analysis",
      healthcare_focus_score = 0,
      total_healthcare_minutes = 0,
      highlights = list(),
      raw_response = response_text
    )
  }
})

n_highlights <- length(analysis$highlights)
log_msg("INFO", "Analysis complete: {n_highlights} healthcare highlights found")

analysis
}

#' Save analysis results
#' @param analysis Analysis results from analyze_transcript
#' @param episode_dir Episode directory
#' @param episode_metadata Episode metadata
#' @return Path to saved file
save_analysis <- function(analysis, episode_dir, episode_metadata) {
# Add metadata
analysis$episode_guid <- episode_metadata$guid
analysis$episode_title <- episode_metadata$title
analysis$episode_date <- format(episode_metadata$pub_date, "%Y-%m-%d")
analysis$analyzed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

# Save JSON
json_path <- file.path(episode_dir, "analysis.json")
safe_write_json(analysis, json_path)

json_path
}

#' Full analysis pipeline for one episode
#' @param episode_dir Episode directory
#' @param episode_metadata Episode metadata
#' @param force Re-analyze even if already exists
#' @return Analysis results as list
analyze_episode <- function(episode_dir, episode_metadata, force = FALSE) {
# Check for existing analysis
json_path <- file.path(episode_dir, "analysis.json")
if (file.exists(json_path) && !force) {
  log_msg("INFO", "Analysis already exists: {json_path}")
  return(safe_read_json(json_path))
}

# Load transcript
transcript_path <- file.path(episode_dir, "transcript.json")
if (!file.exists(transcript_path)) {
  stop("Transcript not found: ", transcript_path)
}

transcript_data <- safe_read_json(transcript_path)

# Analyze
analysis <- analyze_transcript(transcript_data, episode_metadata)

# Save
save_analysis(analysis, episode_dir, episode_metadata)

analysis
}

#' Load analysis from episode directory
#' @param episode_dir Episode directory
#' @return Analysis results as list, or NULL if not found
load_analysis <- function(episode_dir) {
json_path <- file.path(episode_dir, "analysis.json")
if (file.exists(json_path)) {
  safe_read_json(json_path)
} else {
  NULL
}
}

#' Get high-relevance highlights
#' @param analysis Analysis results
#' @return List of high-relevance highlights only
get_high_relevance_highlights <- function(analysis) {
if (is.null(analysis$highlights) || length(analysis$highlights) == 0) {
  return(list())
}

purrr::keep(analysis$highlights, function(h) {
  h$relevance == "High"
})
}

#' Check if episode has significant healthcare content
#' @param analysis Analysis results
#' @param threshold Minimum healthcare_focus_score
#' @return TRUE if episode has significant healthcare content
has_healthcare_content <- function(analysis, threshold = 10) {
if (is.null(analysis$healthcare_focus_score)) {
  return(FALSE)
}
analysis$healthcare_focus_score >= threshold
}
