# Claude Healthcare Content Analyzer for Premier Podcast Summary
# Uses Claude API to identify healthcare-related content in transcripts

#' Find the timestamp where a quote begins using AssemblyAI word data
#' @param quote The quote text to find
#' @param raw_transcript Raw AssemblyAI response with word-level timestamps
#' @return Timestamp string in MM:SS format, or NULL if not found
find_quote_timestamp <- function(quote, raw_transcript) {
  # Get all words with timestamps
  words <- raw_transcript$words
  if (is.null(words) || length(words) == 0) {
    return(NULL)
  }

  # Normalize quote for matching (lowercase, remove extra spaces/punctuation)
  quote_normalized <- tolower(trimws(quote))
  quote_normalized <- gsub("[.,!?;:'\"()]", "", quote_normalized)
  quote_words <- strsplit(quote_normalized, "\\s+")[[1]]

  if (length(quote_words) < 3) {
    return(NULL)
  }

  # Search for the first few words of the quote
  search_words <- quote_words[1:min(6, length(quote_words))]

  # Build word text array for searching
  word_texts <- tolower(sapply(words, function(w) {
    gsub("[.,!?;:'\"()]", "", w$text %||% "")
  }))

  # Sliding window search

  for (i in seq_along(word_texts)) {
    if (i + length(search_words) - 1 > length(word_texts)) {
      break
    }

    # Check if this window matches
    window <- word_texts[i:(i + length(search_words) - 1)]
    if (all(window == search_words)) {
      # Found it - return the timestamp of the first word
      start_ms <- words[[i]]$start %||% 0
      return(ms_to_timestamp(start_ms))
    }
  }

  # Fallback: try partial match with first 4 words
  if (length(quote_words) >= 4) {
    search_words <- quote_words[1:4]
    for (i in seq_along(word_texts)) {
      if (i + 3 > length(word_texts)) {
        break
      }
      window <- word_texts[i:(i + 3)]
      if (all(window == search_words)) {
        start_ms <- words[[i]]$start %||% 0
        return(ms_to_timestamp(start_ms))
      }
    }
  }

  NULL
}

#' Create Claude API request builder
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
      backoff = ~ 2^.x,
      is_transient = \(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529)
      }
    ) |>
    httr2::req_timeout(300)
}

# System prompt for healthcare content analysis
ANALYZER_SYSTEM_PROMPT <- "You are an expert podcast content analyst specializing in healthcare policy. You are analyzing transcripts from 'Your Province. Your Premier.' - a podcast featuring Alberta Premier Danielle Smith discussing provincial issues.

## Your Task
Analyze the provided transcript and identify all content related to:
- Physicians and doctors
- Healthcare system and health policy particularly if it references physicians or doctors
- Hospitals, clinics, and healthcare facilities
- Alberta Health Services (AHS)
- Covenant Health
- Chartered Surgical Facilities
- Medical services, access, and wait times particularly if it references physicians or doctors
- Healthcare funding and reform particularly if it references physicians or doctors
- Emergency services and ambulance
- Primary care and family medicine

Ignore content related to federal benefits.

## Analysis Requirements
For each healthcare-related segment you identify:
1. Provide the timestamp range [MM:SS - MM:SS]
2. Write a concise summary (1-2 sentences) that avoids political spin
3. Extract a notable direct quote from Premier Danielle Smith if available (use exact words from transcript)
4. Provide the exact timestamp when the quote begins (quote_timestamp)
5. Assign a relevance score: High, Medium, or Low
   - High: Direct policy announcements, specific healthcare initiatives, detailed discussion
   - Medium: General healthcare mentions, context for healthcare topics
   - Low: Brief or tangential healthcare references
6. List the specific healthcare topics covered

## Output Format
You must output valid JSON with this exact structure:
```json
{
  \"overall_summary\": \"2-3 brief sentence summary of healthcare content in this episode that avoids political spin\",
  \"healthcare_focus_score\": 75,
  \"total_healthcare_minutes\": 12.5,
  \"highlights\": [
    {
      \"timestamp_start\": \"05:30\",
      \"timestamp_end\": \"08:45\",
      \"quote_timestamp\": \"06:15\",
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
- quote_timestamp must be the EXACT timestamp when the quote begins in the transcript (used for audio clips)
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
  } else if (
    is.list(transcript_data) && !is.null(transcript_data$transcript_markdown)
  ) {
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
  analysis <- tryCatch(
    {
      jsonlite::fromJSON(clean_json, simplifyVector = FALSE)
    },
    error = function(e) {
      log_msg("WARN", "Failed to parse analysis JSON: {e$message}")
      # Try to extract JSON object from text as fallback
      json_match <- regmatches(
        clean_json,
        regexpr("\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}", clean_json, perl = TRUE)
      )
      if (length(json_match) > 0 && nchar(json_match[1]) > 10) {
        tryCatch(
          {
            jsonlite::fromJSON(json_match[1], simplifyVector = FALSE)
          },
          error = function(e2) {
            list(
              overall_summary = "Failed to parse analysis",
              healthcare_focus_score = 0,
              total_healthcare_minutes = 0,
              highlights = list(),
              raw_response = response_text
            )
          }
        )
      } else {
        list(
          overall_summary = "Failed to parse analysis",
          healthcare_focus_score = 0,
          total_healthcare_minutes = 0,
          highlights = list(),
          raw_response = response_text
        )
      }
    }
  )

  n_highlights <- length(analysis$highlights)
  log_msg(
    "INFO",
    "Analysis complete: {n_highlights} healthcare highlights found"
  )

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

  # Look up accurate quote timestamps from AssemblyAI word data
  if (length(analysis$highlights) > 0) {
    raw_path <- file.path(episode_dir, "assemblyai_raw.json")
    if (file.exists(raw_path)) {
      raw_transcript <- safe_read_json(raw_path)

      for (i in seq_along(analysis$highlights)) {
        quote <- analysis$highlights[[i]]$quote
        if (!is.null(quote) && nchar(quote) > 20) {
          accurate_timestamp <- find_quote_timestamp(quote, raw_transcript)
          if (!is.null(accurate_timestamp)) {
            analysis$highlights[[i]]$quote_timestamp <- accurate_timestamp
            log_msg(
              "INFO",
              "Found accurate timestamp for quote {i}: {accurate_timestamp}"
            )
          } else {
            log_msg("WARN", "Could not find timestamp for quote {i}")
          }
        }
      }
    }
  }

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
