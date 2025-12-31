# Local Transcript Formatter for Premier Podcast Summary
# Formats raw AssemblyAI transcription into clean, readable Markdown
# Uses Claude for intelligent section/topic detection

# System prompt for section detection
SECTION_DETECTION_PROMPT <- "You are analyzing a transcript from a radio call-in show 'Your Province. Your Premier.' with host Wayne Nelson and Premier Danielle Smith. Callers phone in to ask questions.

Identify distinct sections where new topics or callers begin. Create a new section (topic title) when:
1. The host introduces a new caller 
2. The host reads a question received by email or text message
3. A significantly different question or topic is raised
4. The conversation shifts to a new subject
5. The host asks a concluding question at the end of the show

IMPORTANT: The start_time should be the timestamp when the HOST (Wayne Nelson) INTRODUCES the question or caller - NOT when the caller starts speaking.

For each section provide a concise topic title (2-5 words) that describes the discussion.
Topics should be specific, e.g.: 'Healthcare Wait Times', 'Pension Reform Question', 'Federal Carbon Tax', 'AISH Benefits Inquiry', 'Rural Policing Concerns'

IMPORTANT: The new section (topic title) should appear BEFORE the question is asked. For example if the host says 'Lorraine is phoning in from Calgary with a victims of crime question. Go ahead, Lorraine.' the new section (topic title) should be before this question.

CALLER: Hi, Premier. Thank you for all your work you’re doing for Alberta and the Albertans.', the new section header should be before this question.


Return ONLY a JSON array, no other text or markdown:
[
  {\"start_time\": \"00:00\", \"title\": \"Opening\"},
  {\"start_time\": \"MM:SS\", \"title\": \"Topic Title\"}
]"

# Configuration for transcript formatting
TRANSCRIPT_CONFIG <- list(
  # Default label for unidentified callers (speaker mapping loaded dynamically)
  default_caller_label = "CALLER",

  # Ad detection patterns (lowercase matching)
  ad_patterns = c(
    "blue j",
    "blue jay",
    "tax research",
    "tax experts",
    "best use of my time",
    "busy season",
    "barn out"
  ),

  # Show intro patterns to detect start of actual content
  show_intro_patterns = c(
    "welcome to your province",
    "your province, your premier",
    "i'm wayne nelson"
  ),

  # Maximum time (ms) to scan for ads at beginning
  max_ad_scan_ms = 60000, # 60 seconds

  # End-of-show patterns to detect closing ads/promos
  end_patterns = c(
    "doc returns",
    "stream on stat",
    "global stream",
    "coming up next",
    "tune in next",
    "my mind is trying to tell me",
    "gripping new cases"
  ),

  # Patterns that indicate actual show content has ended (within utterance)
  show_end_markers = c(
    "doc returns",
    "returns this january",
    "on global"
  )
)

#' Map speaker code to full name
#' @param speaker_code Speaker code from AssemblyAI (A, B, C, etc.)
#' @param speaker_mapping Optional named list mapping codes to names (from speaker_mapping.json)
#' @return Full speaker name
map_speaker_name <- function(speaker_code, speaker_mapping = NULL) {
  # Use dynamic mapping if provided
  if (!is.null(speaker_mapping) && !is.null(speaker_mapping[[speaker_code]])) {
    return(speaker_mapping[[speaker_code]])
  }
  # Fallback to default caller label
  TRANSCRIPT_CONFIG$default_caller_label
}

#' Check if text appears to be advertisement content
#' @param text Text to check
#' @return TRUE if text matches ad patterns
is_advertisement <- function(text) {
  text_lower <- tolower(text)
  any(sapply(TRANSCRIPT_CONFIG$ad_patterns, function(p) {
    grepl(p, text_lower, fixed = TRUE)
  }))
}

#' Check if text indicates start of actual show content
#' @param text Text to check
#' @return TRUE if text matches show intro patterns
is_show_intro <- function(text) {
  text_lower <- tolower(text)
  any(sapply(TRANSCRIPT_CONFIG$show_intro_patterns, function(p) {
    grepl(p, text_lower, fixed = TRUE)
  }))
}

#' Check if text appears to be end-of-show promotional content
#' @param text Text to check
#' @return TRUE if text matches end patterns
is_end_promo <- function(text) {
  text_lower <- tolower(text)
  any(sapply(TRANSCRIPT_CONFIG$end_patterns, function(p) {
    grepl(p, text_lower, fixed = TRUE)
  }))
}

#' Find the index where actual show content begins
#' @param utterances tibble of utterances
#' @return Index of first non-ad utterance
find_content_start <- function(utterances) {
  if (nrow(utterances) == 0) {
    return(1)
  }

  for (i in seq_len(nrow(utterances))) {
    u <- utterances[i, ]

    # Stop scanning after max time
    if (u$start_ms > TRANSCRIPT_CONFIG$max_ad_scan_ms) {
      return(1) # No clear ad section found, start from beginning
    }

    # Check for show intro
    if (is_show_intro(u$text)) {
      return(i)
    }

    # Check if this looks like non-ad content after initial period
    if (i > 1 && !is_advertisement(u$text) && u$start_ms > 30000) {
      # If previous was ad and this isn't, might be the transition
      if (is_advertisement(utterances$text[i - 1])) {
        return(i)
      }
    }
  }

  # Default: start from first utterance
  1
}

#' Extract show intro from utterance that contains both ad and intro
#' @param text Text that may contain ad + intro
#' @return Text with ad portion removed, or original text if no ad found
trim_ad_from_intro <- function(text) {
  text_lower <- tolower(text)

  # Try to find "welcome to your province" which is the clear show start
  welcome_pattern <- "welcome to your province"
  match_pos <- regexpr(welcome_pattern, text_lower, fixed = TRUE)

  if (match_pos > 1) {
    # Start from "welcome to your province"
    # Capitalize first letter properly
    result <- substr(text, match_pos, nchar(text))
    return(paste0(
      toupper(substr(result, 1, 1)),
      substr(result, 2, nchar(result))
    ))
  }

  # Fallback: look for other intro patterns
  for (pattern in TRANSCRIPT_CONFIG$show_intro_patterns) {
    match_pos <- regexpr(pattern, text_lower, fixed = TRUE)
    if (match_pos > 1) {
      result <- trimws(substr(text, match_pos, nchar(text)))
      return(paste0(
        toupper(substr(result, 1, 1)),
        substr(result, 2, nchar(result))
      ))
    }
  }

  text
}

#' Merge consecutive utterances from the same speaker
#' @param utterances tibble of utterances
#' @return tibble with merged utterances
merge_consecutive_speakers <- function(utterances) {
  if (nrow(utterances) == 0) {
    return(utterances)
  }

  merged <- list()
  current <- list(
    speaker = utterances$speaker[1],
    text = utterances$text[1],
    start_ms = utterances$start_ms[1],
    end_ms = utterances$end_ms[1]
  )

  for (i in seq_len(nrow(utterances))[-1]) {
    u <- utterances[i, ]

    if (u$speaker == current$speaker) {
      # Same speaker - merge text
      current$text <- paste(current$text, u$text)
      current$end_ms <- u$end_ms
    } else {
      # Different speaker - save current and start new
      merged <- append(merged, list(current))
      current <- list(
        speaker = u$speaker,
        text = u$text,
        start_ms = u$start_ms,
        end_ms = u$end_ms
      )
    }
  }

  # Don't forget the last one
  merged <- append(merged, list(current))

  # Convert back to tibble
  purrr::map_dfr(merged, tibble::as_tibble)
}

#' Generate a section title based on content
#' @param text Text content to analyze
#' @param is_opening TRUE if this is the opening section
#' @return Section title string
generate_section_title <- function(text, is_opening = FALSE) {
  if (is_opening) {
    return("Opening")
  }

  # Look for topic keywords
  text_lower <- tolower(text)

  if (grepl("pension|cpp|retirement", text_lower)) {
    return("Pension Discussion")
  }
  if (grepl("police|rcmp|sheriff", text_lower)) {
    return("Policing")
  }
  if (grepl("school|education|teacher|charter|private school", text_lower)) {
    return("Education")
  }
  if (grepl("health|doctor|hospital|ahs|medical", text_lower)) {
    return("Healthcare")
  }
  if (grepl("aish|disability|benefit", text_lower)) {
    return("Disability Benefits")
  }
  if (grepl("tax|budget|deficit|spending", text_lower)) {
    return("Fiscal Policy")
  }
  if (grepl("ottawa|federal|trudeau|constitution", text_lower)) {
    return("Federal Relations")
  }
  if (grepl("oil|gas|energy|pipeline", text_lower)) {
    return("Energy Sector")
  }
  if (grepl("alberta next|panel|referendum", text_lower)) {
    return("Alberta Next Panel")
  }
  if (grepl("recall|petition|initiative", text_lower)) {
    return("Citizen Initiatives")
  }
  if (grepl("crime|victim|justice", text_lower)) {
    return("Justice")
  }
  if (grepl("youth|trades|employment|job", text_lower)) {
    return("Employment")
  }
  if (grepl("immigration|newcomer", text_lower)) {
    return("Immigration")
  }

  # Default
  "Discussion"
}

#' Parse sections JSON from Claude response
#' @param json_text Raw JSON text from Claude
#' @return List of sections with start_ms and title
parse_sections_json <- function(json_text) {
  # Clean up response - remove markdown code fences if present
  clean_json <- json_text
  if (grepl("^\\s*```", clean_json)) {
    clean_json <- sub("^\\s*```(json)?\\s*\n?", "", clean_json)
    clean_json <- sub("\\s*```\\s*$", "", clean_json)
  }
  clean_json <- trimws(clean_json)

  # Parse JSON
  sections_raw <- tryCatch(
    {
      jsonlite::fromJSON(clean_json, simplifyVector = FALSE)
    },
    error = function(e) {
      log_msg("WARN", "Failed to parse sections JSON: {e$message}")
      return(list(list(start_time = "00:00", title = "Opening")))
    }
  )

  # Convert timestamps to milliseconds
  sections <- purrr::map(sections_raw, function(s) {
    # Parse MM:SS or HH:MM:SS timestamp
    time_parts <- as.integer(strsplit(s$start_time, ":")[[1]])
    if (length(time_parts) == 2) {
      start_ms <- (time_parts[1] * 60 + time_parts[2]) * 1000
    } else if (length(time_parts) == 3) {
      start_ms <- (time_parts[1] * 3600 + time_parts[2] * 60 + time_parts[3]) *
        1000
    } else {
      start_ms <- 0
    }

    list(
      start_ms = start_ms,
      title = s$title %||% "Discussion"
    )
  })

  # Sort by start_ms
  sections <- sections[order(sapply(sections, function(s) s$start_ms))]

  sections
}

#' Detect transcript sections using Claude
#' @param raw_transcript Raw AssemblyAI response
#' @param speaker_mapping Speaker mapping from identify_speakers
#' @return List of sections with start_ms and title
detect_transcript_sections <- function(raw_transcript, speaker_mapping) {
  # Build readable transcript for Claude
  utterances <- extract_utterances(raw_transcript)

  if (nrow(utterances) == 0) {
    return(list(list(start_ms = 0, title = "Opening")))
  }

  # Format transcript with timestamps and speaker names for Claude
  transcript_lines <- purrr::map_chr(seq_len(nrow(utterances)), function(i) {
    u <- utterances[i, ]
    speaker <- map_speaker_name(u$speaker, speaker_mapping)
    timestamp <- ms_to_timestamp(u$start_ms)
    sprintf("[%s] %s: %s", timestamp, speaker, u$text)
  })
  transcript_text <- paste(transcript_lines, collapse = "\n")

  log_msg("INFO", "Sending transcript to Claude for section detection")

  # Send to Claude for section detection
  body <- list(
    model = CONFIG$anthropic_model,
    max_tokens = 4096,
    system = SECTION_DETECTION_PROMPT,
    messages = list(
      list(role = "user", content = transcript_text)
    )
  )

  response <- tryCatch(
    {
      claude_request() |>
        httr2::req_body_json(body) |>
        httr2::req_perform()
    },
    error = function(e) {
      log_msg("WARN", "Claude section detection failed: {e$message}")
      return(NULL)
    }
  )

  if (is.null(response)) {
    # Fallback to single "Opening" section
    return(list(list(start_ms = 0, title = "Opening")))
  }

  # Parse response
  result <- httr2::resp_body_json(response)
  sections_json <- result$content[[1]]$text

  sections <- parse_sections_json(sections_json)

  log_msg("INFO", "Detected {length(sections)} sections in transcript")

  sections
}

#' Format raw transcript into clean Markdown
#' @param raw_transcript Raw AssemblyAI response
#' @param episode_metadata Episode metadata (title, pub_date, etc.)
#' @param episode_dir Optional episode directory to load speaker mapping from
#' @return Formatted transcript as markdown string
format_transcript <- function(
  raw_transcript,
  episode_metadata,
  episode_dir = NULL
) {
  # Load speaker mapping if episode_dir provided
  speaker_mapping <- NULL
  if (!is.null(episode_dir)) {
    speaker_mapping <- load_speaker_mapping(episode_dir)
    if (!is.null(speaker_mapping)) {
      log_msg(
        "INFO",
        "Loaded speaker mapping with {length(speaker_mapping)} speakers"
      )
    }
  }

  # Extract utterances
  utterances <- extract_utterances(raw_transcript)

  if (nrow(utterances) == 0) {
    log_msg("WARN", "No utterances to format")
    return("")
  }

  log_msg(
    "INFO",
    "Formatting transcript locally ({nrow(utterances)} utterances)"
  )

  # Step 1: Find where actual content begins (skip ads)
  content_start_idx <- find_content_start(utterances)
  if (content_start_idx > 1) {
    log_msg(
      "INFO",
      "Skipping {content_start_idx - 1} ad utterances at beginning"
    )
    utterances <- utterances[content_start_idx:nrow(utterances), ]
  }

  # Step 1b: Clean up first utterance if it contains both ad and intro
  if (nrow(utterances) > 0) {
    first_text <- utterances$text[1]
    if (is_advertisement(first_text) && is_show_intro(first_text)) {
      cleaned_text <- trim_ad_from_intro(first_text)
      if (cleaned_text != first_text) {
        log_msg("INFO", "Trimmed ad content from first utterance")
        utterances$text[1] <- cleaned_text
      }
    }
  }

  # Step 1c: Remove end-of-show promotional content
  # Iteratively scan backward and remove promo content until we find actual show content
  if (nrow(utterances) > 0) {
    max_iterations <- 20 # Prevent infinite loops
    iteration <- 0

    while (nrow(utterances) > 0 && iteration < max_iterations) {
      iteration <- iteration + 1
      found_promo <- FALSE

      # Scan backward from the end looking for promo markers (check last 15 utterances)
      for (i in seq(nrow(utterances), max(1, nrow(utterances) - 15), -1)) {
        text <- utterances$text[i]
        text_lower <- tolower(text)

        # Look for show end markers
        for (marker in TRANSCRIPT_CONFIG$show_end_markers) {
          match_pos <- regexpr(marker, text_lower, fixed = TRUE)
          if (match_pos > 0) {
            # Found the marker
            orig_count <- nrow(utterances)

            if (match_pos == 1) {
              # Marker at the very start - remove this utterance
              utterances <- utterances[1:(i - 1), ]
              log_msg(
                "INFO",
                "Removed end promo utterance {i} (marker at start)"
              )
            } else {
              # Marker in the middle - trim this utterance at the marker
              new_text <- trimws(substr(text, 1, match_pos - 1))
              new_text <- sub("\\. $", ".", new_text)
              new_text <- sub(" $", "", new_text)

              if (nchar(new_text) > 50) {
                # Keep this utterance with trimmed text
                utterances$text[i] <- new_text
                utterances <- utterances[1:i, ]
                log_msg("INFO", "Trimmed end promo from utterance {i}")
                # This is a clean trim - we can stop
                found_promo <- FALSE
                break
              } else {
                # Text too short, remove this utterance
                utterances <- utterances[1:(i - 1), ]
                log_msg(
                  "INFO",
                  "Removed end promo utterance {i} (remaining text too short)"
                )
              }
            }
            found_promo <- TRUE
            break
          }
        }
        if (found_promo) break
      }

      # If no marker found in the scan, we're done
      if (!found_promo) break
    }
  }

  # Step 2: Merge consecutive same-speaker utterances
  utterances <- merge_consecutive_speakers(utterances)
  log_msg("INFO", "After merging: {nrow(utterances)} utterances")

  # Step 3: Detect sections using Claude
  sections <- detect_transcript_sections(raw_transcript, speaker_mapping)

  # Step 4: Build formatted output with section headers
  lines <- character()
  section_idx <- 1

  for (i in seq_len(nrow(utterances))) {
    u <- utterances[i, ]

    # Check if we've reached the next section boundary
    while (
      section_idx <= length(sections) &&
        u$start_ms >= sections[[section_idx]]$start_ms
    ) {
      section <- sections[[section_idx]]
      timestamp <- ms_to_timestamp(section$start_ms)
      lines <- c(lines, sprintf("\n## [%s] %s\n", timestamp, section$title))
      section_idx <- section_idx + 1
    }

    # Format the utterance
    speaker_name <- map_speaker_name(u$speaker, speaker_mapping)
    lines <- c(lines, sprintf("\n**%s:** %s\n", speaker_name, u$text))
  }

  formatted <- paste(lines, collapse = "")

  log_msg("INFO", "Formatted transcript: {nchar(formatted)} characters")
  formatted
}

#' Save formatted transcript
#' @param transcript_md Formatted markdown transcript
#' @param episode_dir Episode directory
#' @param episode_metadata Episode metadata
#' @return Path to saved file
save_formatted_transcript <- function(
  transcript_md,
  episode_dir,
  episode_metadata
) {
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
    formatter = "local", # Changed from "claude" to "local"
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
format_episode_transcript <- function(
  episode_dir,
  episode_metadata,
  force = FALSE
) {
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

  # Format locally (no Claude API call)
  # Pass episode_dir to load speaker mapping
  formatted_md <- format_transcript(
    raw_transcript,
    episode_metadata,
    episode_dir
  )

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
