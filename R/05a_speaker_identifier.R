# Speaker Identifier for Premier Podcast Summary
# Uses Claude to identify callers in radio call-in show transcripts

# System prompt for caller identification
CALLER_ID_SYSTEM_PROMPT <- "You are analyzing a transcript excerpt from a radio call-in show called 'Your Province. Your Premier.' hosted by Wayne Nelson with guest Premier Danielle Smith.

Your task is to identify the caller's name from the context provided. Look for:
1. Host introduction: 'we have [NAME] on the line', '[NAME] is calling from...', 'Go ahead [NAME]'
2. Self-introduction: 'Hi, this is [NAME]', 'My name is [NAME]'
3. Being addressed by name during the conversation

Return ONLY valid JSON with this exact structure:
{
  \"caller_name\": \"FIRSTNAME\" or null,
  \"confidence\": \"high\" | \"medium\" | \"low\",
  \"source\": \"host_introduction\" | \"self_introduction\" | \"addressed_by_name\" | \"unknown\"
}

Rules:
- Use first name only, in ALL CAPS (e.g., \"JOHN\", \"TERESA\")
- Do not include city or location
- Return null for caller_name if the name cannot be determined
- Only output the JSON, no additional text"

#' Calculate speaking statistics per speaker
#' @param utterances List of utterances from AssemblyAI
#' @return tibble with speaker stats
calculate_speaker_stats <- function(utterances) {
  # Convert list to tibble
  utterance_df <- purrr::map_dfr(utterances, function(u) {
    tibble::tibble(
      speaker = u$speaker %||% "unknown",
      start_ms = u$start %||% 0,
      end_ms = u$end %||% 0,
      text = u$text %||% ""
    )
  })

  # Calculate stats per speaker
  utterance_df |>
    dplyr::group_by(speaker) |>
    dplyr::summarise(
      total_ms = sum(end_ms - start_ms),
      utterance_count = dplyr::n(),
      first_appearance_ms = min(start_ms),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(utterance_count))
}

#' Identify primary speakers (Wayne Nelson and Premier Danielle Smith)
#' @param speaker_stats Output from calculate_speaker_stats
#' @param utterances List of utterances from AssemblyAI
#' @return Named list with wayne_code and danielle_code
identify_primary_speakers <- function(speaker_stats, utterances) {
  # Convert utterances to searchable format
  utterance_df <- purrr::map_dfr(utterances, function(u) {
    tibble::tibble(
      speaker = u$speaker %||% "unknown",
      text = tolower(u$text %||% "")
    )
  })

  # Check for characteristic phrases per speaker
  speaker_phrases <- utterance_df |>
    dplyr::group_by(speaker) |>
    dplyr::summarise(
      mentions_premier = sum(grepl("premier|danielle", text)),
      says_welcome = sum(grepl("welcome to|your province|go ahead|calling from|on the line", text)),
      says_thank_you_wayne = sum(grepl("thank you wayne|thanks wayne|hi wayne", text)),
      total_text = paste(text, collapse = " "),
      .groups = "drop"
    )

  # Wayne: Most utterances AND uses host phrases
  # Danielle: Often mentioned as "Premier" and thanks Wayne
  wayne_candidates <- speaker_stats |>
    dplyr::left_join(speaker_phrases, by = "speaker") |>
    dplyr::mutate(
      wayne_score = utterance_count * 2 + says_welcome * 10,
      danielle_score = total_ms / 1000 + says_thank_you_wayne * 20 + mentions_premier * 5
    )

  # Wayne has most utterances (talks frequently but briefly)
  wayne_code <- wayne_candidates |>
    dplyr::arrange(dplyr::desc(wayne_score)) |>
    dplyr::slice(1) |>
    dplyr::pull(speaker)

  # Danielle has most speaking time among remaining speakers
  danielle_code <- wayne_candidates |>
    dplyr::filter(speaker != wayne_code) |>
    dplyr::arrange(dplyr::desc(danielle_score)) |>
    dplyr::slice(1) |>
    dplyr::pull(speaker)

  list(
    wayne_code = wayne_code,
    danielle_code = danielle_code
  )
}

#' Extract context around a speaker's first appearance
#' @param utterances List of utterances from AssemblyAI
#' @param speaker_code The speaker code to get context for
#' @param wayne_code Wayne's speaker code (for intro context)
#' @param context_ms Milliseconds of context to include (default 30000 = 30s)
#' @return Character string with formatted context
extract_caller_context <- function(utterances, speaker_code, wayne_code, context_ms = 30000) {
  # Convert to tibble for easier manipulation
  utterance_df <- purrr::map_dfr(seq_along(utterances), function(i) {
    u <- utterances[[i]]
    tibble::tibble(
      idx = i,
      speaker = u$speaker %||% "unknown",
      start_ms = u$start %||% 0,
      end_ms = u$end %||% 0,
      text = u$text %||% ""
    )
  })

  # Find first appearance of this speaker
 first_appearance <- utterance_df |>
    dplyr::filter(speaker == speaker_code) |>
    dplyr::slice(1)

  if (nrow(first_appearance) == 0) {
    return(NULL)
  }

  first_idx <- first_appearance$idx
  first_time <- first_appearance$start_ms

  # Get utterances around this time (before and after)
  context_start <- max(0, first_time - context_ms)
  context_end <- first_time + context_ms

  context_utterances <- utterance_df |>
    dplyr::filter(
      start_ms >= context_start & start_ms <= context_end
    ) |>
    dplyr::arrange(start_ms)

  # Format as readable transcript
  lines <- purrr::map_chr(seq_len(nrow(context_utterances)), function(i) {
    u <- context_utterances[i, ]
    speaker_label <- dplyr::case_when(
      u$speaker == wayne_code ~ "WAYNE",
      u$speaker == speaker_code ~ "CALLER",
      TRUE ~ paste0("SPEAKER_", u$speaker)
    )
    sprintf("[%s] %s: %s", ms_to_timestamp(u$start_ms), speaker_label, u$text)
  })

  paste(lines, collapse = "\n")
}

#' Identify a single caller using Claude
#' @param context Formatted transcript context
#' @return List with caller_name, confidence, source
identify_single_caller <- function(context) {
  if (is.null(context) || nchar(context) < 20) {
    return(list(caller_name = NULL, confidence = "low", source = "unknown"))
  }

  user_message <- sprintf(
    "Identify the caller's name from this transcript excerpt:\n\n%s",
    context
  )

  body <- list(
    model = CONFIG$anthropic_model,
    max_tokens = 256,
    system = CALLER_ID_SYSTEM_PROMPT,
    messages = list(
      list(role = "user", content = user_message)
    )
  )

  response <- tryCatch({
    claude_request() |>
      httr2::req_body_json(body) |>
      httr2::req_perform()
  }, error = function(e) {
    log_msg("WARN", "Claude API call failed for caller identification: {e$message}")
    return(NULL)
  })

  if (is.null(response)) {
    return(list(caller_name = NULL, confidence = "low", source = "unknown"))
  }

  result <- httr2::resp_body_json(response)
  response_text <- result$content[[1]]$text

  # Clean up response - remove markdown code fences if present
  clean_json <- response_text
  if (grepl("^\\s*```", clean_json)) {
    clean_json <- sub("^\\s*```(json)?\\s*\n?", "", clean_json)
    clean_json <- sub("\\s*```\\s*$", "", clean_json)
  }
  clean_json <- trimws(clean_json)

  # Parse JSON
  parsed <- tryCatch({
    jsonlite::fromJSON(clean_json, simplifyVector = FALSE)
  }, error = function(e) {
    log_msg("WARN", "Failed to parse caller ID response: {e$message}")
    list(caller_name = NULL, confidence = "low", source = "unknown")
  })

  # Ensure caller_name is uppercase if present
  if (!is.null(parsed$caller_name) && nchar(parsed$caller_name) > 0) {
    parsed$caller_name <- toupper(parsed$caller_name)
  }

  parsed
}

#' Identify all callers in transcript
#' @param utterances List of utterances from AssemblyAI
#' @param caller_codes Vector of speaker codes that are callers
#' @param wayne_code Wayne's speaker code
#' @return Named list mapping speaker codes to names
identify_callers <- function(utterances, caller_codes, wayne_code) {
  caller_mapping <- list()

  for (code in caller_codes) {
    log_msg("INFO", "Identifying caller for speaker code: {code}")

    context <- extract_caller_context(utterances, code, wayne_code)
    result <- identify_single_caller(context)

    if (!is.null(result$caller_name) && nchar(result$caller_name) > 0) {
      caller_mapping[[code]] <- result$caller_name
      log_msg("INFO", "Identified speaker {code} as: {result$caller_name} (confidence: {result$confidence})")
    } else {
      caller_mapping[[code]] <- "CALLER"
      log_msg("INFO", "Could not identify speaker {code}, using CALLER")
    }
  }

  caller_mapping
}

#' Main function to identify all speakers in transcript
#' @param raw_transcript Raw AssemblyAI response
#' @return List with mapping and metadata
identify_speakers <- function(raw_transcript) {
  utterances <- raw_transcript$utterances

  if (is.null(utterances) || length(utterances) == 0) {
    log_msg("WARN", "No utterances found in transcript")
    return(list(
      mapping = list(
        A = "WAYNE NELSON",
        B = "PREMIER DANIELLE SMITH"
      ),
      identified_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    ))
  }

  # Calculate speaker statistics
  speaker_stats <- calculate_speaker_stats(utterances)
  log_msg("INFO", "Found {nrow(speaker_stats)} unique speakers")

  # Identify primary speakers (Wayne and Danielle)
  primary <- identify_primary_speakers(speaker_stats, utterances)
  log_msg("INFO", "Identified Wayne as speaker {primary$wayne_code}")
  log_msg("INFO", "Identified Danielle as speaker {primary$danielle_code}")

  # Start building the mapping
  mapping <- list()
  mapping[[primary$wayne_code]] <- "WAYNE NELSON"
  mapping[[primary$danielle_code]] <- "PREMIER DANIELLE SMITH"

  # Identify remaining speakers as callers
  all_codes <- speaker_stats$speaker
  caller_codes <- setdiff(all_codes, c(primary$wayne_code, primary$danielle_code))

  if (length(caller_codes) > 0) {
    log_msg("INFO", "Identifying {length(caller_codes)} potential caller(s)")
    caller_mapping <- identify_callers(utterances, caller_codes, primary$wayne_code)

    # Merge caller mapping
    for (code in names(caller_mapping)) {
      mapping[[code]] <- caller_mapping[[code]]
    }
  }

  list(
    mapping = mapping,
    speaker_stats = as.list(speaker_stats),
    primary_speakers = primary,
    identified_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
}

#' Save speaker mapping to file
#' @param mapping Speaker mapping result from identify_speakers
#' @param episode_dir Episode directory
#' @return Path to saved file
save_speaker_mapping <- function(mapping, episode_dir) {
  json_path <- file.path(episode_dir, "speaker_mapping.json")
  safe_write_json(mapping, json_path)
  json_path
}

#' Load speaker mapping from file
#' @param episode_dir Episode directory
#' @return Speaker mapping list or NULL
load_speaker_mapping <- function(episode_dir) {
  json_path <- file.path(episode_dir, "speaker_mapping.json")
  if (file.exists(json_path)) {
    data <- safe_read_json(json_path)
    # Return just the mapping portion for use in formatting
    data$mapping
  } else {
    NULL
  }
}
