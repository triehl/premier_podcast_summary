# Utility functions for Premier Podcast Summary

#' Safely get environment variable
#' @param name Name of the environment variable
#' @param required If TRUE, throws error when variable is missing
#' @return Character value of the environment variable
get_env_var <- function(name, required = TRUE) {
value <- Sys.getenv(name)
if (required && (is.null(value) || value == "")) {
  stop(sprintf("Required environment variable '%s' is not set. Check your .Renviron file.", name))
}
value
}

#' Create directory if it doesn't exist
#' @param path Path to directory
#' @return Invisible path
ensure_dir <- function(path) {
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
  log_msg("INFO", "Created directory: {path}")
}
invisible(path)
}

#' Convert milliseconds to timestamp string
#' @param ms Milliseconds
#' @return Character timestamp in format MM:SS or HH:MM:SS
ms_to_timestamp <- function(ms) {
seconds_to_timestamp(ms / 1000)
}

#' Convert seconds to timestamp string
#' @param seconds Numeric seconds
#' @return Character timestamp in format MM:SS or HH:MM:SS
seconds_to_timestamp <- function(seconds) {
seconds <- as.integer(seconds)
hours <- seconds %/% 3600
minutes <- (seconds %% 3600) %/% 60
secs <- seconds %% 60

if (hours > 0) {
  sprintf("%02d:%02d:%02d", hours, minutes, secs)
} else {
  sprintf("%02d:%02d", minutes, secs)
}
}

#' Convert timestamp string to seconds
#' @param timestamp Character timestamp (MM:SS or HH:MM:SS)
#' @return Numeric seconds
timestamp_to_seconds <- function(timestamp) {
parts <- as.integer(strsplit(timestamp, ":")[[1]])
if (length(parts) == 2) {
  parts[1] * 60 + parts[2]
} else if (length(parts) == 3) {
  parts[1] * 3600 + parts[2] * 60 + parts[3]
} else {
  stop("Invalid timestamp format: ", timestamp)
}
}

#' Sanitize string for use as filename
#' @param string Input string
#' @return Sanitized string safe for filenames
sanitize_filename <- function(string) {
# Replace spaces with hyphens
result <- gsub("\\s+", "-", string)
# Remove special characters except hyphens and underscores
result <- gsub("[^a-zA-Z0-9_-]", "", result)
# Convert to lowercase
result <- tolower(result)
# Remove multiple consecutive hyphens
result <- gsub("-+", "-", result)
# Trim hyphens from start/end
result <- gsub("^-|-$", "", result)
result
}

#' Create episode slug from title and date
#' @param title Episode title
#' @param pub_date Publication date
#' @return Character slug for URLs and filenames
create_episode_slug <- function(title, pub_date) {
# Format date as YYYY-MM-DD
date_str <- format(as.Date(pub_date), "%Y-%m-%d")
# Sanitize title
title_slug <- sanitize_filename(title)
# Combine (limit title portion length)
if (nchar(title_slug) > 50) {
  title_slug <- substr(title_slug, 1, 50)
  title_slug <- gsub("-$", "", title_slug) # Remove trailing hyphen if cut mid-word
}
paste0(date_str, "-", title_slug)
}

#' Log message with timestamp
#' @param level Log level (INFO, WARN, ERROR, DEBUG)
#' @param message Message with optional glue-style interpolation
#' @param ... Additional arguments for glue interpolation
log_msg <- function(level, message, ...) {
# Simple logging - can be replaced with logger package if needed
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Handle glue-style interpolation
env <- parent.frame()
formatted_msg <- tryCatch({
  glue::glue(message, .envir = env)
}, error = function(e) {
  message
})

cat(sprintf("[%s] %s: %s\n", timestamp, level, formatted_msg))
}

#' Format duration for display
#' @param seconds Duration in seconds
#' @return Human-readable duration string
format_duration <- function(seconds) {
seconds <- as.integer(seconds)
hours <- seconds %/% 3600
minutes <- (seconds %% 3600) %/% 60
secs <- seconds %% 60

if (hours > 0) {
  sprintf("%d hour%s %d minute%s",
          hours, if(hours != 1) "s" else "",
          minutes, if(minutes != 1) "s" else "")
} else if (minutes > 0) {
  sprintf("%d minute%s", minutes, if(minutes != 1) "s" else "")
} else {
  sprintf("%d second%s", secs, if(secs != 1) "s" else "")
}
}

#' Get project root directory
#' @return Path to project root
get_project_root <- function() {
# Try to find project root by looking for _quarto.yml or run.R
candidates <- c(
  getwd(),
  here::here() # if here package is available
)

for (path in candidates) {
  if (file.exists(file.path(path, "_quarto.yml")) ||
      file.exists(file.path(path, "run.R"))) {
    return(path)
  }
}

# Fallback to working directory
getwd()
}

#' Safely read JSON file
#' @param path Path to JSON file
#' @return Parsed JSON as list, or NULL if file doesn't exist
safe_read_json <- function(path) {
if (!file.exists(path)) {
  return(NULL)
}
tryCatch({
  jsonlite::read_json(path)
}, error = function(e) {
  log_msg("WARN", "Failed to read JSON from {path}: {e$message}")
  NULL
})
}

#' Safely write JSON file
#' @param data Data to write
#' @param path Path to JSON file
#' @param pretty Use pretty formatting
safe_write_json <- function(data, path, pretty = TRUE) {
tryCatch({
  ensure_dir(dirname(path))
  jsonlite::write_json(data, path, pretty = pretty, auto_unbox = TRUE)
  log_msg("INFO", "Wrote JSON to {path}")
  TRUE
}, error = function(e) {
  log_msg("ERROR", "Failed to write JSON to {path}: {e$message}")
  FALSE
})
}

#' Retry a function with exponential backoff
#' @param fn Function to retry
#' @param max_attempts Maximum number of attempts
#' @param initial_delay Initial delay in seconds
#' @param ... Arguments to pass to fn
retry_with_backoff <- function(fn, max_attempts = 3, initial_delay = 1, ...) {
attempt <- 1
delay <- initial_delay

while (attempt <= max_attempts) {
  result <- tryCatch({
    fn(...)
  }, error = function(e) {
    if (attempt < max_attempts) {
      log_msg("WARN", "Attempt {attempt} failed: {e$message}. Retrying in {delay}s...")
      Sys.sleep(delay)
      delay <<- delay * 2
      NULL
    } else {
      stop(e)
    }
  })

  if (!is.null(result)) {
    return(result)
  }
  attempt <- attempt + 1
}
}
