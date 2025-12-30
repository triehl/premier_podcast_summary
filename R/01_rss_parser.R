# RSS Feed Parser for Premier Podcast Summary
# Parses the podcast RSS feed and extracts episode metadata

#' Parse RSS feed and extract episode metadata
#' @param rss_url URL of the RSS feed
#' @return tibble with episode metadata
parse_rss_feed <- function(rss_url = CONFIG$rss_url) {
log_msg("INFO", "Fetching RSS feed from {rss_url}")

# Fetch and parse XML
response <- httr2::request(rss_url) |>
  httr2::req_perform()

xml_content <- httr2::resp_body_xml(response)

# Extract all item nodes
items <- xml2::xml_find_all(xml_content, "//item")
log_msg("INFO", "Found {length(items)} episodes in feed")

# Parse each item into a tibble row
episodes <- purrr::map_dfr(items, parse_episode_item)

# Sort by publication date (newest first)
episodes <- episodes |>
  dplyr::arrange(dplyr::desc(pub_date))

log_msg("INFO", "Parsed {nrow(episodes)} episodes")
episodes
}

#' Parse a single episode item from RSS XML
#' @param item XML node for an episode
#' @return tibble row with episode data
parse_episode_item <- function(item) {
# Extract fields with fallbacks
guid <- xml2::xml_text(xml2::xml_find_first(item, ".//guid"))
title <- xml2::xml_text(xml2::xml_find_first(item, ".//title"))
pub_date_raw <- xml2::xml_text(xml2::xml_find_first(item, ".//pubDate"))

# Parse publication date
pub_date <- tryCatch({
  as.POSIXct(pub_date_raw, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC")
}, error = function(e) {
  # Try alternative format
  as.POSIXct(pub_date_raw, tz = "UTC")
})

# Get MP3 URL from enclosure
enclosure <- xml2::xml_find_first(item, ".//enclosure")
mp3_url <- if (!is.na(enclosure)) {
  xml2::xml_attr(enclosure, "url")
} else {
  NA_character_
}

# Get duration (may be in seconds or HH:MM:SS format)
duration_raw <- xml2::xml_text(xml2::xml_find_first(item, ".//itunes:duration"))
duration_seconds <- parse_duration(duration_raw)

# Get description
description <- xml2::xml_text(xml2::xml_find_first(item, ".//description"))
if (is.na(description)) {
  description <- xml2::xml_text(xml2::xml_find_first(item, ".//itunes:summary"))
}

# Create slug for this episode
slug <- create_episode_slug(title, pub_date)

tibble::tibble(
  guid = guid,
  title = title,
  pub_date = pub_date,
  mp3_url = mp3_url,
  duration_seconds = duration_seconds,
  description = description %||% "",
  slug = slug
)
}

#' Parse duration string to seconds
#' @param duration_raw Raw duration string (seconds or HH:MM:SS)
#' @return Numeric duration in seconds
parse_duration <- function(duration_raw) {
if (is.na(duration_raw) || duration_raw == "") {
  return(NA_real_)
}

# Check if it's already in seconds (just a number)
if (grepl("^\\d+$", duration_raw)) {
  return(as.numeric(duration_raw))
}

# Parse HH:MM:SS or MM:SS format
parts <- strsplit(duration_raw, ":")[[1]]
parts <- as.numeric(parts)

if (length(parts) == 3) {
  parts[1] * 3600 + parts[2] * 60 + parts[3]
} else if (length(parts) == 2) {
  parts[1] * 60 + parts[2]
} else {
  NA_real_
}
}

#' Extract channel/podcast metadata from RSS feed
#' @param rss_url URL of the RSS feed
#' @return List with channel info
get_channel_info <- function(rss_url = CONFIG$rss_url) {
log_msg("INFO", "Fetching channel info from {rss_url}")

response <- httr2::request(rss_url) |>
  httr2::req_perform()

xml_content <- httr2::resp_body_xml(response)
channel <- xml2::xml_find_first(xml_content, "//channel")

list(
  title = xml2::xml_text(xml2::xml_find_first(channel, "./title")),
  description = xml2::xml_text(xml2::xml_find_first(channel, "./description")),
  link = xml2::xml_text(xml2::xml_find_first(channel, "./link")),
  author = xml2::xml_text(xml2::xml_find_first(channel, ".//itunes:author")),
  image_url = xml2::xml_attr(xml2::xml_find_first(channel, ".//itunes:image"), "href"),
  language = xml2::xml_text(xml2::xml_find_first(channel, "./language")),
  copyright = xml2::xml_text(xml2::xml_find_first(channel, "./copyright"))
)
}

#' Get the N most recent episodes
#' @param episodes tibble of episodes
#' @param n Number of episodes to return
#' @return tibble with n most recent episodes
get_latest_episodes <- function(episodes, n = CONFIG$max_episodes) {
episodes |>
  dplyr::arrange(dplyr::desc(pub_date)) |>
  dplyr::slice_head(n = n)
}
