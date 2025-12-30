# Quarto Page Generator for Premier Podcast Summary
# Generates Quarto documents from episode data

#' Generate episode analysis page (.qmd)
#' @param episode_metadata Episode metadata (from RSS)
#' @param analysis Analysis results
#' @param clips_manifest Clips manifest (optional)
#' @param output_dir Output directory for .qmd files
#' @return Path to generated file
generate_episode_page <- function(episode_metadata,
                                analysis,
                                clips_manifest = NULL,
                                output_dir = CONFIG$episodes_qmd_dir) {

slug <- episode_metadata$slug
output_path <- file.path(get_project_root(), output_dir, paste0(slug, ".qmd"))

# Format date for display
date_display <- format(episode_metadata$pub_date, "%B %d, %Y")
date_iso <- format(episode_metadata$pub_date, "%Y-%m-%d")

# Build YAML header
yaml_header <- sprintf('---
title: "%s"
date: %s
description: "Healthcare policy analysis from Premier Danielle Smith podcast"
categories: [healthcare, policy, Alberta]
---
', gsub('"', '\\"', episode_metadata$title), date_iso)

# Build content sections
content <- character()

# Overall summary section
content <- c(content, "## Episode Overview\n")
content <- c(content, sprintf("**Air Date:** %s\n\n", date_display))

if (!is.null(analysis$overall_summary) && nchar(analysis$overall_summary) > 0) {
  content <- c(content, sprintf("%s\n\n", analysis$overall_summary))
}

if (!is.null(analysis$healthcare_focus_score)) {
  content <- c(content, sprintf("**Healthcare Focus Score:** %d%%\n\n",
                                as.integer(analysis$healthcare_focus_score)))
}

# Healthcare highlights section
content <- c(content, "## Healthcare Highlights\n\n")

if (!is.null(analysis$highlights) && length(analysis$highlights) > 0) {
  # Group by relevance
  high_highlights <- purrr::keep(analysis$highlights, ~.x$relevance == "High")
  medium_highlights <- purrr::keep(analysis$highlights, ~.x$relevance == "Medium")

  if (length(high_highlights) > 0) {
    content <- c(content, "### Key Points\n\n")
    for (h in high_highlights) {
      content <- c(content, format_highlight_block(h, clips_manifest, episode_metadata))
    }
  }

  if (length(medium_highlights) > 0) {
    content <- c(content, "### Additional Mentions\n\n")
    for (h in medium_highlights) {
      content <- c(content, format_highlight_block(h, clips_manifest, episode_metadata))
    }
  }
} else {
  content <- c(content, "*No significant healthcare content identified in this episode.*\n\n")
}

# Transcript link
transcript_slug <- paste0(slug, "_transcript")
content <- c(content, sprintf("\n## Full Transcript\n\n[View the complete transcript](%s.qmd)\n",
                              transcript_slug))

# Duration info
if (!is.null(episode_metadata$duration_seconds) && !is.na(episode_metadata$duration_seconds)) {
  content <- c(content, sprintf("\n---\n\n*Episode Duration: %s*\n",
                                format_duration(episode_metadata$duration_seconds)))
}

# Write file
ensure_dir(dirname(output_path))
writeLines(c(yaml_header, paste(content, collapse = "")), output_path)
log_msg("INFO", "Generated episode page: {output_path}")

output_path
}

#' Format a single highlight block
#' @param h Highlight object
#' @param clips_manifest Clips manifest for audio links
#' @param episode_metadata Episode metadata
#' @return Formatted markdown string
format_highlight_block <- function(h, clips_manifest = NULL, episode_metadata = NULL) {
lines <- character()

# Timestamp and summary
lines <- c(lines, sprintf("**[%s - %s]** %s\n\n",
                          h$timestamp_start,
                          h$timestamp_end,
                          h$summary %||% ""))

# Quote if available
if (!is.null(h$quote) && nchar(h$quote) > 0) {
  lines <- c(lines, sprintf("> \"%s\"\n\n", h$quote))
}

# Topics
if (!is.null(h$topics) && length(h$topics) > 0) {
  topics_str <- paste(h$topics, collapse = ", ")
  lines <- c(lines, sprintf("*Topics: %s*\n\n", topics_str))
}

# Audio clip if available
if (!is.null(clips_manifest) && !is.null(clips_manifest$clips)) {
  # Find matching clip by timestamp
  matching_clip <- purrr::detect(clips_manifest$clips, function(c) {
    c$timestamp_start == h$timestamp_start && isTRUE(c$success)
  })

  if (!is.null(matching_clip) && !is.null(matching_clip$clip_path)) {
    clip_relative <- get_relative_clip_path(matching_clip$clip_path, "episodes")
    lines <- c(lines, sprintf('<audio controls src="%s">Your browser does not support audio.</audio>\n\n',
                              clip_relative))
  }
}

lines <- c(lines, "---\n\n")

paste(lines, collapse = "")
}

#' Generate transcript subpage (.qmd)
#' @param episode_metadata Episode metadata
#' @param transcript_md Formatted markdown transcript
#' @param output_dir Output directory
#' @return Path to generated file
generate_transcript_page <- function(episode_metadata,
                                   transcript_md,
                                   output_dir = CONFIG$episodes_qmd_dir) {

slug <- episode_metadata$slug
transcript_slug <- paste0(slug, "_transcript")
output_path <- file.path(get_project_root(), output_dir, paste0(transcript_slug, ".qmd"))

date_iso <- format(episode_metadata$pub_date, "%Y-%m-%d")

# Build YAML header
yaml_header <- sprintf('---
title: "Transcript: %s"
date: %s
---
', gsub('"', '\\"', episode_metadata$title), date_iso)

# Content
content <- character()
content <- c(content, sprintf("[← Back to Episode Analysis](%s.qmd)\n\n", slug))
content <- c(content, "---\n\n")
content <- c(content, transcript_md)

# Write file
ensure_dir(dirname(output_path))
writeLines(c(yaml_header, paste(content, collapse = "")), output_path)
log_msg("INFO", "Generated transcript page: {output_path}")

output_path
}

#' Generate or update index.qmd with episode list
#' @param all_episodes tibble of all episodes
#' @param channel_info Channel metadata
#' @return Path to index.qmd
generate_index_page <- function(all_episodes, channel_info) {
output_path <- file.path(get_project_root(), "index.qmd")

# Build YAML header
yaml_header <- '---
title: "Your Province. Your Premier."
subtitle: "Healthcare Policy Analysis"
---
'

# Build content
content <- character()

# Podcast description
content <- c(content, "## About This Podcast\n\n")
if (!is.null(channel_info$description) && nchar(channel_info$description) > 0) {
  content <- c(content, sprintf("%s\n\n", channel_info$description))
}
content <- c(content, "Host **Wayne Nelson** welcomes **Premier Danielle Smith** to discuss key provincial issues affecting Albertans.\n\n")

# Healthcare focus explanation
content <- c(content, "## Healthcare Focus\n\n")
content <- c(content, "This site highlights content specifically related to:\n\n")
content <- c(content, "- Physicians and doctors\n")
content <- c(content, "- Healthcare system and policy\n")
content <- c(content, "- Hospitals and clinics\n")
content <- c(content, "- Alberta Health Services (AHS)\n")
content <- c(content, "- Medical services access and reform\n\n")

# Episode list
content <- c(content, "## Episodes\n\n")

# Filter to processed episodes (those with .qmd files)
processed_episodes <- all_episodes |>
  dplyr::filter(file.exists(file.path(get_project_root(), CONFIG$episodes_qmd_dir, paste0(slug, ".qmd"))))

if (nrow(processed_episodes) > 0) {
  content <- c(content, "| Date | Episode | Healthcare Score |\n")
  content <- c(content, "|------|---------|------------------|\n")

  for (i in seq_len(nrow(processed_episodes))) {
    ep <- processed_episodes[i, ]
    date_str <- format(ep$pub_date, "%Y-%m-%d")

    # Try to load analysis for healthcare score
    analysis_path <- file.path(get_project_root(), CONFIG$data_dir, ep$guid, "analysis.json")
    score <- if (file.exists(analysis_path)) {
      analysis <- safe_read_json(analysis_path)
      if (!is.null(analysis$healthcare_focus_score)) {
        sprintf("%d%%", as.integer(analysis$healthcare_focus_score))
      } else {
        "—"
      }
    } else {
      "—"
    }

    content <- c(content, sprintf("| %s | [%s](episodes/%s.qmd) | %s |\n",
                                  date_str,
                                  gsub("\\|", "\\\\|", ep$title),
                                  ep$slug,
                                  score))
  }
} else {
  content <- c(content, "*No episodes have been processed yet.*\n")
}

content <- c(content, "\n\n---\n\n")
content <- c(content, "*Analysis generated using AssemblyAI for transcription and Claude for content analysis.*\n")

# Write file
writeLines(c(yaml_header, paste(content, collapse = "")), output_path)
log_msg("INFO", "Generated index page: {output_path}")

output_path
}

#' Update _quarto.yml with episode chapters
#' @param all_episodes tibble of all episodes (sorted newest first)
#' @return Path to _quarto.yml
update_quarto_yml <- function(all_episodes) {
output_path <- file.path(get_project_root(), "_quarto.yml")

# Filter to processed episodes
processed_episodes <- all_episodes |>
  dplyr::filter(file.exists(file.path(get_project_root(), CONFIG$episodes_qmd_dir, paste0(slug, ".qmd")))) |>
  dplyr::arrange(dplyr::desc(pub_date))

# Build chapters list - episode pages and their transcripts
episode_chapters <- purrr::map_chr(seq_len(nrow(processed_episodes)), function(i) {
  ep <- processed_episodes[i, ]
  paste0(
    sprintf("      - episodes/%s.qmd\n", ep$slug),
    sprintf("      - episodes/%s_transcript.qmd", ep$slug)
  )
})

# Build full YAML
yaml_content <- sprintf('project:
  type: book
  output-dir: _book
  resources:
    - "data/episodes/*/clips/*.mp3"

book:
  title: "Your Province. Your Premier."
  subtitle: "Healthcare Content Analysis"
  description: |
    Analysis of healthcare-related content from Alberta Premier Danielle Smith
    podcast "Your Province. Your Premier." featuring host Wayne Nelson.
  author: "Automated Analysis"
  date: today
  date-format: "MMMM D, YYYY"

  sidebar:
    style: floating
    search: true

  chapters:
    - index.qmd
    - part: "Episodes"
      chapters:
%s
    - about.qmd

format:
  html:
    theme:
      light: cosmo
      dark: darkly
    css: styles.css
    toc: true
    toc-depth: 2
    number-sections: false

execute:
  freeze: auto

lang: en-CA
', paste(episode_chapters, collapse = "\n"))

writeLines(yaml_content, output_path)
log_msg("INFO", "Updated _quarto.yml with {nrow(processed_episodes)} episodes")

output_path
}

#' Regenerate entire Quarto book structure
#' @param episodes_to_generate tibble of episodes to generate pages for
#' @param all_episodes tibble of all episodes (for index)
#' @param channel_info Channel metadata
#' @return List of generated file paths
regenerate_book <- function(episodes_to_generate, all_episodes, channel_info) {
generated_files <- list()

log_msg("INFO", "Regenerating Quarto book with {nrow(episodes_to_generate)} episodes")

# Generate episode pages
for (i in seq_len(nrow(episodes_to_generate))) {
  ep <- episodes_to_generate[i, ]
  episode_dir <- file.path(get_project_root(), CONFIG$data_dir, ep$guid)

  # Load analysis
  analysis <- load_analysis(episode_dir)
  if (is.null(analysis)) {
    log_msg("WARN", "No analysis found for episode: {ep$title}")
    next
  }

  # Load transcript
  transcript_md <- load_formatted_transcript(episode_dir)
  if (is.null(transcript_md)) {
    log_msg("WARN", "No transcript found for episode: {ep$title}")
    next
  }

  # Load clips manifest
  clips_manifest <- load_clips_manifest(episode_dir)

  # Generate pages
  ep_page <- generate_episode_page(ep, analysis, clips_manifest)
  generated_files$episode_pages <- c(generated_files$episode_pages, ep_page)

  tx_page <- generate_transcript_page(ep, transcript_md)
  generated_files$transcript_pages <- c(generated_files$transcript_pages, tx_page)
}

# Generate index
index_page <- generate_index_page(all_episodes, channel_info)
generated_files$index <- index_page

# Update _quarto.yml
yml_path <- update_quarto_yml(all_episodes)
generated_files$quarto_yml <- yml_path

log_msg("INFO", "Book regeneration complete")
generated_files
}

#' Generate pages for a single episode
#' @param episode Episode metadata row
#' @param episode_dir Episode data directory
#' @return List of generated paths
generate_single_episode_pages <- function(episode, episode_dir) {
analysis <- load_analysis(episode_dir)
transcript_md <- load_formatted_transcript(episode_dir)
clips_manifest <- load_clips_manifest(episode_dir)

if (is.null(analysis) || is.null(transcript_md)) {
  log_msg("WARN", "Missing data for episode: {episode$title}")
  return(NULL)
}

list(
  episode_page = generate_episode_page(episode, analysis, clips_manifest),
  transcript_page = generate_transcript_page(episode, transcript_md)
)
}
