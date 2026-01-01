# Quarto Page Generator for Premier Podcast Summary
# Generates Quarto documents from episode data

#' Generate episode analysis page (.qmd)
#' @param episode_metadata Episode metadata (from RSS)
#' @param analysis Analysis results
#' @param clips_manifest Clips manifest (optional)
#' @param transcript_md Transcript markdown (optional, for anchor links)
#' @param output_dir Output directory for .qmd files
#' @return Path to generated file
generate_episode_page <- function(
  episode_metadata,
  analysis,
  clips_manifest = NULL,
  transcript_md = NULL,
  output_dir = CONFIG$episodes_qmd_dir
) {
  slug <- episode_metadata$slug
  output_path <- file.path(get_project_root(), output_dir, paste0(slug, ".qmd"))

  # Format date for display
  date_display <- format(episode_metadata$pub_date, "%B %-d, %Y")
  date_iso <- format(episode_metadata$pub_date, "%Y-%m-%d")

  # Build YAML header
  yaml_header <- sprintf(
    '---
title: "%s"
---
',
    paste0(date_display, " episode")
  )

  # Build content sections
  content <- character()

  # Overall summary section
  content <- c(content, "# Episode Overview\n")
  #content <- c(content, sprintf("**Air Date:** %s\n\n", date_display))

  # add disclaimer
  content <- c(
    content,
    ":::{.callout-note appearance=\"minimal\"}\n***Note:** The episode transcripts are computer generated from the [radio show audio files](https://globalnews.ca/edmonton/program/your-province-your-premier){target=\"_blank\"}. As a result, some acronyms or text may not be transcribed correctly or the speaker may be misidentified. Confirm the actual content from the audio file before quoting from an episode.*\n:::\n\n"
  )

  if (
    !is.null(analysis$overall_summary) && nchar(analysis$overall_summary) > 0
  ) {
    content <- c(content, sprintf("%s\n\n", analysis$overall_summary))
  }

  if (!is.null(analysis$healthcare_focus_score)) {
    # TREVOR NOTE: hide score
    # content <- c(
    #   content,
    #   sprintf(
    #     "**Healthcare Focus Score:** %d%%\n\n",
    #     as.integer(analysis$healthcare_focus_score)
    #   )
    # )
  }

  # Healthcare highlights section
  content <- c(content, "# Healthcare Highlights\n\n")

  if (!is.null(analysis$highlights) && length(analysis$highlights) > 0) {
    # Group by relevance
    high_highlights <- purrr::keep(
      analysis$highlights,
      ~ .x$relevance == "High"
    )
    medium_highlights <- purrr::keep(
      analysis$highlights,
      ~ .x$relevance == "Medium"
    )

    if (length(high_highlights) > 0) {
      #content <- c(content, "### Key Points\n\n")
      for (h in high_highlights) {
        content <- c(
          content,
          format_highlight_block(
            h,
            clips_manifest,
            episode_metadata,
            transcript_md
          )
        )
      }
    }

    if (length(medium_highlights) > 0) {
      #content <- c(content, "### Additional Mentions\n\n")
      for (h in medium_highlights) {
        content <- c(
          content,
          format_highlight_block(
            h,
            clips_manifest,
            episode_metadata,
            transcript_md
          )
        )
      }
    }
  } else {
    content <- c(
      content,
      "*No significant healthcare content identified in this episode.*\n\n"
    )
  }

  # Transcript link
  transcript_slug <- paste0(slug, "_transcript")
  content <- c(
    content,
    sprintf(
      "\n# Full Transcript\n\n[View the complete transcript](%s.qmd)\n",
      transcript_slug
    )
  )

  # Duration info
  if (
    !is.null(episode_metadata$duration_seconds) &&
      !is.na(episode_metadata$duration_seconds)
  ) {
    content <- c(
      content,
      sprintf(
        "\n---\n\n*Episode Duration: %s*\n",
        format_duration(episode_metadata$duration_seconds)
      )
    )
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
#' @param transcript_md Transcript markdown for anchor lookup
#' @return Formatted markdown string
format_highlight_block <- function(
  h,
  clips_manifest = NULL,
  episode_metadata = NULL,
  transcript_md = NULL
) {
  lines <- character()

  # Topics
  if (!is.null(h$topics) && length(h$topics) > 0) {
    topics_str <- paste(h$topics, collapse = ", ")
    lines <- c(lines, sprintf("## Topics: %s\n\n", topics_str))
  }

  # Timestamp and summary
  lines <- c(
    lines,
    sprintf(
      "**[%s - %s]** %s\n\n",
      h$timestamp_start,
      h$timestamp_end,
      h$summary %||% ""
    )
  )

  # Quote if available
  if (!is.null(h$quote) && nchar(h$quote) > 0) {
    lines <- c(lines, sprintf("> \"%s\"\n\n", h$quote))
  }

  # Audio clip if available
  if (!is.null(clips_manifest) && !is.null(clips_manifest$clips)) {
    # Find matching clip by timestamp
    matching_clip <- purrr::detect(clips_manifest$clips, function(c) {
      c$timestamp_start == h$timestamp_start && isTRUE(c$success)
    })

    if (!is.null(matching_clip) && !is.null(matching_clip$clip_path)) {
      clip_relative <- get_relative_clip_path(
        matching_clip$clip_path,
        "episodes"
      )
      lines <- c(
        lines,
        sprintf(
          '<audio controls src="%s">Your browser does not support audio.</audio>\n\n',
          clip_relative
        )
      )
    }
  }

  # Link to transcript section
  if (!is.null(episode_metadata$slug) && !is.null(transcript_md)) {
    transcript_slug <- paste0(episode_metadata$slug, "_transcript")
    anchor <- create_transcript_anchor(h$timestamp_start, transcript_md)
    if (!is.null(anchor)) {
      lines <- c(
        lines,
        sprintf("[View in transcript](%s.qmd#%s)\n\n", transcript_slug, anchor)
      )
    }
  }

  lines <- c(lines, "---\n\n")

  paste(lines, collapse = "")
}

#' Create anchor ID for transcript section from timestamp
#' @param timestamp Timestamp string like "11:00"
#' @param transcript_md Transcript markdown to search for matching section
#' @return Anchor ID string or NULL if not found
create_transcript_anchor <- function(timestamp, transcript_md = NULL) {
  if (is.null(transcript_md)) {
    return(NULL)
  }

  # Find section header matching or near this timestamp
  # Headers look like: ## [11:00] AISH Application Barriers
  pattern <- sprintf("## \\[%s\\] ([^\n]+)", gsub(":", ":", timestamp))
  match <- regmatches(
    transcript_md,
    regexpr(pattern, transcript_md, perl = TRUE)
  )

  if (length(match) > 0 && nchar(match[1]) > 0) {
    # Extract title from match like "## [11:00] AISH Application Barriers"
    title <- sub("^## \\[[0-9:]+\\] ", "", match[1])
    # Convert to Quarto anchor format (lowercase, hyphens, no special chars)
    anchor <- tolower(title)
    anchor <- gsub("[^a-z0-9 ]", "", anchor)
    anchor <- gsub("\\s+", "-", trimws(anchor))
    return(anchor)
  }

  # Try finding nearest section by converting timestamp to seconds
  ts_seconds <- timestamp_to_seconds(timestamp)
  if (is.na(ts_seconds)) {
    return(NULL)
  }

  # Find all section timestamps
  all_sections <- gregexpr(
    "## \\[([0-9]+:[0-9]+)\\] ([^\n]+)",
    transcript_md,
    perl = TRUE
  )
  matches <- regmatches(transcript_md, all_sections)[[1]]

  if (length(matches) == 0) {
    return(NULL)
  }

  # Parse each section and find the closest one at or before our timestamp
  best_match <- NULL
  best_diff <- Inf

  for (m in matches) {
    section_ts <- sub("^## \\[([0-9:]+)\\].*", "\\1", m)
    section_seconds <- timestamp_to_seconds(section_ts)
    if (!is.na(section_seconds) && section_seconds <= ts_seconds) {
      diff <- ts_seconds - section_seconds
      if (diff < best_diff) {
        best_diff <- diff
        best_match <- m
      }
    }
  }

  if (!is.null(best_match)) {
    title <- sub("^## \\[[0-9:]+\\] ", "", best_match)
    anchor <- tolower(title)
    anchor <- gsub("[^a-z0-9 ]", "", anchor)
    anchor <- gsub("\\s+", "-", trimws(anchor))
    return(anchor)
  }

  NULL
}

#' Generate transcript subpage (.qmd)
#' @param episode_metadata Episode metadata
#' @param transcript_md Formatted markdown transcript
#' @param output_dir Output directory
#' @return Path to generated file
generate_transcript_page <- function(
  episode_metadata,
  transcript_md,
  output_dir = CONFIG$episodes_qmd_dir
) {
  slug <- episode_metadata$slug
  transcript_slug <- paste0(slug, "_transcript")
  output_path <- file.path(
    get_project_root(),
    output_dir,
    paste0(transcript_slug, ".qmd")
  )

  date_display <- format(episode_metadata$pub_date, "%B %-d, %Y")
  date_iso <- format(episode_metadata$pub_date, "%Y-%m-%d")

  # Build YAML header
  yaml_header <- sprintf(
    '---\ntitle: "Transcript: %s"\n---\n',
    paste0(date_display, " episode")
  )

  # Content
  content <- character()
  content <- c(
    content,
    sprintf("[← Back to Episode Analysis](%s.qmd)\n\n", slug)
  )

  # add disclaimer
  content <- c(
    content,
    ":::{.callout-note appearance=\"minimal\"}\n***Note:** The episode transcripts are computer generated from the [radio show audio files](https://globalnews.ca/edmonton/program/your-province-your-premier){target=\"_blank\"}. As a result, some acronyms or text may not be transcribed correctly or the speaker may be misidentified. Confirm the actual content from the audio file before quoting from an episode.*\n:::\n\n"
  )

  # break and transcript contents
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
  yaml_header <- '---\ntitle: "Insights from the Premier\'s call-in radio show: *Your Province. Your Premier.*"\ntoc: false\n---\n'

  # Build content
  content <- character()

  # Podcast description

  content <- "# Overview\n\n - **Description:** This app analyzes the [Premier's  call-in radio show](https://globalnews.ca/edmonton/program/your-province-your-premier){target=\"_blank\"} to identify insights related to physicians and the healthcare system.\n\n - **Goal:** Assist PC&E staff identify comments and policy statements made by the Premier which may impact the AMA without having to listen to the entire show.\n\n:::{.callout-note appearance=\"minimal\"}\n***Note:** The episode transcripts are computer generated from the radio show audio files. As a result, some acronyms or text may not be transcribed correctly or the speaker may be misidentified. Confirm the actual content from the audio file before quoting from an episode.*\n:::\n\n"

  # Episode list
  content <- c(content, "# Episodes\n\n")

  # Filter to processed episodes (those with .qmd files)
  processed_episodes <- all_episodes |>
    dplyr::filter(file.exists(file.path(
      get_project_root(),
      CONFIG$episodes_qmd_dir,
      paste0(slug, ".qmd")
    )))

  # Build episode summary table
  if (nrow(processed_episodes) > 0) {
    # Build episode summary dataframe
    episode_table <- purrr::map_dfr(
      seq_len(nrow(processed_episodes)),
      function(i) {
        ep <- processed_episodes[i, ]

        analysis_path <- file.path(
          get_project_root(),
          CONFIG$data_dir,
          ep$guid,
          "analysis.json"
        )

        # Get summary as bullet list (markdown format)
        summary <- if (file.exists(analysis_path)) {
          analysis <- safe_read_json(analysis_path)
          if (!is.null(analysis$overall_summary)) {
            sentences <- strsplit(
              analysis$overall_summary,
              "(?<=\\.)\\s+",
              perl = TRUE
            )[[1]]
            paste(
              "<ul style=\"padding-left: 1rem;\">",
              paste0(
                "<li style=\"margin-bottom: 1rem;\">",
                sentences,
                "</li>",
                collapse = ""
              ),
              "</ul>"
            )
          } else {
            "\u2014"
          }
        } else {
          "\u2014"
        }

        # Get score as emoji indicator
        score <- if (file.exists(analysis_path)) {
          analysis <- safe_read_json(analysis_path)
          if (!is.null(analysis$healthcare_focus_score)) {
            score_num <- as.numeric(gsub(
              "%",
              "",
              analysis$healthcare_focus_score
            ))
            if (score_num <= 15) {
              "\u26ab\u26aa\u26aa"
            } else if (score_num <= 25) {
              "\u26ab\u26ab\u26aa"
            } else {
              "\u26ab\u26ab\u26ab"
            }
          } else {
            "\u2014"
          }
        } else {
          "\u2014"
        }

        tibble::tibble(
          date = ep$pub_date,
          slug = ep$slug,
          summary = summary,
          score = score
        )
      }
    )

    # Save the dataframe for the R code chunk to use
    saveRDS(
      episode_table,
      file.path(get_project_root(), "data", "episode_table.rds")
    )

    # Add R code chunk for gt() table
    content <- c(
      content,
      '```{r}
#| echo: false
#| message: false

library(gt)
library(dplyr)

episode_table <- readRDS("data/episode_table.rds")

episode_table |>
  mutate(
    date_link = paste0("[", trimws(format(date, "%B %e, %Y")), "](episodes/", slug, ".qmd)")
  ) |>
  select(
    Date = date_link,
    `Healthcare highlights` = summary,
    `Healthcare content` = score
  ) |>
  gt() |>
  fmt_markdown(columns = everything()) |>
  cols_width(
    Date ~ px(100),
    `Healthcare highlights` ~ px(524),
    `Healthcare content` ~ px(125)
  ) |>
  tab_options(
    column_labels.font.weight = "bold"
  ) |>
  cols_align(align = "left", columns = c(Date, `Healthcare highlights`)) |>
  cols_align(align = "right", columns = `Healthcare content`) |>
  opt_interactive(
    page_size_default = 2,
    use_sorting = FALSE,
    )
```
'
    )
  } else {
    content <- c(content, "*No episodes have been processed yet.*\n")
  }

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
    dplyr::filter(file.exists(file.path(
      get_project_root(),
      CONFIG$episodes_qmd_dir,
      paste0(slug, ".qmd")
    ))) |>
    dplyr::arrange(dplyr::desc(pub_date))

  # Build chapters list - episode pages and their transcripts
  episode_chapters <- purrr::map_chr(
    seq_len(nrow(processed_episodes)),
    function(i) {
      ep <- processed_episodes[i, ]
      paste0(
        sprintf("          - href: episodes/%s.qmd\n", ep$slug),
        sprintf(
          "            text: %s",
          format(as.Date(substr(ep$slug, 1, 10)), "%B %-d, %Y")
        )
      )
    }
  )

  # Build full YAML
  yaml_content <- sprintf(
    'project:
  type: website
  resources:
    - "data/episodes/*/clips/*.mp3"

website:
  title: ""
  bread-crumbs: false
  description: |
    Analysis of healthcare-related content from Alberta Premier Danielle Smith
    on the "Your Province. Your Premier." radio show.
  navbar:
      pinned: true
      left:
        - text: "Home"
          href: "https://connect.albertadoctors.org/he/"
        - text: "Reports"
          href: "https://connect.albertadoctors.org/he/reports"
        - text: "Apps"
          href: "https://connect.albertadoctors.org/he/apps"
        - text: "Data"
          href: "https://connect.albertadoctors.org/he/data"
        - text: "Learn R"
          href: "https://connect.albertadoctors.org/he/learn"
  sidebar:
    style: floating
    search: true
    contents:
      - text: "🏠️ Insights from the Premier\'s radio show"
        file: index.qmd
      - text: "---"
      - section: "Episodes"
        contents:
%s
      - about.qmd

format:
  html:
    theme:
      - flatly
      - "custom.scss"
    css: amaweb.css
    toc: true
    toc-depth: 2
    number-sections: false
    html-table-processing: none

lang: en-CA
',
    paste(episode_chapters, collapse = "\n")
  )

  writeLines(yaml_content, output_path)
  log_msg(
    "INFO",
    "Updated _quarto.yml with {nrow(processed_episodes)} episodes"
  )

  output_path
}

#' Regenerate entire Quarto book structure
#' @param episodes_to_generate tibble of episodes to generate pages for
#' @param all_episodes tibble of all episodes (for index)
#' @param channel_info Channel metadata
#' @return List of generated file paths
regenerate_book <- function(episodes_to_generate, all_episodes, channel_info) {
  generated_files <- list()

  log_msg(
    "INFO",
    "Regenerating Quarto book with {nrow(episodes_to_generate)} episodes"
  )

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
    ep_page <- generate_episode_page(
      ep,
      analysis,
      clips_manifest,
      transcript_md
    )
    generated_files$episode_pages <- c(generated_files$episode_pages, ep_page)

    tx_page <- generate_transcript_page(ep, transcript_md)
    generated_files$transcript_pages <- c(
      generated_files$transcript_pages,
      tx_page
    )
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
    episode_page = generate_episode_page(
      episode,
      analysis,
      clips_manifest,
      transcript_md
    ),
    transcript_page = generate_transcript_page(episode, transcript_md)
  )
}
