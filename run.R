#!/usr/bin/env Rscript
# Premier Podcast Summary - Main Pipeline
#
# This script orchestrates the full pipeline:
# 1. Parse RSS feed for new episodes
# 2. Download MP3 files
# 3. Transcribe with AssemblyAI
# 4. Format transcripts with Claude
# 5. Analyze healthcare content with Claude
# 6. Extract audio clips with FFmpeg
# 7. Generate Quarto book pages
# 8. Render the book

# ============================================================================
# Setup
# ============================================================================

# Required packages
required_packages <- c(
  "tidyverse",
  "httr2",
  "xml2",
  "jsonlite",
  "fs",
  "cli",
  "glue",
  "purrr"
)

# Check and load packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with: install.packages('%s')", pkg, pkg))
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Source all R modules
source("R/00_config.R")
source("R/utils.R")
source("R/01_rss_parser.R")
source("R/02_cache_manager.R")
source("R/03_downloader.R")
source("R/04_assemblyai.R")
source("R/05_claude_transcriber.R")
source("R/06_claude_analyzer.R")
source("R/07_audio_clipper.R")
source("R/08_quarto_generator.R")

# ============================================================================
# Main Pipeline
# ============================================================================

#' Run the full podcast processing pipeline
#'
#' @param max_episodes Maximum number of episodes to process (default from config)
#' @param force_reprocess Force reprocessing of all episodes
#' @param skip_render Skip Quarto book rendering
#' @return Invisible NULL
run_pipeline <- function(max_episodes = CONFIG$max_episodes,
                        force_reprocess = FALSE,
                        skip_render = FALSE) {

  cli_h1("Premier Podcast Summary Pipeline")
  cli_alert_info("Processing up to {max_episodes} episodes")

  # -------------------------------------------------------------------------
  # Step 1: Initialize
  # -------------------------------------------------------------------------
  cli_h2("Step 1: Initializing")

  # Check for API keys
  tryCatch({
    get_env_var("ANTHROPIC_API_KEY")
    cli_alert_success("Anthropic API key found")
  }, error = function(e) {
    cli_alert_danger("Anthropic API key not found!")
    cli_alert_info("Copy .Renviron.example to .Renviron and add your API key")
    stop("Missing ANTHROPIC_API_KEY")
  })

  tryCatch({
    get_env_var("ASSEMBLYAI_API_KEY")
    cli_alert_success("AssemblyAI API key found")
  }, error = function(e) {
    cli_alert_danger("AssemblyAI API key not found!")
    cli_alert_info("Copy .Renviron.example to .Renviron and add your API key")
    stop("Missing ASSEMBLYAI_API_KEY")
  })

  # Initialize cache
  init_cache()
  cli_alert_success("Cache initialized")

  # Check FFmpeg
  ffmpeg_available <- check_ffmpeg()
  if (ffmpeg_available) {
    cli_alert_success("FFmpeg found")
  } else {
    cli_alert_warning("FFmpeg not found - audio clips will be skipped")
  }

  # -------------------------------------------------------------------------
  # Step 2: Parse RSS Feed
  # -------------------------------------------------------------------------
  cli_h2("Step 2: Parsing RSS Feed")

  channel_info <- get_channel_info()
  cli_alert_success("Podcast: {channel_info$title}")

  all_episodes <- parse_rss_feed()
  cli_alert_success("Found {nrow(all_episodes)} total episodes")

  # -------------------------------------------------------------------------
  # Step 3: Identify Episodes to Process
  # -------------------------------------------------------------------------
  cli_h2("Step 3: Checking Cache")

  cache_df <- load_episodes_cache()

  if (force_reprocess) {
    cli_alert_warning("Force reprocess enabled - ignoring cache")
    episodes_to_process <- all_episodes |>
      dplyr::slice_head(n = max_episodes) |>
      dplyr::mutate(episode_dir = file.path(get_project_root(), CONFIG$data_dir, guid))
  } else {
    episodes_to_process <- get_episodes_to_process(all_episodes, cache_df, max_episodes)
  }

  if (nrow(episodes_to_process) == 0) {
    cli_alert_success("All episodes are up to date!")

    if (!skip_render) {
      cli_h2("Rendering Quarto Book")
      render_book()
    }

    cli_h1("Pipeline Complete!")
    return(invisible(NULL))
  }

  cli_alert_info("Processing {nrow(episodes_to_process)} episode(s)")

  # -------------------------------------------------------------------------
  # Step 4: Download Episodes
  # -------------------------------------------------------------------------
  cli_h2("Step 4: Downloading Episodes")

  for (i in seq_len(nrow(episodes_to_process))) {
    episode <- episodes_to_process[i, ]
    cli_alert_info("[{i}/{nrow(episodes_to_process)}] {episode$title}")

    mp3_path <- download_episode_mp3(episode)

    if (!is.null(mp3_path)) {
      cache_df <- update_episode_status(
        episode$guid,
        EPISODE_STATUS$downloaded,
        cache_df,
        extra_fields = list(mp3_path = mp3_path)
      )
    }
  }

  # -------------------------------------------------------------------------
  # Step 5: Transcribe with AssemblyAI
  # -------------------------------------------------------------------------
  cli_h2("Step 5: Transcribing with AssemblyAI")

  for (i in seq_len(nrow(episodes_to_process))) {
    episode <- episodes_to_process[i, ]
    episode_dir <- episode$episode_dir

    current_status <- get_episode_status(episode$guid, cache_df)
    if (!is.null(current_status) && current_status %in% c(
      EPISODE_STATUS$transcribed, EPISODE_STATUS$formatted,
      EPISODE_STATUS$analyzed, EPISODE_STATUS$complete
    )) {
      cli_alert_info("[{i}/{nrow(episodes_to_process)}] Already transcribed: {episode$title}")
      next
    }

    cli_alert_info("[{i}/{nrow(episodes_to_process)}] Transcribing: {episode$title}")

    tryCatch({
      result <- transcribe_episode(episode_dir)

      cache_df <- update_episode_status(
        episode$guid,
        EPISODE_STATUS$transcribed,
        cache_df,
        extra_fields = list(transcript_id = result$id)
      )

      cli_alert_success("Transcription complete")
    }, error = function(e) {
      cli_alert_danger("Transcription failed: {e$message}")
      cache_df <<- update_episode_status(
        episode$guid,
        EPISODE_STATUS$error,
        cache_df,
        extra_fields = list(error_message = e$message)
      )
    })
  }

  # -------------------------------------------------------------------------
  # Step 6: Format Transcripts with Claude
  # -------------------------------------------------------------------------
  cli_h2("Step 6: Formatting Transcripts")

  for (i in seq_len(nrow(episodes_to_process))) {
    episode <- episodes_to_process[i, ]
    episode_dir <- episode$episode_dir

    current_status <- get_episode_status(episode$guid, cache_df)
    if (!is.null(current_status) && current_status %in% c(
      EPISODE_STATUS$formatted, EPISODE_STATUS$analyzed, EPISODE_STATUS$complete
    )) {
      cli_alert_info("[{i}/{nrow(episodes_to_process)}] Already formatted: {episode$title}")
      next
    }

    cli_alert_info("[{i}/{nrow(episodes_to_process)}] Formatting: {episode$title}")

    tryCatch({
      format_episode_transcript(episode_dir, episode)

      cache_df <- update_episode_status(
        episode$guid,
        EPISODE_STATUS$formatted,
        cache_df
      )

      cli_alert_success("Formatting complete")
    }, error = function(e) {
      cli_alert_danger("Formatting failed: {e$message}")
      cache_df <<- update_episode_status(
        episode$guid,
        EPISODE_STATUS$error,
        cache_df,
        extra_fields = list(error_message = e$message)
      )
    })
  }

  # -------------------------------------------------------------------------
  # Step 7: Analyze Healthcare Content with Claude
  # -------------------------------------------------------------------------
  cli_h2("Step 7: Analyzing Healthcare Content")

  for (i in seq_len(nrow(episodes_to_process))) {
    episode <- episodes_to_process[i, ]
    episode_dir <- episode$episode_dir

    current_status <- get_episode_status(episode$guid, cache_df)
    if (!is.null(current_status) && current_status %in% c(
      EPISODE_STATUS$analyzed, EPISODE_STATUS$complete
    )) {
      cli_alert_info("[{i}/{nrow(episodes_to_process)}] Already analyzed: {episode$title}")
      next
    }

    cli_alert_info("[{i}/{nrow(episodes_to_process)}] Analyzing: {episode$title}")

    tryCatch({
      analysis <- analyze_episode(episode_dir, episode)

      n_highlights <- length(analysis$highlights)
      score <- analysis$healthcare_focus_score %||% 0

      cli_alert_success("Found {n_highlights} healthcare highlights (score: {score}%)")

      cache_df <- update_episode_status(
        episode$guid,
        EPISODE_STATUS$analyzed,
        cache_df
      )
    }, error = function(e) {
      cli_alert_danger("Analysis failed: {e$message}")
      cache_df <<- update_episode_status(
        episode$guid,
        EPISODE_STATUS$error,
        cache_df,
        extra_fields = list(error_message = e$message)
      )
    })
  }

  # -------------------------------------------------------------------------
  # Step 8: Extract Audio Clips
  # -------------------------------------------------------------------------
  if (ffmpeg_available) {
    cli_h2("Step 8: Extracting Audio Clips")

    for (i in seq_len(nrow(episodes_to_process))) {
      episode <- episodes_to_process[i, ]
      episode_dir <- episode$episode_dir

      cli_alert_info("[{i}/{nrow(episodes_to_process)}] Extracting clips: {episode$title}")

      tryCatch({
        clips <- extract_episode_clips(episode_dir)

        n_clips <- sum(clips$success)
        cli_alert_success("Extracted {n_clips} clips")

        cache_df <- update_episode_status(
          episode$guid,
          EPISODE_STATUS$clipped,
          cache_df
        )
      }, error = function(e) {
        cli_alert_warning("Clip extraction failed: {e$message}")
        # Don't mark as error - clips are optional
      })
    }
  } else {
    cli_h2("Step 8: Skipping Audio Clips (FFmpeg not available)")
  }

  # -------------------------------------------------------------------------
  # Step 9: Generate Quarto Pages
  # -------------------------------------------------------------------------
  cli_h2("Step 9: Generating Quarto Pages")

  for (i in seq_len(nrow(episodes_to_process))) {
    episode <- episodes_to_process[i, ]
    episode_dir <- episode$episode_dir

    cli_alert_info("[{i}/{nrow(episodes_to_process)}] Generating pages: {episode$title}")

    tryCatch({
      generate_single_episode_pages(episode, episode_dir)

      cache_df <- update_episode_status(
        episode$guid,
        EPISODE_STATUS$complete,
        cache_df
      )

      cli_alert_success("Pages generated")
    }, error = function(e) {
      cli_alert_danger("Page generation failed: {e$message}")
    })
  }

  # Regenerate index page
  cli_alert_info("Updating index page")
  generate_index_page(all_episodes, channel_info)

  # Update _quarto.yml
  cli_alert_info("Updating _quarto.yml")
  update_quarto_yml(all_episodes)

  # -------------------------------------------------------------------------
  # Step 10: Render Quarto Book
  # -------------------------------------------------------------------------
  if (!skip_render) {
    cli_h2("Step 10: Rendering Quarto Book")
    render_book()
  } else {
    cli_h2("Step 10: Skipping Render (--skip-render)")
  }

  # -------------------------------------------------------------------------
  # Done
  # -------------------------------------------------------------------------
  cli_h1("Pipeline Complete!")

  # Summary
  final_cache <- load_episodes_cache()
  complete <- sum(final_cache$status == EPISODE_STATUS$complete)
  errors <- sum(final_cache$status == EPISODE_STATUS$error)

  cli_alert_success("Complete: {complete} episodes")
  if (errors > 0) {
    cli_alert_warning("Errors: {errors} episodes")
  }

  invisible(NULL)
}

#' Render the Quarto book
render_book <- function() {
  cli_alert_info("Rendering Quarto book...")

  # Check if quarto is available
  quarto_available <- tryCatch({
    system2("quarto", "--version", stdout = TRUE, stderr = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!quarto_available) {
    cli_alert_warning("Quarto CLI not found. Install from: https://quarto.org/docs/get-started/")
    cli_alert_info("You can render manually with: quarto render")
    return(invisible(NULL))
  }

  result <- system2("quarto", "render", stdout = TRUE, stderr = TRUE)

  if (attr(result, "status") %||% 0 == 0) {
    cli_alert_success("Book rendered to _book/")
    cli_alert_info("Open _book/index.html in your browser to view")
  } else {
    cli_alert_danger("Quarto render failed")
    cat(result, sep = "\n")
  }

  invisible(NULL)
}

#' Process a single episode (for testing/debugging)
#'
#' @param episode_number Which episode to process (1 = most recent)
#' @param force Force reprocessing
#' @return Episode metadata
process_single_episode <- function(episode_number = 1, force = FALSE) {
  cli_h1("Processing Single Episode")

  # Initialize
  init_cache()

  # Get episode
  all_episodes <- parse_rss_feed()
  episode <- all_episodes[episode_number, ]

  cli_alert_info("Episode: {episode$title}")

  episode_dir <- file.path(get_project_root(), CONFIG$data_dir, episode$guid)
  ensure_dir(episode_dir)

  # Download
  cli_h2("Downloading")
  mp3_path <- download_episode_mp3(episode)

  # Transcribe
  cli_h2("Transcribing")
  transcript <- transcribe_episode(episode_dir, force = force)

  # Format
  cli_h2("Formatting")
  format_episode_transcript(episode_dir, episode, force = force)

  # Analyze
  cli_h2("Analyzing")
  analysis <- analyze_episode(episode_dir, episode, force = force)

  # Clips
  cli_h2("Extracting Clips")
  if (check_ffmpeg()) {
    clips <- extract_episode_clips(episode_dir, analysis)
  }

  # Generate pages
  cli_h2("Generating Pages")
  generate_single_episode_pages(episode, episode_dir)

  cli_h1("Done!")
  episode
}

# ============================================================================
# CLI Interface
# ============================================================================

# Run if executed directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--help" %in% args || "-h" %in% args) {
    cat("
Premier Podcast Summary Pipeline

Usage: Rscript run.R [options]

Options:
  --max-episodes N    Maximum episodes to process (default: 5)
  --force             Force reprocessing of all episodes
  --skip-render       Skip Quarto book rendering
  --help, -h          Show this help message

Examples:
  Rscript run.R                           # Process up to 5 new episodes

  Rscript run.R --max-episodes 10         # Process up to 10 episodes
  Rscript run.R --force                   # Reprocess all episodes
  Rscript run.R --skip-render             # Skip rendering (just process)

")
  } else {
    # Parse arguments
    max_episodes <- CONFIG$max_episodes
    force <- "--force" %in% args
    skip_render <- "--skip-render" %in% args

    max_idx <- which(args == "--max-episodes")
    if (length(max_idx) > 0 && max_idx < length(args)) {
      max_episodes <- as.integer(args[max_idx + 1])
    }

    # Run pipeline
    run_pipeline(
      max_episodes = max_episodes,
      force_reprocess = force,
      skip_render = skip_render
    )
  }
}
