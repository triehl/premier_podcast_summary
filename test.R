# For testing purposes we have to delete certain files and reset statuses

# For rerunning transcript analysis
# deletee analysis file
clean_episode_folders <- function() {
  data_dir <- file.path(get_project_root(), CONFIG$data_dir)
  episode_dirs <- dir_ls(data_dir, type = "directory")

  for (episode_dir in episode_dirs) {
    # Delete files except the ones to keep
    files <- dir_ls(episode_dir, type = "file")
    files_to_delete <- files[
      !path_file(files) %in%
        c(
          "episode.mp3",
          "assemblyai_raw.json",
          "speaker_mapping.json",
          "transcript.md",
          "transcript.json"
        )
    ]
    file_delete(files_to_delete)

    # Delete clips subfolder if it exists
    clips_dir <- file.path(episode_dir, "clips")
    if (dir_exists(clips_dir)) {
      dir_delete(clips_dir)
    }

    cli_alert_info("Cleaned {episode_dir}")
  }
}

# write a function that searches cache/episodes_cache.json for episodes with status "complete" and resets their status to "transcribed"
reset_processing_status <- function() {
  # clean out cached files
  clean_episode_folders()

  cache_df <- load_episodes_cache()

  completed_episodes <- cache_df |>
    dplyr::filter(status == EPISODE_STATUS$complete)

  for (i in seq_len(nrow(completed_episodes))) {
    episode <- completed_episodes[i, ]
    cli_alert_info("Resetting episode {episode$guid} status")
    cache_df <- update_episode_status(
      episode$guid,
      EPISODE_STATUS$formatted,
      cache_df
    )
  }
}

#---------------------------------------------------------------------------

# For testing
#
reset_processing_status()


#---

quote <-
  "You know, I have a very dear friend who is a paramedic who has been given me a blow by blow of where things have improved and then where they have regressed. So part of the solution to that is we've created a new specialized emergency or EMS oversight so that there is. So that the people making the decisions are making them in the interest of what paramedics need. So this new oversight just came in place I believe September 1st and we're going to work through all of the issues that were identified in a previous report. Part of what we had seen is that ambulances are being treated like an extension of Hallway medicine. And so if you have people who are in long term care patients who are in acute care beds, then that prevents us from being able to move emergency patients into those beds. And then when the emergency beds fill up, then the hallways fill up. And then when the hallways fill up, they just keep parking ambulances. So the solution is we've got to get those alternative level of care patients out. We're about to start into a real effort on some specific hospitals to figure out how to do this efficiently. We have just started focusing in on the patients that were in Hospital over 300 days. Imagine that, like we had people who were living in hospital for over 300 days. There were seven in particular who'd been in hospital for a combined 18 years. We found an appropriate place for them and now we're going to make sure everyone who's been in the hospital over 300 days has deficient to move o and then we'll go to 200 days and then we'll go to 100 days. And by doing so, we hope to get that 1700 alternative level of care patients that we know we have in hospitals out so that we can start moving the patient flow. It's taking a lot longer than I wanted to to get here. It took a lot of sort of structural change to get there. But we're there right now and I'm with you. The other thing I should just say about bargaining, because I know unfortunately HSAA also didn't accept our most recent proposal. It was about a 6040 split. And so I know that there are some people happy and some that are not. But one of the processes is for us to do market adjustments. If we have a particular employee group that is not being paid, they're like the same relative to sort of that Western average, then there is room for an adjustment. So I would just encourage John to go and have a look at what that looks like because we've been very surgical, especially with HSAA. There's about 150 different professions that that they represent, paramedics being one of them. And so I hope we've got that right. But have a look at what the most recent proposal was because if we can get that passed, it should be able to address a lot of things that you've identified."

transcript <-
  "You know, I have a very dear friend who is a paramedic who has been given me a blow by blow of where things have improved and then where they have regressed. So part of the solution to that is we've created a new specialized emergency or EMS oversight so that there is. So that the people making the decisions are making them in the interest of what paramedics need. So this new oversight just came in place I believe September 1st and we're going to work through all of the issues that were identified in a previous report. Part of what we had seen is that ambulances are being treated like an extension of Hallway medicine. And so if you have people who are in long term care patients who are in acute care beds, then that prevents us from being able to move emergency patients into those beds. And then when the emergency beds fill up, then the hallways fill up. And then when the hallways fill up, they just keep parking ambulances. So the solution is we've got to get those alternative level of care patients out. We're about to start into a real effort on some specific hospitals to figure out how to do this efficiently. We have just started focusing in on the patients that were in Hospital over 300 days. Imagine that, like we had people who were living in hospital for over 300 days. There were seven in particular who'd been in hospital for a combined 18 years. We found an appropriate place for them and now we're going to make sure everyone who's been in the hospital over 300 days has deficient to move o and then we'll go to 200 days and then we'll go to 100 days. And by doing so, we hope to get that 1700 alternative level of care patients that we know we have in hospitals out so that we can start moving the patient flow. It's taken a lot longer than I wanted to to get here. It took a lot of sort of structural change to get there. But we're there right now and I'm with you. The other thing I should just say about bargaining, because I know unfortunately HSAA also didn't accept our most recent proposal. It was about a 6040 split. And so I know that there are some people happy and some that are not. But one of the processes is for us to do market adjustments. If we have a particular employee group that is not being paid, they're like the same relative to sort of that Western average, then there is room for an adjustment. So I would just encourage John to go and have a look at what that looks like because we've been very surgical, especially with HSAA. There's about 150 different professions that that they represent, paramedics being one of them. And so I hope we've got that right. But have a look at what the most recent proposal was because if we can get that passed, it should be able to address a lot of things that you've identified."

if (quote == transcript) {
  print("The quote and transcript are identical.")
} else {
  print("The quote and transcript differ.")
}
