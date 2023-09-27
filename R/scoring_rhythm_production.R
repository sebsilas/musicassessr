

score_rhythm_production <- function(stimuli_durations, user_durations, bpm = NULL) {

  print('dadauser_dur')

  print(user_durations)

  if(is.scalar.na.or.null(user_durations) || length(user_durations) == 0) {

    res <- list(
      stimuli_durations = NA,
      mean_duration = NA,
      precision = NA,
      accuracy = NA,
      dtw_distance = NA,
      tam_distance = NA,
      user_bpm = NA,
      user_durations = NA,
      rhythfuzz = NA
      )

  } else {

    user_durations <- user_durations[!is.na(user_durations)]

    stimuli_bpm <- round(60/stimuli_durations)  # Note, this is not a good way to get the BPM for actual stimuli, but will work for the beat sync trials where the stimuli is basically a metronome :

    mean_dur <- mean(user_durations, na.rm = TRUE)

    if(is.null(bpm)) {
      bpm <- round(60/mean_dur) # This is a proxy but not particularly sophisticated..
    }

    print('user_durations..')
    print(user_durations)

    if(is.scalar.na.or.null(stimuli_durations)) {
      dtw_dist <- NA
      tam_dist <- NA
    } else {
      dtw_res <- dtw::dtw(stimuli_durations, user_durations)
      print('dtw_res')
      print(dtw_res)
      dtw_dist <- dtw_res$distance
      print('dtw_dist')
      print(dtw_dist)
      tam_dist <- TSdist::TAMDistance(stimuli_durations, user_durations)
    }

    res <- list(
      stimuli_durations = stimuli_durations,
      mean_duration = mean_dur,
      precision = sd(user_durations, na.rm = TRUE),
      accuracy = stats::mad(user_durations, center = bpm_to_ms(stimuli_bpm)),
      dtw_distance = dtw_dist,
      tam_distance = tam_dist,
      user_bpm = bpm,
      user_durations = user_durations,
      rhythfuzz = rhythfuzz(stimuli_durations, user_durations)
    )

  }

  return(res)
}


#' Feedback for rhythm production style pages
#'
#' @return
#' @export
#'
#' @examples
feedback_rhythm_production <- function() {

  psychTestR::reactive_page(function(state, answer, ...) {

    stimulus_trigger_times_df <- tibble::tibble(stimulus_trigger_times = answer$stimulus_trigger_times)
    onsets_df <- answer$pyin_style_res

    no_stimulus_trigger_times <- is.scalar.na(stimulus_trigger_times_df$stimulus_trigger_times) && nrow(stimulus_trigger_times_df)  > 0

    is_plottable <- !(no_stimulus_trigger_times && nrow(onsets_df) < 1 || is.scalar.na(onsets_df))

    if(is_plottable) {

      onsets_df <- onsets_df %>%
        dplyr::select(onset) %>%
        dplyr::rename(Onset = onset) %>%
        dplyr::mutate(Type = "User")

      if(!no_stimulus_trigger_times) {
        stimulus_trigger_times_df <- stimulus_trigger_times_df %>%
          dplyr::rename(Onset = stimulus_trigger_times) %>%
          dplyr::mutate(Type = "Stimulus")

        onsets_df <- onsets_df %>%
          rbind(stimulus_trigger_times_df)

      }

      p <- ggplot2::ggplot() +
        ggplot2::geom_vline(ggplot2::aes(xintercept = Onset, color = Type), data = onsets_df) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank())


      pl <- shiny::renderPlot({ p }, width = 500)

      answer$pyin_style_res <- NULL
      answer$onsets_noteon_timecode <- NULL
      tab <- list_to_shiny_table(answer)

    } else {
      pl <- "There is nothing to plot. Did you tap?"
      tab <- shiny::tags$p("")
    }

    ui <- shiny::tags$div(
      shiny::tags$p(pl),
      if(is_plottable) tags$h3('Response Data'),
      tab)

    psychTestR::one_button_page(ui)


  })
}

