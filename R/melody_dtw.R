

plot_dtw_melody <- function(stimuli, stimuli_durations, pyin_smoothed_pitchtrack) {

  stimuli <- tibble::tibble(freq = c(hrep::midi_to_freq(stimuli), NA), dur = c(stimuli_durations, NA), onset = c(0, cumsum(stimuli_durations)))
  # stimuli <- stimuli %>%
  #   dplyr::add_row(onset = stimuli_durations[nrow(stimuli)]+stimuli_durations[nrow(stimuli)], dur = NA, freq = NA)


  plot <- ggplot2::ggplot(NULL, ggplot2::aes(x = onset, y = freq)) +
    ggplot2::geom_line(data = pyin_smoothed_pitchtrack, mapping = ggplot2::aes(x = onset, y = freq), size = .5, alpha = .6) +
    ggplot2::geom_segment(data = stimuli, mapping = ggplot2::aes(x = onset, xend = dplyr::lead(onset), y = freq, yend = freq), color = "blue", alpha = .7)

  rendered_plot <- shiny::renderPlot({ plot }, width = 500)
}

prepare_mel_trial_user_prod_for_dtw <- function(pyin_smoothed_pitchtrack, pyin_res) {

  # participant entry to dtw
  pyin_notes2 <- pyin_res %>% dplyr::select(-dur)

  res <- dplyr::full_join(pyin_notes2, pyin_smoothed_pitchtrack, by = "onset") %>% dplyr::rename(quantized_note = freq.x, freq =  freq.y) %>%
    dplyr::arrange(onset) %>% tidyr::fill(quantized_note)

  res$freq[!is.na(res$freq)]

}


prepare_mel_stimuli_for_dtw <- function(melody, durations) {

  melody <- hrep::midi_to_freq(melody)

  end <- durations[length(durations)]
  out <- data.frame(dur = cumsum(rep(.01, end/.01)))

  df <- data.frame(note = melody, dur = durations)

  df2 <- dplyr::full_join(df, out) %>% dplyr::arrange(dur) %>% tidyr::fill(note, .direction = "up")

  plot <- ggplot2::ggplot(df2) +
    ggplot2::geom_point(ggplot2::aes(x = dur, y = note), size = 0.2)


  df2$note
}


# get the resolution of pyin smoothed pitch track
#pyin_resolution <- round(mean(diff(d$onset), na.rm = TRUE), 2)
# .01

# d <- prepare_mel_trial_user_prod_for_dtw(hbd_pt, hbd_notes)



# stimuli entry to dtw
# df_row <- itembankr::Berkowitz("main") %>% dplyr::slice(200000)
# f <- hrep::midi_to_freq(itembankr::str_mel_to_vector(df_row %>% dplyr::pull(melody)) + 60)
# du <- cumsum(itembankr::str_mel_to_vector(df_row %>% dplyr::pull(durations)))
#
# stim_test_dtw <- prepare_mel_stimuli_for_dtw(f, du)
#
#
# dtw_res <- dtw::dtw(d$freq[!is.na(d$freq)], stim_test_dtw$note, keep = TRUE)
#
# dtw::dtwPlotTwoWay(dtw_res)
#
# dtw_res$normalizedDistance
