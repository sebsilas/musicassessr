

plot_dtw_melody <- function(stimuli, stimuli_durations, pyin_smoothed_pitchtrack) {

  stimuli <- tibble::tibble(freq = c(hrep::midi_to_freq(stimuli), NA), dur = c(stimuli_durations, NA), onset = c(0, cumsum(stimuli_durations)))

  plot <- ggplot2::ggplot(NULL, ggplot2::aes(x = onset, y = freq)) +
    ggplot2::geom_line(data = pyin_smoothed_pitchtrack, mapping = ggplot2::aes(x = onset, y = freq), size = .5, alpha = .6) +
    ggplot2::geom_segment(data = stimuli, mapping = ggplot2::aes(x = onset, xend = dplyr::lead(onset), y = freq, yend = freq), color = "blue", alpha = .7)

  rendered_plot <- shiny::renderPlot({ plot }, width = 500)
}

prepare_mel_trial_user_prod_for_dtw <- function(pyin_smoothed_pitchtrack, pyin_res) {

  # participant entry to dtw
  pyin_notes <- pyin_res %>% dplyr::select(-dur)

  res <- dplyr::full_join(pyin_notes, pyin_smoothed_pitchtrack, by = "onset") %>% dplyr::rename(quantized_note = freq.x, freq =  freq.y) %>%
    dplyr::arrange(onset) %>% tidyr::fill(quantized_note)

  res$freq[!is.na(res$freq)]

}


prepare_mel_stimuli_for_dtw <- function(melody, durations) {

  if(is.na(durations)) {
    durations <- 5
  }
  melody <- hrep::midi_to_freq(melody)

  end <- durations[length(durations)]

  out <- data.frame(dur = cumsum(rep(.01, end/.01)))

  df <- data.frame(note = melody, dur = durations) %>%
    dplyr::full_join(out) %>%
    dplyr::arrange(dur) %>%
    tidyr::fill(note, .direction = "up")

  df$note
}
