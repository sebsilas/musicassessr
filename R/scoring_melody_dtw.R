
#' Compute the dynamic time warp distance between a melodic stimuli and a user's continuous pitch curve, attempting to sing that stimuli
#'
#' @param stimuli
#' @param stimuli_durations
#' @param pyin_pitch_track
#' @param pyin_res
#'
#' @return
#' @export
#'
#' @examples
get_melody_dtw <- function(stimuli, stimuli_durations, pyin_pitch_track, pyin_res) {

  stopifnot(is.numeric(stimuli),
            is.numeric(stimuli_durations),
            is.data.frame(pyin_pitch_track),
            is.data.frame(pyin_res))

  if(length(pyin_pitch_track) > 1) {

    user_prod_for_dtw <- prepare_mel_trial_user_prod_for_dtw(pyin_pitch_track, pyin_res)
    stimuli_for_dtw <- prepare_mel_stimuli_for_dtw(stimuli, stimuli_durations)

    melody_dtw <- tryCatch({
      dtw::dtw(user_prod_for_dtw, stimuli_for_dtw, keep = TRUE)$distance
    },
    error = function(cond) {
      print(cond)
      return(NA)
    })
  } else {
    return(NA)
  }
}

#' Plot a melody after being transformed into a representation for use with the dynamic time warp algorithm
#'
#' @param stimuli
#' @param stimuli_durations
#' @param pyin_smoothed_pitchtrack
#'
#' @return
#' @export
#'
#' @examples
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

  res <- dplyr::full_join(pyin_notes, pyin_smoothed_pitchtrack, by = "onset") %>%
    dplyr::rename(quantized_note = freq.x, freq =  freq.y) %>%
    dplyr::arrange(onset) %>% tidyr::fill(quantized_note)

  res$freq[!is.na(res$freq)]

}



#' Convert a melody to a pseudo-timeseries representation
#'
#' @param melody
#' @param durations
#' @param convert_midi_to_freq
#'
#' @return
#' @export
#'
#' @examples
prepare_mel_stimuli_for_dtw <- function(melody, durations, convert_midi_to_freq = TRUE) {

  if(is.scalar.na(durations)) {
    warning('Manually setting durations to 5')
    durations <- 5
  }

  if(convert_midi_to_freq) {
    melody <- hrep::midi_to_freq(melody)
  }

  end <- durations[length(durations)]

  out <- data.frame(dur = cumsum(rep(.01, end/.01)))

  df <- data.frame(note = melody, dur = durations) %>%
    dplyr::full_join(out) %>%
    dplyr::arrange(dur) %>%
    tidyr::fill(note, .direction = "up")

  df$note
}

#
# x <- rnorm(100)
# plot(x)
# a <- acf(x)
# pacf(x)
# ar_mod <- ar(x)
#
# x2 <- readr::read_csv("/Users/sebsilas/true_vamp_pyin_pyin_smoothedpitchtrack.csv",
#                       col_names = c('onset', 'freq'))
#
#
# plot(x2$freq)
# ar_mod2 <- ar(x2$freq)
# ar_mod2 %>% broom::tidy()
#
# broom::tidy(ar_mod2)
#
#
# AR <- arima(x2$freq, order = c(1,0,0))
# print(AR)
#
# ts.plot(x2$freq)
# AR_fit <- x2$freq - residuals(AR)
# points(AR_fit, type = "l", col = 2, lty = 2)
# sos <- sum(AR$residuals^2)
#
#
# AR2 <- arima(x, order = c(1,0,0))
# print(AR2)
# AR2_fit <- x - residuals(AR2)
# points(AR2_fit, type = "l", col = 2, lty = 2)
#
#
# sos2 <- sum(AR2$residuals^2)

