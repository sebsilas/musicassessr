

# Trial level



score_melody_note_accuracy <- function(user_prod_pitches, stimuli, freq, nearest_pitches) {

  # for each user frequency, compute the distance, in cents, to the nearest stimuli pitch

  res <- itembankr::vector_cents_between_two_vectors(freq, hrep::midi_to_freq(nearest_pitches))

  mean(res, na.rm = TRUE)

}




score_melody_interval_accuracy <- function(sung_interval,
                                           sung_interval_cents,
                                           stimuli_interval) {

  if (all( c(!is.na(sung_interval), !is.na(sung_interval_cents), is.na(stimuli_interval) ) ) ) {


    sung_interval_cents <- sung_interval_cents[2:length(sung_interval_cents)] %>% abs()
    stimuli_interval <- stimuli_interval[2:length(stimuli_interval)] %>% abs()

    nearest_intervals <-
      find_closest_stimuli_pitch_to_user_production_pitches(
        stimuli_pitches = stimuli_interval,
        user_production_pitches = sung_interval[!is.na(sung_interval)],
        allOctaves = TRUE)

    r <- purrr::map2_dbl(sung_interval_cents,nearest_intervals*100, # *100 = to cents
                         function(s, t) abs(s) - abs(t))

    res <- mean(r, na.rm = TRUE)

  } else {
    res <- NA
  }
  res
}

# Test level




#' Score melody note precision
#'
#' @param all_participant_trials
#'
#' @return
#' @export
#'
#' @examples
score_melody_note_precision <- function(all_participant_trials) {
  tibble::tibble(freq = unlist(all_participant_trials$freq),
         note = unlist(all_participant_trials$nearest_stimuli_note)) %>%
    dplyr::mutate(pitch_class = itembankr::midi_to_sci_notation(note)) %>%
    dplyr::group_by(pitch_class) %>%
    dplyr::summarise(sd = sd(cents(440, freq), na.rm = TRUE)) %>%
    dplyr::pull(sd) %>%
    mean(na.rm = TRUE)
}



#' Score melody interval precision
#'
#' @param all_participant_trials
#'
#' @return
#' @export
#'
#' @examples
score_melody_interval_precision <- function(all_participant_trials) {
  tibble::tibble(interval = unlist(all_participant_trials$interval),
         sung_interval_cents = unlist(all_participant_trials$interval_cents)) %>%
    dplyr::group_by(interval) %>%
    dplyr::summarise(sd = sd(sung_interval_cents, na.rm = TRUE)) %>%
    dplyr::pull(sd) %>%
    mean(na.rm = TRUE)
}



# Utils


find_closest_value <- function(x, vector, return_value) {

  # given a value, x, and a vector of values,
  # return the index of the value in the vector, or the value itself, which is closest to x
  # if return_value == TRUE, return the value, otherwise the index
  res <- base::which.min(abs(vector - x))
  res <- ifelse(return_value == TRUE, vector[res], res)
}

