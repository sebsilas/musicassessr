

#' Get long note PCA scores
#'
#' @param long_note_accuracy
#' @param long_note_dtw_distance
#' @param long_note_autocorrelation_mean
#' @param long_note_run_test
#' @param long_note_no_cpts
#' @param long_note_beginning_of_second_cpt
#'
#' @return
#' @export
#'
#' @examples
get_long_note_pcas <- function(long_note_scores) {

  long_tone_summary <- long_note_scores %>%
    dplyr::mutate_if(is.character,as.numeric) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE)) %>%
    dplyr::mutate(long_note_accuracy = abs(long_note_accuracy)) %>%
    predict(long_note_pca2,
            data = .,
            old.data = long_note_agg %>%
              dplyr::select(long_note_accuracy, long_note_dtw_distance, long_note_autocorrelation_mean,
                            long_note_run_test, long_note_no_cpts, long_note_beginning_of_second_cpt)
            # you need to pass this for standardization or you will get NaNs
            # https://stackoverflow.com/questions/27534968/dimension-reduction-using-psychprincipal-does-not-work-for-smaller-data
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(pca_long_note_randomness = RC1,
                  pca_long_note_accuracy = RC2,
                  pca_long_note_scoop = RC3)
}








#' Compute long tone pitch metrics
#'
#' @param target_pitch A MIDI pitch.
#' @param freq The pyin frequency vector.
#'
#' @return
#' @export
#'
#' @examples
long_note_pitch_metrics <- function(target_pitch, freq) {

  if(!is_midi_note(target_pitch)) {
    warning('The target pitch was not in the MIDI pitch range (0-127). Are you using a frequency value in Hz by accident?')
  }


  ## dtw scoring
  ref <- itembankr::produce_arrhythmic_durations(length(freq), hrep::midi_to_freq(target_pitch))
  long_note_dtw_distance <- dtw::dtw(freq, ref)$distance

  # the note accuracy is the average deviation from the target note
  long_note_accuracy <- score_long_note_accuracy(target_pitch, freq)

  # change points
  cps <- calculate_stable_part(tibble::tibble(freq = freq))

  list(
    "long_note_accuracy" = long_note_accuracy,
    "long_note_var" = var(freq),
    "long_note_dtw_distance" = long_note_dtw_distance,
    "long_note_autocorrelation_mean" = mean(abs(stats::acf(freq,  na.action = stats::na.pass, plot = FALSE)$acf)),
    "long_note_run_test" = as.numeric(randtests::runs.test(freq)$statistic),
    "long_note_no_cpts" = if(is_na_length_1(cps)) NA else cps$no_cpts,
    "long_note_beginning_of_second_cpt" = if(is_na_length_1(cps)) NA else cps$beginning_of_second_cpt,
    "long_note_na_count" = sum(c(is.na(freq) | is.nan(freq))),
    "long_note_dtw_distance_max" = max(long_note_accuracy),
    "long_note_accuracy_max" = max(long_note_accuracy),
    "long_note_freq_max" = max(freq),
    "long_note_freq_min" = min(freq)
  )

}



# rewrite singing measures

score_long_note_accuracy <- function(target_pitch_midi, freqs_hz_by_user) {
  target_pitch_hz <- hrep::midi_to_freq(target_pitch_midi)
  cents_vector_in_rel_to_target_note <- itembankr::vector_cents(target_pitch_hz, freqs_hz_by_user)
  # sign must be preserved to not confound accuracy and precision (at the trial level):
  note_accuracy <- mean(cents_vector_in_rel_to_target_note)
}



#' Calculate metrics related to "changepoints" in a time series (e.g., for long note trials)
#' Input: df with "freq" column
#'
#' @param df
#' @param plot
#' @param return_df
#'
#' @return
#' @export
#'
#' @examples
calculate_stable_part <- function(df, plot = FALSE, return_df = FALSE) {

  tryCatch({
    ansmean <- changepoint::cpt.mean(df$freq, method = 'BinSeg')
    no_cpts <- length(ansmean@cpts)
    cpts <- ansmean@cpts
    last_change <- cpts[no_cpts]

    if(no_cpts > 1) {
      beginning_of_second_cpt <- cpts[2]
    } else {
      beginning_of_second_cpt <- NA
    }

    if(!is.na(last_change)) {
      df[1:(last_change-1), "stable"] <- FALSE
      df[last_change:nrow(df), "stable"] <- TRUE
    }

    if(plot) {
      p <- ggplot2::ggplot(df, aes(x = onset, y = freq)) +
        geom_line(aes(color = stable))

      print(p)
    }

    if(return_df) {
      list(no_cpts = no_cpts,
           beginning_of_second_cpt = beginning_of_second_cpt,
           df = df)

    } else {
      list(no_cpts = no_cpts,
           beginning_of_second_cpt = beginning_of_second_cpt)
    }
  }, error = function(err) {
    print(err)
    NA
  })

}



# Utils

score_cents_deviation_from_nearest_stimuli_pitch <- function(user_prod_pitches, stimuli, freq) {


  nearest_pitches <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli,
                                                                           user_production_pitches = user_prod_pitches,
                                                                           allOctaves = TRUE)

  res <- itembankr::vector_cents_between_two_vectors(freq, hrep::midi_to_freq(nearest_pitches))

  res <- mean(abs(res), na.rm = TRUE)
  res

}





get_all_octaves_in_gamut <- Vectorize(function(note, gamut_min = midi.gamut.min, gamut_max = midi.gamut.max) {

  stopifnot(length(note) == 1)

  # given a note and a range/gamut, find all midi octaves of that note within the specified range/gamut
  res <- c(note)

  # first go down
  while(note > gamut_min) {
    note <- note - 12
    res <- c(res, note)
  }
  # then go up
  while(note < gamut_max) {
    note <- note + 12
    res <- c(res, note)
  }
  res <- res[!duplicated(res)]
  res <- res[order(res)]
  res
})


#' Find the closest pitch(s) in a stimulus, to the notes a user produced
#'
#' @param stimuli_pitches
#' @param user_production_pitches
#' @param allOctaves
#'
#' @return
#' @export
#'
#' @examples
find_closest_stimuli_pitch_to_user_production_pitches <-
  function(stimuli_pitches, user_production_pitches, allOctaves = TRUE) {

    stimuli_pitches <- as.integer(stimuli_pitches)
    # if allOctaves is true, get the possible pitches in all other octaves. this should therefore resolve issues
    # where someone was presented stimuli out of their range and is penalised for it
    if (allOctaves) {
      stimuli_pitches_in_all_octaves <- as.integer(sort(as.vector(unlist(get_all_octaves_in_gamut(stimuli_pitches)))))
      res <- purrr::map_int(user_production_pitches, find_closest_value, vector = stimuli_pitches_in_all_octaves, return_value = TRUE)

    } else {
      res <- purrr::map_int(user_production_pitches, find_closest_value, vector = stimuli_pitches, return_value = TRUE)
    }
    res
  }



# dd <- calculate_stable_part(test_long_note_scoop, plot = FALSE)


# as <- get_all_octaves_in_gamut(41, midi.gamut.min, midi.gamut.max)
# as2 <- get_all_octaves_in_gamut(41:42, midi.gamut.min, midi.gamut.max) # should be stopped
# tt <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = c(60, 62, 65, 64), user_production_pitches = c(50, 60, 70, 80, 76))
# tt2 <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = c(60, 62, 65, 64), user_production_pitches = c(50, 60, 70, 80, 76), allOctaves = FALSE)

