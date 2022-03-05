# singing

score_cents_deviation_from_nearest_stimuli_pitch <- function(user_prod_pitches, stimuli, freq) {


  nearest_pitches <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli,
                                                                           user_production_pitches = user_prod_pitches,
                                                                           allOctaves = TRUE)

  res <- vector_cents_between_two_vectors(freq, hrep::midi_to_freq(nearest_pitches))

  res <- mean(abs(res), na.rm = TRUE)
  res

}

### long tone scoring


#' Get pitch metrics for long tone trials
#'
#' @param target_pitch
#' @param freq
#'
#' @return
#' @export
#'
#' @examples
long_note_pitch_metrics <- function(target_pitch, freq, state) {


  ## dtw scoring

  ref <- itembankr::produce_arrhythmic_durations(length(freq), hrep::midi_to_freq(target_pitch))
  dtw.distance <- dtw::dtw(freq, ref)$distance

  # note accuracy, interval accuracy, note precision, interval precision
  # see DOI: 10.1121/1.3478782

  cents_vector_in_rel_to_target_note <- vector.cents(target_pitch, freq)

  # the note accuracy is the average deviation from the target note
  note.accuracy <- mean(abs(cents_vector_in_rel_to_target_note))

  cents_vector_in_rel_to_mean <- vector_cents(mean(freq), freq)
  # the precision is the standard deviation of pitches not in relation to the target note
  # but instead in relation to whatever the mean was the the participant sang
  # in this long note context, then, this measures how stable the pitch was and has a slightly different meaning
  # note precision in the context of melody trials, which is the consistency of producing the same pitch classes on multiple occurences
  # note that note precision has no reference to target pitch and therefore thus independent of accuracy
  note.precision <- sqrt( sum(cents_vector_in_rel_to_mean^2)/length(freq) )

  # now grab the PCA version
  long_tone_holder_df <- tibble::tibble(note_accuracy = note.accuracy,
                                        note_precision = note.precision,
                                        dtw_distance = dtw.distance)


  agg_dv_long_note <- predict(musicassessr::long_note_pca, data = long_tone_holder_df, old.data = musicassessr::long_tone_dat_min) %>% as.vector()

  item_df <- tibble::tibble(stimuli = target_pitch, agg_dv_long_note = agg_dv_long_note, p_id = psychTestR::p_id(state))

  long_note_IRT <- predict(musicassessr::long_note_mod, newdata = item_df, re.form = NA) %>% as.vector() # predict without random fx


  list("note_accuracy" = note.accuracy,
       "note_precision" = note.precision,
       "dtw_distance" = dtw.distance,
       "agg_dv_long_note" = agg_dv_long_note,
       "long_note_IRT" = long_note_IRT)
}

get_note_precision <- function(pyin_result) {
  note_precision <- pyin_result %>%
    dplyr::group_by(sci_notation) %>%
    dplyr::summarise(sd_for_pitch_class = sd(freq, na.rm = TRUE),
                     participant_precision = mean(sd_for_pitch_class, na.rm = TRUE)) %>%
    dplyr::summarise(note_precision_melody = mean(participant_precision, na.rm = TRUE)) %>%
    dplyr::pull(note_precision_melody)
}



## low level

find_closest_value <- function(x, vector, return_value) {
  # given a value, x, and a vector of values,
  # return the index of the value in the vector, or the value itself, which is closest to x
  # if return_value == TRUE, return the value, otherwise the index
  res <- base::which.min(abs(vector - x))
  res <- ifelse(return_value == TRUE, vector[res], res)
}

#find_closest_value(14, c(1, 6, 12, 28, 33), TRUE)

get_all_octaves_in_gamut <- function(note, gamut_min = midi.gamut.min, gamut_max = midi.gamut.max) {

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
}


find_closest_stimuli_pitch_to_user_production_pitches <- function(stimuli_pitches, user_production_pitches, allOctaves = TRUE) {

  # if allOctaves is true, get the possible pitches in all other octaves. this should therefore resolve issues
  # where someone was presented stimuli out of their range and is penalised for it
  if (allOctaves == TRUE) {
    res <- sapply(user_production_pitches, find_closest_value, get_all_octaves_in_gamut(stimuli_pitches), return_value = TRUE)
  } else {
    res <- sapply(user_production_pitches, find_closest_value, stimuli_pitches, return_value = TRUE)
  }
  res
}


# and some singing accuracy metrics on read in
cents <- function(notea, noteb) {
  # get the cents between two notes (as frequencies)
  res <- 1200 * log2(noteb/notea)
  res
}

vector_cents <- function(reference_note, vector_of_values) {
  # given a vector of values and a target note, give the cents of the vector note relative to the target note
  res <- vapply(vector_of_values, cents, "notea" = reference_note, FUN.VALUE = 100.001)
  res
}

vector_cents_between_two_vectors <- function(vectora, vectorb) {
  # for each note (as a freq) in a vector, get the cents difference of each note in vector A and vector B
  res <- c()
  for (n in 1:length(vectora)) {
    cent_res <- cents(vectora[n], vectorb[n])
    res <- c(res, cent_res)
  }
  res
}



vector_cents_first_note <- function(vector_of_values) {
  # given a vector of frequencies, give the cents relative to the first note
  res <- vapply(vector_of_values, cents, "notea" = vector_of_values[1], FUN.VALUE = 100.001)
  res
}

#as <- get_all_octaves_in_gamut(41, midi.gamut.min, midi.gamut.max)

#as2 <- unlist(lapply(c(51, 39, 41, 43), function(x) get_all_octaves_in_gamut(x, midi.gamut.min, midi.gamut.max)))

