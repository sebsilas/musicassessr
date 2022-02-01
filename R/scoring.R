
#' Score using melodic production measures
#'
#' @param user_melody_input
#' @param user_duration_input
#' @param user_onset_input
#' @param stimuli
#' @param stimuli_durations
#' @param pyin_pitch_track
#' @param user_response_midi_note_off
#' @param onsets_noteoff
#' @param answer_meta_data
#' @param as_tb
#'
#' @return
#' @export
#'
#' @examples
score_melodic_production <- function(user_melody_input = numeric(),
                                     user_duration_input = numeric(),
                                     user_onset_input = numeric(),
                                     stimuli = numeric(),
                                     stimuli_durations = numeric(),
                                     pyin_pitch_track = tibble::tibble(),
                                     user_response_midi_note_off = numeric(),
                                     onsets_noteoff = numeric(),
                                     answer_meta_data = tibble::tibble(),
                                     as_tb = FALSE) {
  # N.B; this should remain completely abstracted from psychTestR for post-hoc analyses
  stopifnot(
    is.numeric(user_melody_input), is.numeric(user_duration_input),
    is.numeric(user_onset_input), tibble::is_tibble(pyin_pitch_track) | is.na(pyin_pitch_track), is.numeric(stimuli),
    is.numeric(stimuli_durations), is.numeric(user_response_midi_note_off) | is.na(user_response_midi_note_off),
    is.numeric(onsets_noteoff) | is.na(onsets_noteoff), tibble::is_tibble(answer_meta_data) | is.na(answer_meta_data),
    is.logical(as_tb)
  )

  # features df
  features_df <- tibble::tibble(
    freq = hrep::midi_to_freq(user_melody_input),
    note = user_melody_input,
    onset = user_onset_input,
    dur = user_duration_input
  ) %>% produce_extra_melodic_features()

  # stimuli
  stimuli_length <- length(stimuli)

  # calculate measures
  trial_length <- user_onset_input[length(user_onset_input)]
  # octave dependent
  no_correct <- sum(as.numeric(user_melody_input %in% stimuli))
  no_errors <- length(user_melody_input) - no_correct
  errors_boolean <- as.vector(!user_melody_input %in% stimuli)
  # octaves independent (i.e octave errors allowed)
  user_pitch_classes <- itembankr::midi_to_pitch_class(user_melody_input) # pitch_class allows octave errors
  stimuli_pitch_classes <- itembankr::midi_to_pitch_class(stimuli)
  correct_boolean_octaves_allowed <- as.vector(user_pitch_classes %in% stimuli_pitch_classes)
  errors_boolean_octaves_allowed <- as.vector(!user_pitch_classes %in% stimuli_pitch_classes)
  no_correct_octaves_allowed <- sum(correct_boolean_octaves_allowed)
  no_errors_octaves_allowed <- sum(errors_boolean_octaves_allowed)


  # opti3
  opti3 <- get_opti3(stimuli, stimuli_durations, stimuli_length, features_df)
  no_note_events <- length(user_melody_input)

  # accuracy style measures

  ## proportion of correct note events i.e., no_correct/no_note_events
  proportion_of_correct_note_events <- get_proportion_of_correct_note_events(stimuli, user_melody_input, no_correct, no_errors, no_note_events)
  proportion_of_correct_note_events_octaves_allowed <- get_proportion_of_correct_note_events(stimuli, user_melody_input, no_correct_octaves_allowed, no_errors_octaves_allowed, no_note_events)

  ## controlled for stimuli length and [0,1]

  proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal <- proportion_of_correct_note_events * log_normal(no_note_events/stimuli_length)
  proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal <- proportion_of_correct_note_events_octaves_allowed * log_normal(no_note_events/stimuli_length)


  # proportion of stimuli (target) notes found
  proportion_of_stimuli_notes_found <- length(base::intersect(user_melody_input, stimuli))/stimuli_length
  proportion_of_stimuli_notes_found_octaves_allowed <- length(base::intersect(user_pitch_classes, stimuli_pitch_classes))/stimuli_length


  # fine-grained pitch measures (i.e singing style):
  note_precision <- get_note_precision(features_df)
  mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = features_df$note, stimuli = stimuli, freq = features_df$freq)
  mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(features_df$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)
  melody_dtw <- get_melody_dtw(stimuli, stimuli_durations, pyin_pitch_track, features_df)

  res <- list(
    stimuli = stimuli,
    stimuli_durations = stimuli_durations,
    stimuli_length = stimuli_length,
    user_response_note = user_melody_input,
    user_response_note_summary = as.list(round(summary(user_melody_input))),
    user_response_midi_note_off = user_response_midi_note_off,
    pyin_pitch_track = pyin_pitch_track,
    durations = user_duration_input,
    user_response_pitch_classes = user_pitch_classes,
    onsets_noteon = user_onset_input,
    onsets_noteoff = onsets_noteoff,
    trial_length = trial_length,
    no_note_events = no_note_events,
    no_correct = no_correct,
    no_errors = no_errors,
    errors_boolean = errors_boolean,
    correct_boolean_octaves_allowed = correct_boolean_octaves_allowed,
    errors_boolean_octaves_allowed = errors_boolean_octaves_allowed,
    no_correct_octaves_allowed = no_correct_octaves_allowed,
    no_errors_octaves_allowed = no_errors_octaves_allowed,
    proportion_of_correct_note_events = proportion_of_correct_note_events,
    proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal = proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal,
    proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal = proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal,
    proportion_of_stimuli_notes_found = proportion_of_stimuli_notes_found,
    proportion_of_stimuli_notes_found_octaves_allowed = proportion_of_stimuli_notes_found_octaves_allowed,
    opti3 = opti3$opti3,
    ngrukkon = opti3$ngrukkon,
    harmcore = opti3$harmcore,
    rhythfuzz = opti3$rhythfuzz,
    note_precision = note_precision,
    melody_dtw = ifelse(is.na(melody_dtw), NA, melody_dtw),
    mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
    mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
    answer_meta_data = answer_meta_data)

  if(as_tb) {
    tibble::as_tibble(base::t(res))
  } else {
    res
  }


}



# (t <- score_melodic_production(user_melody_input = c(60, 62, 64, 65),
#                                user_duration_input = rep(1, 4),
#                                user_onset_input = c(0, cumsum(rep(1, 3))),
#                                stimuli = c(60, 62, 64, 65),
#                                stimuli_duration = rep(1, 4)))
#
# (t2 <- score_melodic_production(user_melody_input = c(60, 67, 64, 65),
#                                 user_duration_input = rep(1, 4),
#                                 user_onset_input = c(0, cumsum(rep(1, 3))),
#                                 stimuli = c(60, 62, 64, 65),
#                                 stimuli_duration = rep(1, 4)))
#
#
# (t3 <- score_melodic_production(user_melody_input = c(60, 67, 64, 65, 60, 60),
#                                 user_duration_input = rep(1, 6),
#                                 user_onset_input = c(0, cumsum(rep(1, 5))),
#                                 stimuli = c(60, 62, 64, 65),
#                                 stimuli_duration = rep(1, 4)))
#
# # octave
#
# (t4 <- score_melodic_production(user_melody_input = c(72, 62, 64, 65),
#                                user_duration_input = rep(1, 4),
#                                user_onset_input = c(0, cumsum(rep(1, 3))),
#                                stimuli = c(60, 62, 64, 65),
#                                stimuli_duration = rep(1, 4)))


#' Produce extra melodic features from a pyin note track
#'
#' @param pyin_style_res
#'
#' @return
#' @export
#'
#' @examples
produce_extra_melodic_features <- function(pyin_style_res) {

  if(!"note" %in% names(pyin_style_res) & "freq" %in% names(pyin_style_res)) {
    pyin_style_res <- pyin_style_res %>%
      dplyr::mutate(note = round(hrep::freq_to_midi(freq)))
  }

  pyin_style_res <- pyin_style_res %>%
    dplyr::mutate(
      sci_notation = itembankr::midi_to_sci_notation(note),
      interval = c(NA, diff(note)),
      ioi = c(NA, diff(onset)),
      ioi_class = classify_duration(ioi)) %>%
        segment_phrase()

  if(!"freq" %in% names(pyin_style_res)) {
    pyin_style_res <- pyin_style_res %>% dplyr::mutate(freq = hrep::midi_to_freq(note))
  }

  pyin_style_res %>% dplyr::mutate(
    cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
    # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
    pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
    pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq)))
  )

}

# helper functions / mainly for dealing with presence of NAs when scoring methods used at test time

get_proportion_of_correct_note_events <- function(stimuli, user_melody_input, no_correct, no_errors, no_note_events) {
  if (no_errors == 0 & no_correct == length(stimuli)) {
    proportion_of_correct_note_events  <- 1
  } else {
    proportion_of_correct_note_events <- no_correct/no_note_events
  }
}

get_durations <- function(result) {
  if(is.null(result$dur)) {
    durations <- diff(onsets_noteon)
  } else {
    durations <- result$dur
  }
}


get_opti3 <- function(stimuli, stimuli_durations = NA, stimuli_length, user_input_as_pyin) {
  # opti3
  if(length(user_input_as_pyin$note) < 3 | stimuli_length < 3) {
    list(opti3 = NA, ngrukkon = NA, rhythfuzz = NA, harmcore = NA)
  } else {
    if(is.na(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }

    stimuli_df <- tibble::tibble(
      note = stimuli,
      dur = stimuli_durations,
      onset = cumsum(stimuli_durations),
      ioi = c(NA, diff(onset)),
      ioi_class = classify_duration(ioi)
    ) %>% segment_phrase()

    opti3 <- opti3_df(melody1 = stimuli_df,
                     melody2 = user_input_as_pyin)
  }
}


reverse_answer <- function(ans) {
  # if reverse == TRUE, then check for the reverse of the answer
  if (length(ans) == 1) {
    ans <- stringi::stri_reverse(ans)
  } else {
    ans <- rev(ans)
  }
  ans
}

check_answer <- function(user_response, correct_answer,
                         reverse = FALSE,
                         type = c("single_digits", "numbers", "chunks_numbers",
                                  "single_letters", "chunk_letters",
                                  "words",
                                  "midi_pitches",
                                  "rhythms",
                                  "melodies_with_rhythms")
) {

  # check that the lengths are the same
  stopifnot(length(user_response) == length(correct_answer))

  # reverse answer if need be
  if (reverse == TRUE) {
    correct_answer <- reverse_answer(correct_answer)
  }
  # match based on type of input
  if (length(user_response) > 1) {
    correct <- identical(user_response, correct_answer)
  } else if (type == "midi_pitches") {
    # itembankr::ngrukkon()
  } else if (type == "rhythms") {
    ## rhythm metric
    # itembankr::rhythfuzz()
  } else if (type == "melodies_with_rhythms") {

  }

  else {
    correct <- user_response == correct_answer
  }
  correct
}

check_opti3 <- function(user_response, correct_answer, reverse = FALSE) {
  # if reverse == TRUE, then check for the reverse of the answer

  if (reverse == TRUE) {
    #correct <-
  }
  else {
    #user_response ==
  }
}

# tests
# check_answer("a", "b", type = "single_letters")
# check_answer("a", "a", type = "single_letters")
# check_answer("aaa", "bbb", reverse = FALSE, type = "chunk_letters")
# check_answer("aaa", "aaa", type = "chunk_letters")
# check_answer(11, 22, type = "single_numbers")
# check_answer(11, 11, type = "single_numbers")
# check_answer(11, 11, reverse = TRUE,  type = "chunk_digits")
# check_answer(123, 123, reverse = TRUE, type = "chunk_digits")
# check_answer(123, 321, reverse = TRUE, type = "chunk_digits")
#
# check_answer(1:3, 3:1, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 3:1, reverse = FALSE, type = "chunk_digits")
# check_answer(1:3, 1:3, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 1:4, reverse = TRUE, type = "chunk_digits")
#
# check_answer(c(10,50,60), c(10,50,60), type = "midi_pitches")
# check_answer(c(10,50,60), c(10,50,60), reverse = TRUE, type = "midi_pitches")
# check_answer(c(10,50,60), c(60,50,10), reverse = TRUE, type = "midi_pitches")
#


# t <- itembankr::midi_file_to_notes_and_durations('/Users/sebsilas/true.mid', string_df = FALSE)
# t2 <- itembankr::midi_file_to_notes_and_durations('/Users/sebsilas/true.mid', string_df = TRUE, produce_extra_melodic_features = TRUE)
# t3 <- t2 %>% to_string_df()



# t <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
#                          user_duration_input = c(1, 1.5, 1, 1.2, 1),
#                          user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
#                          stimuli = c(60, 62, 63, 64, 65),
#                          stimuli_durations = c(1, 1, 1, 1, 1))
