

#' Score using melodic production measures
#'
#' @param user_melody_freq Can be NULL for MIDI.
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
#' @param additional_scoring_measures
#'
#' @return
#' @export
#'
#' @examples
score_melodic_production <- function(user_melody_freq = numeric(),
                                     user_melody_input = numeric(),
                                     user_duration_input = numeric(),
                                     user_onset_input = numeric(),
                                     stimuli = numeric(),
                                     stimuli_durations = numeric(),
                                     pyin_pitch_track = tibble::tibble(),
                                     user_response_midi_note_off = numeric(),
                                     onsets_noteoff = numeric(),
                                     answer_meta_data = tibble::tibble(),
                                     as_tb = FALSE,
                                     additional_scoring_measures = NULL) {
  # N.B; this should remain completely abstracted from psychTestR for post-hoc analyses
  stopifnot(
    is.numeric(user_melody_freq),
    is.numeric(user_melody_input),
    is.numeric(user_duration_input),
    is.numeric(user_onset_input),
    is.numeric(stimuli),
    is.numeric(stimuli_durations),
    tibble::is_tibble(pyin_pitch_track) | is.na(pyin_pitch_track),
    is.numeric(user_response_midi_note_off) | is.na(user_response_midi_note_off),
    is.numeric(onsets_noteoff) | is.na(onsets_noteoff),
    tibble::is_tibble(answer_meta_data) | is.na(answer_meta_data),
    is.logical(as_tb),
    is.null(additional_scoring_measures) | is.function(additional_scoring_measures) | is.list(additional_scoring_measures)
  )

  # features df
  features_df <- tibble::tibble(
    freq = if(is_null_length_1(user_melody_freq)) hrep::midi_to_freq(user_melody_input) else user_melody_freq,
    note = user_melody_input,
    onset = user_onset_input,
    dur = user_duration_input
  ) %>% itembankr::produce_extra_melodic_features()


  # stimuli
  stimuli_length <- length(stimuli)

  # calculate measures
  trial_length <- user_onset_input[length(user_onset_input)]
  # octave dependent
  no_correct <- sum(as.numeric(user_melody_input %in% stimuli))
  no_errors <- length(user_melody_input) - no_correct
  errors_boolean <- as.vector(!user_melody_input %in% stimuli)
  # octave independent (i.e octave errors allowed)
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
  nearest_pitches <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli, user_production_pitches = features_df$note, allOctaves = TRUE)

  if(!"note" %in% names(pyin_pitch_track)) {
    pyin_pitch_track <- pyin_pitch_track %>%
      dplyr::mutate(note = dplyr::case_when(is.na(freq) ~ NA, TRUE ~ round(hrep::freq_to_midi(freq))))
  }

  nearest_pitches_pyin_track <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli, user_production_pitches = pyin_pitch_track$note, allOctaves = TRUE)
  mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = features_df$note, stimuli = stimuli, freq = features_df$freq)
  mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(features_df$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)
  melody_dtw <- get_melody_dtw(stimuli, stimuli_durations, pyin_pitch_track, features_df)
  features_df$nearest_stimuli_note <- nearest_pitches

  pyin_pitch_track <- pyin_pitch_track %>%
    dplyr::mutate(interval = c(NA, diff(note)),
                  interval_cents = itembankr::cents(dplyr::lag(freq), freq),
                  nearest_stimuli_note = nearest_pitches_pyin_track)

  # additional (user-defined)
  additional_scoring_measures <- apply_additional_scoring_measures(additional_scoring_measures, features_df$onset, features_df$dur, features_df$freq, features_df$note, stimuli, stimuli_durations)

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
    proportion_of_correct_note_events_octaves_allowed = proportion_of_correct_note_events_octaves_allowed,
    proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal = proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal,
    proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal = proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal,
    proportion_of_stimuli_notes_found = proportion_of_stimuli_notes_found,
    proportion_of_stimuli_notes_found_octaves_allowed = proportion_of_stimuli_notes_found_octaves_allowed,
    opti3 = opti3$opti3,
    ngrukkon = opti3$ngrukkon,
    harmcore = opti3$harmcore,
    rhythfuzz = opti3$rhythfuzz,
    melody_dtw = melody_dtw,
    mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
    mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
    answer_meta_data = answer_meta_data,
    additional_scoring_measures = additional_scoring_measures,
    production = features_df,
    melody_note_accuracy = score_melody_note_accuracy(user_melody_input, stimuli, user_melody_freq, nearest_pitches),
    melody_interval_accuracy = score_melody_interval_accuracy(features_df$interval, features_df$interval_cents, diff(stimuli))
    )

  if(as_tb) {
    tibble::as_tibble(base::t(res))
  } else {
    res
  }


}


apply_additional_scoring_measures <- function(additional_scoring_measures, onset, dur, freq, note, stimuli, stimuli_durations) {


  stopifnot(is.null(additional_scoring_measures) |
              is.function(additional_scoring_measures) |
              is.list(additional_scoring_measures))

  if(is.null(additional_scoring_measures)) {
    return(NA)
  } else {
    if(is.list(additional_scoring_measures)) {
      additional_scores <- purrr::map(additional_scoring_measures, function(scoring_fun) {
        check_additional_measures_args(scoring_fun)
        scoring_fun(onset, dur, freq, note, stimuli, stimuli_durations)
      })
    } else if(is.function(additional_scoring_measures)) {
      check_additional_measures_args(additional_scoring_measures)
      additional_scores <- additional_scoring_measures(onset, dur, freq, note, stimuli, stimuli_durations)
    } else {
      stop('additional_scoring_measures must be function or list.')
    }
  }
}

check_additional_measures_args <- function(fun) {

  req_args <- c("onset", "dur", "freq", "note", "stimuli", "stimuli_durations")

  cmp <- setdiff(formalArgs(fun), req_args)

  # test

  if(length(cmp) == 0) {
    TRUE
  } else {
    stop(paste0("The req args are ", paste0(req_args, collapse = " "), collapse = " "))
  }
}

test_additional_measures_fun_success <- function(onset, dur, freq, note, stimuli, stimuli_durations) {
  #
}

test_additional_measures_fun_failure <- function(onset, dur, freq2, note, stimuli, stimuli_durations) {
  # freq is misspelled
}

# check_additional_measures_args(test_additional_measures_fun_success)
# check_additional_measures_args(test_additional_measures_fun_failure)


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




# helper functions / mainly for dealing with presence of NAs when scoring methods used at test time

get_proportion_of_correct_note_events <- function(stimuli, user_melody_input, no_correct, no_errors, no_note_events) {
  if (no_errors == 0 & no_correct == length(stimuli)) {
    proportion_of_correct_note_events  <- 1
  } else {
    proportion_of_correct_note_events <- no_correct/no_note_events
  }
}

get_durations <- function(result) {
  if(is_null_length_1(result$dur)) {
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
    if(is_na_length_1(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }

    stimuli_df <- tibble::tibble(
      note = stimuli,
      dur = stimuli_durations,
      onset = cumsum(stimuli_durations),
      ioi = c(NA, diff(onset)),
      ioi_class = itembankr::classify_duration(ioi)
    ) %>% itembankr::segment_phrase()

    opti3 <- opti3_df(melody1 = stimuli_df,
                      melody2 = user_input_as_pyin)
  }
}








# t <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
#                          user_duration_input = c(1, 1.5, 1, 1.2, 1),
#                          user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
#                          stimuli = c(60, 62, 63, 64, 65),
#                          stimuli_durations = c(1, 1, 1, 1, 1))


# with additional measures





# t1 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
#                          user_duration_input = c(1, 1.5, 1, 1.2, 1),
#                          user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
#                          stimuli = c(60, 62, 63, 64, 65),
#                          stimuli_durations = c(1, 1, 1, 1, 1),
#                          additional_scoring_measures = log_scores)
#
# t2 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
#                                user_duration_input = c(1, 1.5, 1, 1.2, 1),
#                                user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
#                                stimuli = c(60, 62, 63, 64, 65),
#                                stimuli_durations = c(1, 1, 1, 1, 1),
#                                additional_scoring_measures = check_no_notes_above_c4)

# t3 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
#                                user_duration_input = c(1, 1.5, 1, 1.2, 1),
#                                user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
#                                stimuli = c(60, 62, 63, 64, 65),
#                                stimuli_durations = c(1, 1, 1, 1, 1),
#                                additional_scoring_measures = list(log_scores, check_no_notes_above_c4))

