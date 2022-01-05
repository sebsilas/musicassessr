
#' Score using melodic production measures
#'
#' @param user_melody_input
#' @param user_duration_input
#' @param user_onset_input
#' @param stimuli
#' @param stimuli_durations
#' @param singing_measures
#' @param pyin_pitch_track
#' @param user_response_midi_note_off
#' @param onsets_noteoff
#' @param answer_meta_data
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
                                     answer_meta_data = tibble::tibble()
                                     ) {

  stopifnot(
    is.numeric(user_melody_input), is.numeric(user_duration_input),
    is.numeric(user_onset_input), is.logical(singing_measures),
    tibble::is_tibble(pyin_pitch_track) | is.na(pyin_pitch_track), is.numeric(stimuli),
    is.numeric(stimuli_durations), is.numeric(user_response_midi_note_off) | is.na(user_response_midi_note_off),
    is.numeric(onsets_noteoff) | is.na(onsets_noteoff), tibble::is_tibble(answer_meta_data) | is.na(answer_meta_data)
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
  # accuracy
  accuracy <- get_note_accuracy(stimuli, user_melody_input, no_correct, no_errors)
  accuracy_octaves_allowed <- get_note_accuracy(stimuli, user_melody_input, no_correct_octaves_allowed, no_errors_octaves_allowed)

  # similarity
  similarity <- get_similarity(stimuli, stimuli_length, user_melody_input, user_duration_input, stimuli_durations)
  no_note_events <- length(user_melody_input)

  # by note events measures
  correct_by_note_events <- no_correct/no_note_events
  correct_by_note_events_log_normal <- correct_by_note_events * log_normal(no_note_events/stimuli_length)

  correct_by_note_events_octaves_allowed <- no_correct_octaves_allowed/no_note_events
  correct_by_note_events_octaves_allowed_log_normal <- correct_by_note_events_octaves_allowed * log_normal(no_note_events/stimuli_length)

  # fine-grained pitch measures (i.e singing style):

  # note precision
  note_precision <- get_note_precision(features_df)
  # cents_deviation_from_nearest_stimuli_pitch
  mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = features_df$note,                                                                                                stimuli = stimuli, freq = features_df$freq)
  # mean cents deviation
  mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(features_df$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)
  # melody dtw
  melody_dtw <- get_melody_dtw(stimuli, stimuli_durations, pyin_pitch_track, features_df)


  list(
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
    correct_by_note_events = correct_by_note_events,
    correct_by_note_events_log_normal = correct_by_note_events_log_normal,
    correct_by_note_events_octaves_allowed = correct_by_note_events_octaves_allowed,
    correct_by_note_events_octaves_allowed_log_normal = correct_by_note_events_octaves_allowed_log_normal,
    accuracy = accuracy,
    accuracy_octaves_allowed = accuracy_octaves_allowed,
    similarity = similarity$opti3,
    ngrukkon = similarity$ngrukkon,
    harmcore = similarity$harmcore,
    rhythfuzz = similarity$rhythfuzz,
    note_precision = note_precision,
    melody_dtw = ifelse(is.na(melody_dtw), NA, melody_dtw),
    mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
    mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
    answer_meta_data = answer_meta_data)

}


#' Score melodic production measures
#'
#' @param input
#' @param result
#' @param trial_type
#' @param user_melody_input
#' @param singing_measures
#' @param pyin_pitch_track
#' @param stimuli
#' @param stimuli_durations
#'
#' @return
#' @export
#'
#' @examples
melody_scoring_from_user_input <- function(input,
                                           result = NULL,
                                           trial_type = "audio",
                                           user_melody_input = NULL,
                                           singing_measures = TRUE,
                                           pyin_pitch_track = NULL,
                                           stimuli = NA,
                                           stimuli_durations = NA) {


  # grab midi related stuff
  if(trial_type == "midi") {
    user_response_midi_note_off <- rjson::fromJSON(input$user_response_midi_note_off)
    onsets_noteon <- rjson::fromJSON(input$onsets_noteon)
    onsets_noteoff <- rjson::fromJSON(input$onsets_noteoff)
  }
  else {
    if(is.numeric(result$freq)) {
      onsets_noteoff <- NA
      user_response_midi_note_off <- NA
      result <- produce_extra_melodic_features(result)
      user_melody_input <- result$note
      onsets_noteon <- result$onset

    } else {
      print("Bad Result.")
      return(NA)
    }

  }

  if(length(user_melody_input) == 0) {
    if(trial_type == "midi") { shiny::showNotification(psychTestR::i18n("nothing_entered")) }
    return(list(user_melody_input = NA, reason = "nothing_entered"))
  }

  else {

    # grab data
    stimuli_length <- length(stimuli)
    durations <- get_durations(result)
    # calculate measures
    trial_length <- onsets_noteon[length(onsets_noteon)]
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
    # accuracy
    accuracy <- get_note_accuracy(stimuli, user_melody_input, no_correct, no_errors)
    accuracy_octaves_allowed <- get_note_accuracy(stimuli, user_melody_input, no_correct_octaves_allowed, no_errors_octaves_allowed)

    # similarity
    similarity <- get_similarity(stimuli, stimuli_length, user_melody_input, durations, stimuli_durations)
    no_note_events <- length(user_melody_input)

    # by note events measures
    correct_by_note_events <- no_correct/no_note_events
    correct_by_note_events_log_normal <- correct_by_note_events * log_normal(no_note_events/stimuli_length)

    correct_by_note_events_octaves_allowed <- no_correct_octaves_allowed/no_note_events
    correct_by_note_events_octaves_allowed_log_normal <- correct_by_note_events_octaves_allowed * log_normal(no_note_events/stimuli_length)

    if(singing_measures) {

      # note precision
      note_precision <- get_note_precision(result)

      # cents_deviation_from_nearest_stimuli_pitch
      mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = result$note,
                                                                                                          stimuli = stimuli, freq = result$freq)
      # mean cents deviation
      mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(result$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)

      # melody dtw
      melody_dtw <- get_melody_dtw(stimuli, stimuli_durations, pyin_pitch_track, result)


    } else {
      note_precision <- NA
      mean_cents_deviation_from_nearest_stimuli_pitch <- NA
      mean_cents_deviation_from_nearest_midi_pitch <- NA
      melody_dtw <- NA
    }

    list(stimuli = stimuli,
         stimuli_durations = stimuli_durations,
         stimuli_length = stimuli_length,
         user_response_note = user_melody_input,
         user_response_note_summary = as.list(round(summary(user_melody_input))),
         user_response_midi_note_off = user_response_midi_note_off,
         pyin_pitch_track = pyin_pitch_track,
         durations = durations,
         user_response_pitch_classes = user_pitch_classes,
         onsets_noteon = onsets_noteon,
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
         correct_by_note_events = correct_by_note_events,
         correct_by_note_events_log_normal = correct_by_note_events_log_normal,
         correct_by_note_events_octaves_allowed = correct_by_note_events_octaves_allowed,
         correct_by_note_events_octaves_allowed_log_normal = correct_by_note_events_octaves_allowed_log_normal,
         accuracy = accuracy,
         accuracy_octaves_allowed = accuracy_octaves_allowed,
         similarity = similarity$opti3,
         ngrukkon = similarity$ngrukkon,
         harmcore = similarity$harmcore,
         rhythfuzz = similarity$rhythfuzz,
         note_precision = note_precision,
         melody_dtw = ifelse(is.na(melody_dtw), NA, melody_dtw),
         mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
         mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
         answer_meta_data = input$answer_meta_data)
  }
}

#' Produce extra melodic features from a pyin note track
#'
#' @param pyin_style_res
#'
#' @return
#' @export
#'
#' @examples
produce_extra_melodic_features <- function(pyin_style_res) {
  pyin_style_res %>% dplyr::mutate(
                    pitch = round(hrep::freq_to_midi(freq)),
                    cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
                    # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
                    pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                    pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                    sci_notation = itembankr::midi_to_sci_notation(note),
                    interval = c(NA, diff(note)),
                    ioi = c(NA, diff(onset)),
                    ioi_class = itembankr::classify_duration(ioi))

}

get_durations <- function(result) {
  if(is.null(result$dur)) {
    durations <- diff(onsets_noteon)
  } else {
    durations <- result$dur
  }
}

get_note_accuracy <- function(stimuli, user_melody_input, no_correct, no_errors) {
  # accuracy
  if (no_errors == 0 & no_correct == length(stimuli)) {
    accuracy <- 1
  } else {
    accuracy <- no_correct/length(user_melody_input)
  }
}

get_similarity <- function(stimuli, stimuli_length, user_melody_input, durations, stimuli_durations = NA) {
  # similarity
  if(length(user_melody_input) < 3 | stimuli_length < 3) {
    list(opti3 = NA, ngrukkon = NA, rhythfuzz = NA, harmcore = NA)
  } else {
    if(is.na(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }
    similarity <- itembankr::opti3(pitch_vec1 = stimuli,
                                   dur_vec1 = stimuli_durations,
                                   pitch_vec2 = user_melody_input,
                                   dur_vec2 = durations,
                                   return_components = TRUE)
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
    # itembankr::ngrukkon()
  } else if (type == "melodies_with_rhythms") {

  }

  else {
    correct <- user_response == correct_answer
  }


  correct
}

check_similarity <- function(user_response, correct_answer, reverse = FALSE) {
  # if reverse == TRUE, then check for the reverse of the answer

  if (reverse == TRUE) {
    #correct <-
  }
  else {
    #user_response ==
  }
}

log_normal <- function(x, a = 1) exp(-(log(x)/a)^2)