
melody_scoring_from_user_input <- function(input,
                                           result = NULL,
                                           trial_type = "audio",
                                           user_melody_input = NULL,
                                           singing_measures = TRUE,
                                           pyin_pitch_track = NULL,
                                           stimuli = NA, stimuli_durations = NA) {


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
    stimuli_length <- get_stimuli_length(input)
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
         similarity = similarity,
         note_precision = note_precision,
         melody_dtw = ifelse(is.na(melody_dtw), NA, melody_dtw),
         mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
         mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
         answer_meta_data = input$answer_meta_data)
  }
}

produce_extra_melodic_features <- function(res) {
  res %>% dplyr::mutate(cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
                    # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
                    pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                    pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                    sci_notation = itembankr::midi_to_sci_notation(note),
                    interval = c(NA, diff(note)),
                    ioi = c(NA, diff(onset)),
                    ioi_class = itembankr::classify_duration(ioi))

}

get_stimuli_length <- function(input) {
  if(length(input$answer_meta_data) > 1) {
    stimuli_length <- input$answer_meta_data$N
  } else {
    stimuli_length <- 1
  }
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
    similarity <- NA
  } else {
    if(is.na(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }
    similarity <- itembankr::opti3(pitch_vec1 = stimuli,
                                   dur_vec1 = stimuli_durations,
                                   pitch_vec2 = user_melody_input,
                                   dur_vec2 = durations)
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
