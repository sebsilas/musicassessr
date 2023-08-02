

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
#' @param with_pmi
#' @param compute_accuracy_measures_aligned
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
                                     additional_scoring_measures = NULL,
                                     with_pmi = FALSE,
                                     compute_accuracy_measures_aligned = TRUE) {


  # N.B; this should remain completely abstracted from psychTestR for post-hoc analyses
  stopifnot(
    is.numeric(user_melody_freq),
    is.numeric(user_melody_input),
    is.numeric(user_duration_input),
    is.numeric(user_onset_input),
    is.numeric(stimuli), !is.na(stimuli),
    is.numeric(stimuli_durations),
    tibble::is_tibble(pyin_pitch_track) | is.na(pyin_pitch_track),
    is.numeric(user_response_midi_note_off) | is.na(user_response_midi_note_off),
    is.numeric(onsets_noteoff) | is.na(onsets_noteoff),
    tibble::is_tibble(answer_meta_data) | is.na(answer_meta_data),
    is.scalar.logical(as_tb),
    is.null(additional_scoring_measures) | is.function(additional_scoring_measures) | is.list(additional_scoring_measures),
    is.scalar.logical(compute_accuracy_measures_aligned)
  )

  # features df
  features_df <- tibble::tibble(
    freq = if(length(user_melody_freq) == 0) hrep::midi_to_freq(user_melody_input) else user_melody_freq,
    note = user_melody_input,
    onset = user_onset_input,
    dur = user_duration_input
  ) %>% itembankr::produce_extra_melodic_features()



  # Calculate measures

  # Stimuli and trial length
  stimuli_length <- length(stimuli)
  trial_length <- user_onset_input[length(user_onset_input)]

  # Octave-dependent
  no_correct_notes <- sum(as.numeric(user_melody_input %in% stimuli))
  no_error_notes <- length(user_melody_input) - no_correct_notes
  errors_boolean <- as.vector(!user_melody_input %in% stimuli)

  # Octave-independent (i.e octave errors allowed)
  user_pitch_classes <- itembankr::midi_to_pitch_class(user_melody_input) # pitch_class allows octave errors
  stimuli_pitch_classes <- itembankr::midi_to_pitch_class(stimuli)
  correct_boolean_octaves_allowed <- as.vector(user_pitch_classes %in% stimuli_pitch_classes)
  errors_boolean_octaves_allowed <- as.vector(!user_pitch_classes %in% stimuli_pitch_classes)
  no_correct_notes_octaves_allowed <- sum(correct_boolean_octaves_allowed)
  no_error_notes_octaves_allowed <- sum(errors_boolean_octaves_allowed)

  # Accuracy stuff

  # Octave-dependent
  # no_hits <- no_correct_notes
  # no_false_alarms <- no_error_notes
  # no_misses <- length(setdiff(stimuli, user_melody_input))

  # Some of these are the same thing, with different names
  no_hits <- no_correct_notes_octaves_allowed
  no_false_alarms <- no_error_notes_octaves_allowed
  no_misses <- length(setdiff(stimuli_pitch_classes, user_pitch_classes))

  if(compute_accuracy_measures_aligned) {
    acc <- compute_accuracy_measures_aligned(stimuli, user_melody_input)
    accuracy <- acc$accuracy
    precision <- acc$precision
    recall <- acc$recall
    F1_score = acc$F1_score
  } else {
    accuracy <- compute_accuracy(no_hits, no_false_alarms, no_misses)
    precision <- compute_precision(no_hits, no_false_alarms)
    recall <- compute_recall(no_hits, no_misses)
    F1_score = compute_F1_score(no_hits, no_misses, no_false_alarms)
  }

  PMI <- if(with_pmi) pmi(stimuli, user_melody_input) else NA

  # opti3
  opti3 <- get_opti3(stimuli, stimuli_durations, stimuli_length, features_df)
  no_recalled_notes <- length(user_melody_input)

  # accuracy style measures

  ## proportion of correct note events i.e., no_correct_notes/no_recalled_notes
  proportion_of_correct_note_events <- get_proportion_of_correct_note_events(stimuli, user_melody_input, no_correct_notes, no_error_notes, no_recalled_notes)
  proportion_of_correct_note_events_octaves_allowed <- get_proportion_of_correct_note_events(stimuli, user_melody_input, no_correct_notes_octaves_allowed, no_error_notes_octaves_allowed, no_recalled_notes)

  ## controlled for stimuli length and [0,1]

  proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal <- proportion_of_correct_note_events * log_normal(no_recalled_notes/stimuli_length)
  proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal <- proportion_of_correct_note_events_octaves_allowed * log_normal(no_recalled_notes/stimuli_length)


  # proportion of stimuli (target) notes found
  proportion_of_stimuli_notes_found <- length(base::intersect(user_melody_input, unique(stimuli) )) / length(unique(stimuli))
  proportion_of_stimuli_notes_found_octaves_allowed <- length(base::intersect(user_pitch_classes, unique(stimuli_pitch_classes) )) / length(unique(stimuli_pitch_classes))

  # fine-grained pitch measures (i.e singing style):
  nearest_pitches <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli, user_production_pitches = features_df$note, allOctaves = TRUE)

  if(length(pyin_pitch_track) > 0 & !"note" %in% names(pyin_pitch_track)) {
    pyin_pitch_track <- pyin_pitch_track %>%
      dplyr::rowwise() %>%
      dplyr::mutate(note = if(is.na(freq)) NA else round(hrep::freq_to_midi(freq))) %>%
      dplyr::ungroup()
  }

  nearest_pitches_pyin_track <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli, user_production_pitches = pyin_pitch_track$note, allOctaves = TRUE)
  mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = features_df$note, stimuli = stimuli, freq = features_df$freq)
  mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(features_df$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)
  melody_dtw <- get_melody_dtw(stimuli, stimuli_durations, pyin_pitch_track, features_df)
  features_df$nearest_stimuli_note <- nearest_pitches

  if(length(pyin_pitch_track) > 0) {
    pyin_pitch_track <- pyin_pitch_track %>%
      dplyr::mutate(interval = c(NA, diff(note)),
                    interval_cents = itembankr::cents(dplyr::lag(freq), freq),
                    nearest_stimuli_note = nearest_pitches_pyin_track)
  }

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
    no_recalled_notes = no_recalled_notes,
    no_correct_notes = no_correct_notes,
    no_error_notes = no_error_notes,
    errors_boolean = errors_boolean,
    correct_boolean_octaves_allowed = correct_boolean_octaves_allowed,
    errors_boolean_octaves_allowed = errors_boolean_octaves_allowed,
    no_correct_notes_octaves_allowed = no_correct_notes_octaves_allowed,
    no_error_notes_octaves_allowed = no_error_notes_octaves_allowed,
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
    melody_interval_accuracy = score_melody_interval_accuracy(features_df$interval, features_df$interval_cents, diff(stimuli)),
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    F1_score = F1_score,
    PMI = PMI)

  if(as_tb) {
    tibble::as_tibble(base::t(res))
  } else {
    res
  }


}



compute_accuracy <- function(no_hits, no_false_alarms, no_misses) no_hits / (no_hits + no_false_alarms + no_misses)
compute_recall <- function(no_hits, no_misses) no_hits/(no_hits + no_misses)
compute_precision <- function(no_hits, no_false_alarms) no_hits/(no_hits + no_false_alarms)
compute_F1_score <- function(no_hits, no_misses, no_false_alarms) 2 * no_hits/(2 * no_hits + no_misses + no_false_alarms)

compute_sloboda_parker <- function(stimuli, recall) {
  no_hits <- purrr::map2_lgl(stimuli, recall, `==`) %>% sum()
  no_misses <- purrr::map2_lgl(stimuli, recall, `!=`) %>% sum()
  compute_recall(no_hits, no_misses)
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



# helper functions / mainly for dealing with presence of NAs when scoring methods used at test time

get_proportion_of_correct_note_events <- function(stimuli, user_melody_input, no_correct_notes, no_error_notes, no_recalled_notes) {
  if (no_error_notes == 0 & no_correct_notes == length(stimuli)) {
    proportion_of_correct_note_events  <- 1
  } else {
    proportion_of_correct_note_events <- no_correct_notes/no_recalled_notes
  }
}

get_durations <- function(result) {
  if(is.scalar.null(result$dur)) {
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
    if(is.scalar.na(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }

    stimuli_df <- tibble::tibble(
      note = stimuli,
      dur = stimuli_durations,
      onset = c(0, cumsum(stimuli_durations)[1:(length(stimuli_durations)-1)]),
      ioi = c(NA, diff(onset)),
      ioi_class = itembankr::classify_duration(ioi)
    ) %>% itembankr::segment_phrase()

    opti3 <- opti3_df(melody1 = stimuli_df,
                      melody2 = user_input_as_pyin)
  }
}




get_longer_and_shorter_melodies <- function(stimuli, recall) {

  if(length(stimuli) == length(recall)) {
    shorter <- NA
    longer <- NA
    longer_mel_type <- NA
    shorter_mel_type <- NA
  } else if(length(stimuli) < length(recall)){
    shorter <- stimuli
    longer <- recall
    longer_mel_type <- "recall"
    shorter_mel_type <- "stimuli"
  } else if(length(stimuli) > length(recall)) {
    shorter <- recall
    longer <- stimuli
    longer_mel_type <- "stimuli"
    shorter_mel_type <- "recall"
  } else {
    stop("Alignment issue")
  }

  list(longer = list(mel_type = longer_mel_type, mel = longer),
       shorter = list(mel_type = shorter_mel_type, mel = shorter)
       )

}


compute_accuracy_measures_aligned <- function(stimuli, recall){

  long_short_res <- get_longer_and_shorter_melodies(stimuli, recall)
  longer_mel <- long_short_res$longer
  shorter_mel <- long_short_res$shorter
  longer_mel_type <- long_short_res$longer_mel_type
  shorter_mel_type <- long_short_res$shorter_mel_type
  longer_mel_length <- length(longer_mel)
  shorter_mel_length <- length(shorter_mel)

  if(longer_mel_length == shorter_mel_length) {

    recall_pitch_classes <- itembankr::midi_to_pitch_class(recall)
    stimuli_pitch_classes <- itembankr::midi_to_pitch_class(stimuli)
    no_hits <- sum(recall_pitch_classes %in% stimuli_pitch_classes)
    no_misses <- length(setdiff(stimuli_pitch_classes, recall_pitch_classes))
    no_false_alarms <- sum(!recall_pitch_classes %in% stimuli_pitch_classes)
    accuracy <- compute_accuracy(no_hits, no_false_alarms, no_misses)
    precision <- compute_precision(no_hits, no_false_alarms)
    recall <- compute_recall(no_hits, no_misses)
    F1_score <- compute_F1_score(no_hits, no_misses, no_false_alarms)

    res <- tibble::tibble(
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      F1_score = F1_score
    )

    return(res)

  } else {

    longer_mel_ngrams <- itembankr::get_all_ngrams(longer_mel, N = shorter_mel_length)


    res <- longer_mel_ngrams %>%
      dplyr::select(value) %>%
      dplyr::rename(longer_mel_v = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(longer_mel_v = list(itembankr::str_mel_to_vector(longer_mel_v))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(shorter_mel_v = list(shorter_mel))

    if(shorter_mel_type == "recall") {
      res <- res %>%
        dplyr::rename(recall = shorter_mel_v, stimuli = longer_mel_v)
    } else {
      res <- res %>%
        dplyr::rename(stimuli = shorter_mel_v, recall = longer_mel_v)
    }

    res <- res %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        recall_pitch_classes = list(itembankr::midi_to_pitch_class(recall)),
        stimuli_pitch_classes = list(itembankr::midi_to_pitch_class(stimuli)),
        no_hits = length(recall_pitch_classes %in% stimuli_pitch_classes),
        no_misses = length(setdiff(stimuli_pitch_classes, recall_pitch_classes)),
        no_false_alarms = length(!recall_pitch_classes %in% stimuli_pitch_classes),
        accuracy = compute_accuracy(no_hits, no_false_alarms, no_misses),
        precision = compute_precision(no_hits, no_misses),
        recall = compute_recall(no_hits, no_false_alarms),
        F1_score = compute_F1_score(no_hits, no_misses, no_false_alarms)
      ) %>%
      dplyr::ungroup()

    res <- tibble::tibble(
      accuracy = max(res$accuracy, na.rm = TRUE),
      precision = max(res$precision, na.rm = TRUE),
      recall = max(res$recall, na.rm = TRUE),
      F1_score = max(res$F1_score, na.rm = TRUE)
    )

    return(res)

  }

}

# get_longer_and_shorter_melodies(1:10, 1:9)

# align_stimuli_and_recall(60:67, 72:73, compute_recall, "recall")


