

#' Wrapper to add user-specified additional scoring measures to pYIN melodic production
#'
#' @param type
#' @param melconv
#' @param additional_scoring_measures
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin_melodic_production_additional_measures <- function(type = c("both", "notes", "pitch_track"),
                                                                   melconv = FALSE,
                                                                   additional_scoring_measures = NULL, ...) {

    function(input, state, ...) {
      get_answer_pyin_melodic_production(input = input,
                                         state = state,
                                         type = type,
                                         melconv = melconv,
                                        additional_scoring_measures = additional_scoring_measures, ...)
    }

}

#' Analyse a melody with pyin then compute melodic production scores
#'
#' @param input
#' @param type
#' @param state
#' @param melconv
#' @param additional_scoring_measures
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin_melodic_production <- function(input,
                                               type = c("both", "notes", "pitch_track"),
                                               state,
                                               melconv = FALSE,
                                               additional_scoring_measures = NULL, ...) {

  logging::loginfo("Get pyin melodic production")


  pyin_res <- get_answer_pyin(input, type, state, melconv, ...)

  logging::loginfo("Got pyin")

  if(is.scalar.na.or.null(pyin_res$pyin_res) || is.scalar.na.or.null(pyin_res$pyin_res$freq)) {

    res <- list(
      error = TRUE,
      reason = "There was nothing in the pitch track",
      user_satisfied = if(is.null(input$user_satisfied)) NA else input$user_satisfied,
      user_rating = if(is.null(input$user_rating)) NA else input$user_rating,
      attempt = if(length(input$attempt) == 0) NA else as.numeric(input$attempt),
      opti3 = NA,
      answer_meta_data = tibble::as_tibble(input$answer_meta_data),
      stimuli = as.numeric(rjson::fromJSON(input$stimuli))
      )

    logging::loginfo("There was nothing in the pitch track")


  } else {

    res <- concat_mel_prod_results(input = input,
                                   state = state,
                                   melconv_res = pyin_res$melconv_res,
                                   pyin_style_res = pyin_res,
                                   pyin_pitch_track = pyin_res$pyin_pitch_track,
                                   additional_scoring_measures = additional_scoring_measures)

  }

  res

}



get_answer_pyin_note_only <- function(input, type = "notes", state, ...) {
  get_answer_pyin_melodic_production(input, type, state, melconv, ...)
}

#' get_answer pyin for long notes
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin_long_note <- function(input, state, ...) {

  audio_file <- get_audio_file(input, state)

  pyin_res <- pyin::pyin(audio_file, type = "pitch_track")


  if(is.scalar.na.or.null(pyin_res)) {

    long_note_pitch_measures <- list(
      "long_note_accuracy" = NA,
      "long_note_var" = NA,
      "long_note_dtw_distance" = NA,
      "long_note_autocorrelation_mean" = NA,
      "long_note_run_test" = NA,
      "long_note_no_cpts" = NA,
      "long_note_beginning_of_second_cpt" = NA,
      "long_note_na_count" = NA,
      "long_note_dtw_distance_max" = NA,
      "long_note_accuracy_max" = NA,
      "long_note_freq_max" = NA,
      "long_note_freq_min" = NA
    )

    noise.classification <- list(prediction = "noise", failed_tests = 'na_count')

  } else {
    long_note_pitch_measures <- long_note_pitch_metrics(as.numeric(input$stimuli), pyin_res$freq)
    noise.classification <- classify_whether_noise(long_note_pitch_measures)
  }


  display_noise_trial_message(noise.classification, state)

  c(
    list(file = audio_file,
         stimuli = as.numeric(input$stimuli),
         onset = if(is.scalar.na.or.null(pyin_res)) NA else pyin_res$onset,
         freq = if(is.scalar.na.or.null(pyin_res)) NA else pyin_res$freq,
         noise_classification = noise.classification$prediction,
         failed_tests = noise.classification$failed_tests),
    # Append measures
    long_note_pitch_measures

  )


}

display_noise_trial_message <- function(noise_classification, state) {

  display_noise_trial_notificiation <- psychTestR::get_global("display_noise_trial_message", state)

  if(is.null(display_noise_trial_notificiation)) {
    display_noise_trial_notificiation <- FALSE
  }

  if(display_noise_trial_notificiation) {
    if(noise_classification$prediction == "noise") {
      shiny::showNotification(paste0(c("Prediction: That was a noise trial. ", paste0(length(noise_classification$failed_tests), "/9 failed tests: "), noise_classification$failed_tests), collapse = "  "))
    } else {
      shiny::showNotification(paste0(c("Prediction: That was a proper long note singing trial. ", paste0(length(noise_classification$failed_tests), "/9 failed tests: "), noise_classification$failed_tests), collapse = "  "))
    }
  }
}


#' Get answer by running pyin on a recorded audio file
#'
#' @param input
#' @param type
#' @param state
#' @param melconv
#' @param add_silence_to_beginning_of_audio_file In seconds.
#' @param add_silence_to_end_of_audio_file In seconds.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin <- function(input,
                            type = c("both", "notes", "pitch_track"),
                            state,
                            melconv = FALSE,
                            add_silence_to_beginning_of_audio_file = 0.5,
                            add_silence_to_end_of_audio_file = 0,
                            ...) {

  type <- match.arg(type)

  # Get file
  audio_file <- get_audio_file(input, state)

  # For audio with silence added at beginning:
  new_audio_file <- paste0(tools::file_path_sans_ext(audio_file), "_beginning_silence_added.wav")

  # Add silence to beginning
  silence_file <- add_silence_to_audio_file(old_file = audio_file,
                            new_file = new_audio_file, # Note, we overwrite the old file
                            no_seconds_silence_beginning = add_silence_to_beginning_of_audio_file,
                            no_seconds_silence_end = add_silence_to_end_of_audio_file)

  file_to_use <- if(identical(silence_file, "ERROR")) audio_file else new_audio_file

  # Get pyin
  pyin_res <- get_pyin(file_to_use, type, state)

  # Get melconv (optionally)
  melconv_res <- get_melconv(melconv, pyin_res)

  list(pyin_res = pyin_res$pyin_res,
       pyin_pitch_track = pyin_res$pyin_pitch_track,
       melconv_res = melconv_res)

}



#'summarise pyin output with basic statistics
#'
#' @param input
#' @param state
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_simple_pyin_summary <- function(input, state, ...) {

  audio_file <- get_audio_file(input, state)

  pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)

  if(is.scalar.na(pyin_res$note)) {
    res <- list("Min." = NA,
                "1st Qu." = NA,
                "Median" = NA,
                "Mean" = NA,
                "3rd Qu." = NA,
                "Max." = NA)
  } else {
    res <- as.list(round(summary(pyin_res$note)))
  }

  res$file <- audio_file
  res
}




get_audio_file <- function(input, state, ...) {

  audio_file <- paste0('www/audio/', input$file_url)

  audio_file
}

get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, state, ...) {
      audio_file <- get_audio_file(input, state)
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
      if(is.scalar.null(pyin_res$freq) | is.scalar.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        freqs <- pyin_res$freq
        list(user_response = floor(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else if (floor_or_ceiling == "ceiling") {

    function(input, state, ...) {
      audio_file <- get_audio_file(input, state)
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
      if(is.scalar.null(pyin_res$freq) | is.scalar.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        audio_file <- get_audio_file(input, state)
        pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
        freqs <- pyin_res$freq
        list(user_response = ceiling(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else {

    function(input, state, ...) {
      audio_file <- get_audio_file(input, state)
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
      if(is.scalar.null(pyin_res$freq) | is.logical(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        freqs <- pyin_res$freq
        list(user_response = round(mean(hrep::freq_to_midi(freqs))))
      }
    }
  }
}

get_pyin <- function(audio_file, type, state) {

  if(type == "notes") {
    pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
    list("pyin_res" = pyin_res, "pyin_pitch_track" = NA)
  } else if(type == "pitch_track") {
    pyin_pitch_track <- pyin::pyin(audio_file, type = "pitch_track", if_bad_result_return_single_na = FALSE)
    list("pyin_res" = NA, "pyin_pitch_track" = pyin_pitch_track)
  } else {
    pyin_res <- pyin::pyin(audio_file, type = "notes", if_bad_result_return_single_na = FALSE)

    pyin_pitch_track <- pyin::pyin(audio_file, type = "pitch_track", if_bad_result_return_single_na = FALSE)

    list('pyin_res' = pyin_res,
         'pyin_pitch_track' = pyin_pitch_track)
  }
}


get_melconv <- function(melconv, pyin_res) {

  if(melconv) {
    melconv_res <- melconv_from_pyin_res(pyin_res)
    melconv_notes <- itembankr::str_mel_to_vector(melconv_res$notes)
    melconv_durations <- itembankr::str_mel_to_vector(melconv_res$durations)
  } else {
    melconv_notes <- NA
    melconv_durations <- NA
  }
  list(
    "notes" = melconv_notes,
    "durations" = melconv_durations
  )
}


# Midi-related


#' Get a MIDI result and compute melodic production scores
#'
#' @param input
#' @param state
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_midi_melodic_production <- function(input, state, ...) {



  if(check_midi_melodic_production_lengths(input$user_response_midi_note_on,
                                          input$user_response_midi_note_off,
                                          input$onsets_noteon,
                                          input$onsets_noteoff)) {

    midi_res <- list(
      pyin_style_res = tibble::tibble(
      file_name = NA,
      onset = NA,
      dur = NA,
      freq = NA,
      note = NA))


    res <- list(error = TRUE,
                reason = "No midi notes / lengths unequal.",
                user_satisfied = if (is.null(input$user_satisfied)) NA else input$user_satisfied,
                user_rating = if (is.null(input$user_rating)) NA else input$user_rating,
                attempt = if (length(input$attempt) == 0) NA else as.numeric(input$attempt),
                opti3 = NA,
                answer_meta_data = tibble::as_tibble(input$answer_meta_data),
                stimuli = as.numeric(rjson::fromJSON(input$stimuli)))

     } else {

       midi_res <- get_answer_midi(input, state, ...)


       res <- concat_mel_prod_results(input,
                                      state,
                                      melconv_res = list(notes = NA, durations = NA),
                                      pyin_style_res = midi_res$pyin_style_res,
                                      pyin_pitch_track = NA)
     }


  return(res)

}

#' Get answer for a MIDI page
#'
#' @param input
#' @param state
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_midi <- function(input, state, ...) {


  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {

    list(
      stimulus_trigger_times = NA,
      onsets_noteon_timecode = NA,
      user_response_midi_note_on = NA,
      user_response_midi_note_off =  NA,
      onsets_noteon =  NA,
      onsets_off = NA,
      pyin_style_res = NA,
      stimuli = if(is.null(input$stimuli)) NA else as.numeric(rjson::fromJSON(input$stimuli))
      )

  } else {

    trial_start_time_timecode <- input$trial_start_time
    trial_start_time_timecode2 <- input$trial_start_time2
    latency_estimate <- trial_start_time_timecode2 - trial_start_time_timecode
    onsets_noteon_timecode <- if(is.null(input$onsets_noteon_timecode)) NA else as.numeric(rjson::fromJSON(input$onsets_noteon_timecode))
    notes <- if(is.null(input$user_response_midi_note_on)) NA else as.integer(rjson::fromJSON(input$user_response_midi_note_on))
    notes_off <- if(is.null(input$user_response_midi_note_off)) NA else as.integer(rjson::fromJSON(input$user_response_midi_note_off))
    onsets <- (onsets_noteon_timecode - trial_start_time_timecode) / 1000
    stimulus_trigger_times <- if(is.null(input$stimulus_trigger_times)) NA else (as.numeric(rjson::fromJSON(input$stimulus_trigger_times)) - trial_start_time_timecode) / 1000

    stimulus <- if(is.null(input$stimuli)) NA else as.numeric(rjson::fromJSON(input$stimuli))

    # We just assume the last duration is 0.5 always (or the last duration of the stimulus, if there is one).
    # There is no way of telling when the participant really designates that a "hit" is over
    # Well, technically you can do this with a keyboard noteoff, but this is complicated for various reasons.
    if(is.scalar.na.or.null(stimulus)) {
      last_dur <- 0.5
    } else {
      last_dur <- stimulus[length(stimulus)]
    }

    pyin_style_res <- tibble::tibble(
      onset = onsets,
      dur = c(diff(onsets), last_dur),
      note = notes,
      freq = hrep::midi_to_freq(notes),
      file_name = NA,
    ) %>%
      dplyr::relocate(file_name)

    list(
      stimulus_trigger_times = stimulus_trigger_times,
      onsets_noteon_timecode = onsets_noteon_timecode,
      user_response_midi_note_on = notes,
      user_response_midi_note_off =  notes_off,
      onsets_noteon = onsets,
      pyin_style_res = pyin_style_res,
      stimuli = stimulus,
      trial_start_time_timecode = trial_start_time_timecode,
      trial_start_time_timecode2 = trial_start_time_timecode2,
      latency_estimate = latency_estimate
      )
  }

}


#' Get answer for rhythm production data
#'
#' @param input
#' @param state
#' @param type
#' @param feedback
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_rhythm_production <- function(input, state, type = c("midi", "audio", "key_presses"), feedback = TRUE, ...) {

  type <- match.arg(type)

  stimuli_durations <- if(is.scalar.na.or.null(input$stimuli_durations)) NA else round(rjson::fromJSON(input$stimuli_durations), 2)

  if(type == "midi") {

    res <- get_answer_rhythm_production_midi(input, state, ...)

    if(is.scalar.na.or.null(res$pyin_style_res) || is.scalar.na.or.null(res$pyin_style_res$dur)) {
      user_durations <- NA
    } else {
      user_durations <- res$pyin_style_res$dur
    }

  } else if(type == "audio") {

    res <- get_answer_rhythm_production_audio(input, state, ...)
    if(is.scalar.na.or.null(res$dur) || "error" %in% names(res)) {
      user_durations <- NA
    } else {
      user_durations <- res$dur
    }

  } else if(type == "key_presses") {

    res <- get_answer_rhythm_production_key_presses(input, state, ...)
    user_durations <- res$user_durations

  } else {
      stop("Unknown type.")
  }

  answer_meta_data <- input$answer_meta_data

  bpm <- if(is.null(answer_meta_data)) NULL

  res_scored <- score_rhythm_production(stimuli_durations, user_durations, bpm = bpm)


  # Display feedback
  display_rhythm_production_feedback(feedback, res)


  res <- c(
    res,
    res_scored,
    list(
      user_satisfied = if (is.scalar.na.or.null(input$user_satisfied)) NA else input$user_satisfied,
      user_rating = if (is.scalar.na.or.null(input$user_rating)) NA else input$user_rating,
      attempt = if (length(input$attempt) == 0) NA else as.numeric(input$attempt)
    )
  )

  return(res)

}

get_answer_rhythm_production_midi <- function(input, state, ...) {

  midi_res <- get_answer_midi(input, state, ...)

  return(midi_res)
}


get_answer_rhythm_production_key_presses <- function(input, state, ...) {

  keypress_res <- get_answer_key_presses_page(input, state, ...)

  return(keypress_res)
}


get_answer_key_presses_page <- function(input, ...) {

  onsets_keydown <- rjson::fromJSON(input$onsets_keydown)
  onsets_keyup <- rjson::fromJSON(input$onsets_keyup)
  durations <- diff(onsets_keydown)

  stimulus <- if(is.null(input$stimuli)) NA else as.numeric(rjson::fromJSON(input$stimuli))

  if(is.scalar.na.or.null(stimulus)) {
    last_dur <- 0.5
  } else {
    last_dur <- stimulus[length(stimulus)]
  }

  # last_dur <- onsets_keyup[length(onsets_keyup)]  - onsets_keydown[length(onsets_keydown)]
  # last_dur <- if(is.null(last_dur) || last_dur < 0) 0.5 else last_dur

  durations <- c(durations, last_dur)

  list(
    keypress_keydown = rjson::fromJSON(input$keypress_keydown),
    keypress_keyup = rjson::fromJSON(input$keypress_keyup),
    onsets_keyup = onsets_keyup,
    onsets_keydown = onsets_keydown,
    user_durations = durations
  )

}



get_answer_rhythm_production_audio <- function(input, state, ...) {

  onset_res <- get_answer_onset_detection(input, state, ...)

  if(is.scalar.na.or.null(onset_res) || is.scalar.na.or.null(onset_res$onset)) {

    logging::loginfo("There was nothing in the onset track")

    res <- list(error = TRUE,
                reason = "There was nothing in the onset track")

    logging::loginfo("There was nothing in the onset track")

  } else {
    return(onset_res)
  }
}

bpm_to_ms <- function(bpm) {
  60/bpm
}


get_answer_midi_note_mode <- function(input, state, ...) {

  if(is.scalar.na(input$user_response_midi_note_on) |
     is.scalar.null(input$user_response_midi_note_on)) {
    list(note = NA)
  } else {
    list(note = getmode(rjson::fromJSON(input$user_response_midi_note_on)))
  }
}

# other

get_answer_interval_page <- function(input, state, ...) {

  answer_meta_data <- psychTestR::get_global("answer_meta_data", state)

  # TODO: The following works as feedback, but make an option to enable it conditionally (optionally):

  # msg <- if(input$dropdown == answer_meta_data$interval) "Correct Answer!" else "Wrong Answer!"
  # shiny::showNotification(msg)

  c(
    list(
      user_choice = input$dropdown),
    as.list(answer_meta_data)
  )
}



#' Simple saving of the corresponding URL of a trial file
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_save_audio_file <- function(input, ...) {
  list(key = input$key,
       file_url = input$file_url,
       user_satisfied = input$user_satisfied,
       user_rating = if(is.null(input$user_rating)) NA else input$user_rating)
}



# Generic

concat_mel_prod_results <- function(input,
                                    state,
                                    melconv_res,
                                    pyin_style_res,
                                    pyin_pitch_track,
                                    additional_scoring_measures = NULL, ...) {

  # Grab MIDI-specific data if available
  if(length(input$user_response_midi_note_off) == 0) {
    user_response_midi_note_off <- NA
    onsets_noteoff <- NA
  } else {
    user_response_midi_note_off <- as.numeric(rjson::fromJSON(input$user_response_midi_note_off))
    onsets_noteoff <- as.numeric(rjson::fromJSON(input$onsets_noteoff))
  }

  # Grab stimuli information
  if(is.scalar.null(input$stimuli)) {
    stimuli <- rjson::fromJSON(psychTestR::get_global("stimuli", state))
    stimuli_durations <- rjson::fromJSON(psychTestR::get_global("stimuli_durations", state))
  } else {
    stimuli <- as.numeric(rjson::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(rjson::fromJSON(input$stimuli_durations))
  }

  # Produce trial-level scores
  scores <- score_melodic_production(user_melody_freq = if(is.null(pyin_style_res$pyin_res)) pyin_style_res$freq else pyin_style_res$pyin_res$freq,
                                     user_melody_input = if(is.null(pyin_style_res$pyin_res)) pyin_style_res$note else pyin_style_res$pyin_res$note,
                                     user_duration_input = if(is.null(pyin_style_res$pyin_res)) pyin_style_res$dur else pyin_style_res$pyin_res$dur,
                                     user_onset_input = if(is.null(pyin_style_res$pyin_res)) pyin_style_res$onset else pyin_style_res$pyin_res$onset,
                                     stimuli = stimuli,
                                     stimuli_durations = stimuli_durations,
                                     pyin_pitch_track = pyin_pitch_track,
                                     user_response_midi_note_off = user_response_midi_note_off,
                                     onsets_noteoff = onsets_noteoff,
                                     answer_meta_data = tibble::as_tibble(input$answer_meta_data),
                                     as_tb = FALSE,
                                     additional_scoring_measures = additional_scoring_measures)

  # Store scores for final scores at end of test. Perhaps now deprecated
  # But could be useful to standardise final scores across tests (i.e., so each test doesn't need it's own final_results function)

  ongoing_scores <- psychTestR::get_global("scores", state)
  psychTestR::set_global("scores", c(ongoing_scores, scores$opti3), state)

  results <- c(
    list(file = get_audio_file(input, state),
         error = FALSE,
         attempt = if(length(input$attempt) == 0) NA  else as.numeric(input$attempt),
         user_satisfied = if(is.null(input$user_satisfied)) NA else input$user_satisfied,
         user_rating = if(is.null(input$user_rating)) NA else input$user_rating,
         melconv_notes = melconv_res$notes,
         melconv_durations = melconv_res$durations),
        # Append scores
         scores
      )

  # Add trial level data to DB
  add_trial_trial_level_data_to_db(state = state, res = results, pyin_style_res = pyin_style_res, scores = scores)

  results
}



add_trial_trial_level_data_to_db <- function(state, res, pyin_style_res, scores) {

  use_musicassessr_db <- psychTestR::get_global("use_musicassessr_db", state)

  if(is.null(use_musicassessr_db)) {
    use_musicassessr_db <- FALSE
  }

  if(use_musicassessr_db) {

    db_con <- psychTestR::get_global("db_con", state)

    logging::loginfo("Store results in SQL database")

    singing_trial <- psychTestR::get_global("singing_trial", state)
    print('singing_trial??')
    print(singing_trial)
    instrument <- if(singing_trial) "Voice" else psychTestR::get_global("inst", state)
    print(instrument)
    trial_time_started <- psychTestR::get_global("trial_time_started", state)
    session_id <- psychTestR::get_global("session_id", state)
    item_bank_id <- psychTestR::get_global("item_bank_id", state)
    item_id <- if(is.null(res$answer_meta_data$item_id)) psychTestR::get_global("item_id", state) else res$answer_meta_data$item_id
    display_modality <- psychTestR::get_global("display_modality", state)
    phase <- if(is.null(psychTestR::get_global("phase", state))) res$answer_meta_data$phase else psychTestR::get_global("phase", state)
    rhythmic <- if(is.null(psychTestR::get_global("rhythmic", state))) res$answer_meta_data$rhythmic else psychTestR::get_global("rhythmic", state)
    test_id <- psychTestR::get_global('test_id', state)
    attempt <- psychTestR::get_global("number_attempts", state)

    # Append trials
    trial_id <- db_append_trials(
                     db_con,
                     audio_file = basename(res$file),
                     time_started = trial_time_started,
                     time_completed = Sys.time(),
                     instrument = instrument,
                     attempt = as.integer(attempt),
                     item_id = item_id,
                     display_modality = display_modality,
                     phase = phase,
                     rhythmic = rhythmic,
                     item_bank_id = as.integer(item_bank_id),
                     session_id = as.integer(session_id),
                     test_id = as.integer(test_id)
                     )

    # Append melodic production
    db_append_melodic_production(db_con, trial_id, pyin_style_res$pyin_res, scores$correct_boolean_octaves_allowed)

    # Append scores
    db_append_scores_trial(db_con,
                           res$trial_length,
                           res$no_recalled_notes,
                           res$no_correct_notes,
                           res$no_error_notes,
                           res$no_correct_notes_octaves_allowed,
                           res$no_error_notes_octaves_allowed,
                           res$proportion_of_correct_note_events,
                           res$proportion_of_correct_note_events_octaves_allowed,
                           res$proportion_of_stimuli_notes_found,
                           res$proportion_of_stimuli_notes_found_octaves_allowed,
                           res$opti3,
                           res$ngrukkon,
                           res$harmcore,
                           res$rhythfuzz,
                           res$melody_dtw,
                           res$mean_cents_deviation_from_nearest_stimuli_pitch,
                           res$mean_cents_deviation_from_nearest_midi_pitch,
                           res$melody_note_accuracy,
                           res$melody_interval_accuracy,
                           res$accuracy,
                           res$precision,
                           res$recall,
                           res$F1_score,
                           res$PMI,
                           trial_id)


  }
}



#' Get onset information from an audio file
#'
#' @param input
#' @param state
#' @param add_silence_to_beginning_of_audio_file
#' @param add_silence_to_end_of_audio_file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_onset_detection <- function(input,
                                       state,
                                       add_silence_to_beginning_of_audio_file = 0.5,
                                       add_silence_to_end_of_audio_file = 0, ...) {

  audio_file <- get_audio_file(input, state, ...)

  # For audio with silence added at beginning:
  new_audio_file <- paste0(tools::file_path_sans_ext(audio_file), "_beginning_silence_added.wav")

  # Make sure old original file is built
  valid_file <- FALSE
  while(!valid_file) {
    valid_file <- file.exists(audio_file)
  }

  # Add silence to beginning
  silence_file <- add_silence_to_audio_file(old_file = audio_file,
                                            new_file = new_audio_file, # Note, we overwrite the old file
                                            no_seconds_silence_beginning = add_silence_to_beginning_of_audio_file,
                                            no_seconds_silence_end = add_silence_to_end_of_audio_file)

  file_to_use <- if(identical(silence_file, "ERROR")) audio_file else new_audio_file

  # And same with new (silence-added) file
  valid_file <- FALSE
  while(!valid_file) {
    valid_file <- file.exists(file_to_use)
  }



  onset_res <- vampr::onset_detection(file_to_use)

  trial_start_time_timecode <- input$trial_start_time
  trial_start_time_timecode2 <- input$trial_start_time2
  latency_estimate <- trial_start_time_timecode2 - trial_start_time_timecode
  stimulus_trigger_times <- if(is.null(input$stimulus_trigger_times)) NA else (as.numeric(rjson::fromJSON(input$stimulus_trigger_times)) - trial_start_time_timecode) / 1000
  # We just assume the last duration is 0.5 always. There is no way of telling when the participant really designates that a "hit" is over
  # Technically you can do this with a keyboard noteoff, but this is complicated for various reasons.
  stimulus <- if(is.null(input$stimuli)) NA else as.numeric(rjson::fromJSON(input$stimuli))

  if(is.scalar.na.or.null(stimulus)) {
    last_dur <- 0.5
  } else {
    last_dur <- stimulus[length(stimulus)]
  }

  onsets <- onset_res$onset
  durs <- c(diff(onsets), last_dur)

  list(
    file = new_audio_file,
    onset = if(is.scalar.na.or.null(onset_res)) NA else onsets,
    dur = if(is.scalar.na.or.null(onset_res)) NA else durs,
    stimulus_trigger_times = stimulus_trigger_times,
    stimuli = stimulus,
    trial_start_time_timecode = trial_start_time_timecode,
    trial_start_time_timecode2 = trial_start_time_timecode2,
    latency_estimate = latency_estimate
    )

}


# Utils

check_midi_melodic_production_lengths <- function(user_response_midi_note_on,
                                                  user_response_midi_note_off,
                                                  onsets_noteon,
                                                  onsets_noteoff) {

  lengths <- c(length(rjson::fromJSON(user_response_midi_note_on)),
               length(rjson::fromJSON(user_response_midi_note_off)),
               length(rjson::fromJSON(onsets_noteon)),
               length(rjson::fromJSON(onsets_noteoff)))

  are_lengths_equal <- length(unique(lengths)) == 1

  any(lengths == 0) | ! are_lengths_equal
}

