

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

  answer_meta_data <- input$answer_meta_data

  if(is.character(answer_meta_data) && length(answer_meta_data) > 1L) {
    answer_meta_data <- named_chr_v_to_tib(answer_meta_data)
  } else {
    answer_meta_data <- tibble::as_tibble(answer_meta_data)
  }

  if("ngrukkon" %in% names(answer_meta_data)) {
    answer_meta_data <- answer_meta_data %>%
      dplyr::rename(ngrukkon_with_parent_melody = ngrukkon)
  }

  if(is.scalar.na.or.null(pyin_res$pyin_res) || is.scalar.na.or.null(pyin_res$pyin_res$freq)) {

    res <- list(
      error = TRUE,
      reason = "There was nothing in the pitch track",
      user_satisfied = if(is.null(input$user_satisfied)) NA else input$user_satisfied,
      user_rating = if(is.null(input$user_rating)) NA else input$user_rating,
      attempt = if(length(input$attempt) == 0) NA else as.numeric(input$attempt),
      opti3 = NA,
      answer_meta_data = answer_meta_data,
      stimuli = as.numeric(jsonlite::fromJSON(input$stimuli))
      )

    logging::loginfo("There was nothing in the pitch track")


  } else {

    res <- concat_mel_prod_results(input = input,
                                   state = state,
                                   melconv_res = pyin_res$melconv_res,
                                   pyin_style_res = pyin_res,
                                   pyin_pitch_track = pyin_res$pyin_pitch_track,
                                   additional_scoring_measures = additional_scoring_measures,
                                   file_used = pyin_res$file_used)

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


  asynchronous_api_mode <- psychTestR::get_global("asynchronous_api_mode", state)


  if(length(asynchronous_api_mode) != 0L && asynchronous_api_mode) {

    res <- list(key = input$key,
         file_url = input$file_url,
         user_satisfied = input$user_satisfied)

  } else {

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

    res <- c(
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

  return(res)


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
       melconv_res = melconv_res,
       file_used = file_to_use)

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

    answer_meta_data <- input$answer_meta_data

    if(is.character(answer_meta_data) && length(answer_meta_data) > 0L) {
      answer_meta_data <- named_chr_v_to_tib(answer_meta_data)
    } else {
      answer_meta_data <- tibble::as_tibble(answer_meta_data)
    }

    if("ngrukkon" %in% names(answer_meta_data)) {
      answer_meta_data <- answer_meta_data %>%
        dplyr::rename(ngrukkon_with_parent_melody = ngrukkon)
    }


    res <- list(error = TRUE,
                reason = "No midi notes / lengths unequal.",
                user_satisfied = if (is.null(input$user_satisfied)) NA else input$user_satisfied,
                user_rating = if (is.null(input$user_rating)) NA else input$user_rating,
                attempt = if (length(input$attempt) == 0) NA else as.numeric(input$attempt),
                opti3 = NA,
                answer_meta_data = answer_meta_data,
                stimuli = as.numeric(jsonlite::fromJSON(input$stimuli)),
                stimuli_durations = as.numeric(jsonlite::fromJSON(input$stimuli_durations)))

     } else {

       midi_res <- get_answer_midi(input, state, ...)

       if(!psychTestR::get_global("asynchronous_api_mode", state)) {

         res <- concat_mel_prod_results(input,
                                        state,
                                        melconv_res = list(notes = NA, durations = NA),
                                        pyin_style_res = midi_res$pyin_style_res,
                                        pyin_pitch_track = NA)
       }
     }

  # We want to register the file in the DB even if there is no input:
  if(psychTestR::get_global("asynchronous_api_mode", state)) {

    review_items_id <- psychTestR::get_global('review_items_id', state)
    new_items_id <- psychTestR::get_global('new_items_id', state)
    review_items_id <- if(is.null(review_items_id)) NA else review_items_id
    new_items_id <- if(is.null(new_items_id)) NA else new_items_id

    stimuli <- as.numeric(jsonlite::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(jsonlite::fromJSON(input$stimuli_durations))

    test_id <- psychTestR::get_global('test_id', state)
    item_id <- psychTestR::get_global('item_id', state)
    user_id <- psychTestR::get_global('user_id', state)
    instrument <- psychTestR::get_global('inst', state)
    trial_time_started <- psychTestR::get_global('trial_time_started', state)
    trial_time_completed <- Sys.time()

    score_to_use <- "opti3"
    display_modality <- psychTestR::get_global('display_modality', state)
    phase <- psychTestR::get_global('phase', state)
    rhythmic <- psychTestR::get_global('rhythmic', state)
    module <- psychTestR::get_local(".module", state)

    logging::loginfo("module: %s", module)

    page_label <- input$page_label

    session_id <- psychTestR::get_global("session_id", state) %>% get_promise_value()

    if(length(session_id) > 1) {
      session_id <- session_id$session_id
    }


    additional <- psychTestR::get_global('additional', state)
    additional <- if(is.scalar.character(additional)) additional else jsonlite::toJSON(additional, auto_unbox = TRUE)

    melody_block_paradigm <- psychTestR::get_global('melody_block_paradigm', state)

    logging::loginfo('melody_block_paradigm: %s', melody_block_paradigm)

    dur <- midi_res$pyin_style_res$dur
    onset <- midi_res$pyin_style_res$onset
    note <- midi_res$pyin_style_res$note

    attempt <- psychTestR::get_global('number_attempts', state)

    if(length(attempt) == 0) {
      attempt <- 1L
    }

    logging::loginfo("Appending MIDI trial result")

    midi_trial_result <- promises::future_promise({

      musicassessrdb::midi_add_trial_and_compute_trial_scores_api(stimuli = if(length(stimuli) == 0) NA else stimuli,
                                                                  stimuli_durations = if(length(stimuli_durations) == 0) NA else stimuli_durations,
                                                                  test_id = if(length(test_id) == 0) NA else test_id,
                                                                  item_id = if(length(item_id) == 0) NA else item_id, # N.B. needs to be a scalar character, hence the NA string
                                                                  user_id = user_id,
                                                                  instrument = instrument,
                                                                  trial_time_started = trial_time_started,
                                                                  trial_time_completed = trial_time_completed,
                                                                  score_to_use = score_to_use,
                                                                  display_modality = display_modality,
                                                                  phase = if(length(phase) == 0 || musicassessr::is.scalar.na(phase)) "test" else phase,
                                                                  rhythmic = if(length(rhythmic) == 0 || musicassessr::is.scalar.logical(rhythmic)) FALSE else rhythmic,
                                                                  session_id = session_id,
                                                                  review_items_id = review_items_id,
                                                                  new_items_id = new_items_id,
                                                                  dur = dur,
                                                                  onset = onset,
                                                                  note = note,
                                                                  attempt = attempt,
                                                                  additional = additional,
                                                                  melody_block_paradigm = if(length(melody_block_paradigm) > 0) melody_block_paradigm else "NA",
                                                                  page_label = if(length(page_label) > 0) page_label else "NA",
                                                                  module = module)

    },
    seed = NULL) %>%
      promises::then(
        onFulfilled = function(result) {

          logging::loginfo("Promise fulfilled.")
          logging::loginfo("Returning promise result: %s", result)

          if(is.scalar.na.or.null(result)) {
            return(NA)
          } else if(result$status == 200) {
            return(result)
          } else {
            return(NA)
          }

        },
        onRejected = function(err) {

          logging::logerror("Promise failed: %s", err)

        }
      )

    res <- list(user_satisfied = if (is.null(input$user_satisfied)) NA else input$user_satisfied,
                user_rating = if (is.null(input$user_rating)) NA else input$user_rating,
                attempt = if (length(input$attempt) == 0) NA else as.numeric(input$attempt))

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

  if(length(jsonlite::fromJSON(input$user_response_midi_note_on)) == 0) {

    list(
      stimulus_trigger_times = NA,
      onsets_noteon_timecode = NA,
      user_response_midi_note_on = NA,
      user_response_midi_note_off =  NA,
      onsets_noteon =  NA,
      onsets_off = NA,
      pyin_style_res = NA,
      stimuli = if(is.null(input$stimuli)) NA else as.numeric(jsonlite::fromJSON(input$stimuli)),
      velocities = NA
      )

  } else {

    trial_start_time_timecode <- input$trial_start_time
    trial_start_time_timecode2 <- input$trial_start_time2
    latency_estimate <- trial_start_time_timecode2 - trial_start_time_timecode
    onsets_noteon_timecode <- if(is.null(input$onsets_noteon_timecode)) NA else as.numeric(jsonlite::fromJSON(input$onsets_noteon_timecode))
    notes <- if(is.null(input$user_response_midi_note_on)) NA else as.integer(jsonlite::fromJSON(input$user_response_midi_note_on))
    notes_off <- if(is.null(input$user_response_midi_note_off)) NA else as.integer(jsonlite::fromJSON(input$user_response_midi_note_off))
    onsets <- (onsets_noteon_timecode - trial_start_time_timecode) / 1000
    stimulus_trigger_times <- if(is.null(input$stimulus_trigger_times)) NA else (as.numeric(jsonlite::fromJSON(input$stimulus_trigger_times)) - trial_start_time_timecode) / 1000
    velocities <- if(is.scalar.na.or.null(input$velocities)) NA else as.numeric(jsonlite::fromJSON(input$velocities))
    stimulus <- if(is.null(input$stimuli)) NA else as.numeric(jsonlite::fromJSON(input$stimuli))
    stimulus_durations <- if(is.null(input$stimuli_durations)) NA else as.numeric(jsonlite::fromJSON(input$stimuli_durations))

    # We just assume the last duration is 0.5 always (or the last duration of the stimulus, if there is one).
    # There is no way of telling when the participant really designates that a "hit" is over
    # Well, technically you can do this with a keyboard noteoff, but this is complicated for various reasons.
    if(is.scalar.na.or.null(stimulus)) {
      last_dur <- 0.5
    } else {
      last_dur <- stimulus_durations[length(stimulus_durations)]
    }

    pyin_style_res <- tibble::tibble(
      onset = onsets,
      dur = c(diff(onsets), last_dur),
      note = notes,
      freq = hrep::midi_to_freq(notes),
      file_name = NA,
    ) %>%
      dplyr::relocate(file_name)


    # otf_r <- rhythfuzz(classify_duration(stimulus_durations), classify_duration(pyin_style_res$dur))

    # logging::loginfo("otf_r: %s", otf_r)

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
      latency_estimate = latency_estimate,
      velocities = velocities
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
get_answer_rhythm_production <- function(input,
                                         state, type = c("midi", "audio", "key_presses"), feedback = TRUE, ...) {

  type <- match.arg(type)

  stimuli_durations <- jsonlite::fromJSON(input$stimuli_durations)

  if(is.scalar.character(stimuli_durations) && stimuli_durations == "NA") {
    stimuli_durations <- NA
  } else if(length(input$stimuli_durations) == 0 || is.scalar.na.or.null.or.length.zero(stimuli_durations)) {
    stimuli_durations <- NA
  } else {
    stimuli_durations <- round(as.numeric(stimuli_durations), 2)
  }

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
      key = input$key,
      file_url = input$file_url,
      user_satisfied = if (is.scalar.na.or.null(input$user_satisfied)) NA else input$user_satisfied,
      user_rating = if (is.scalar.na.or.null(input$user_rating)) NA else input$user_rating,
      attempt = if (length(input$attempt) == 0) NA else as.numeric(input$attempt)
    )
  )

  return(res)

}

get_answer_rhythm_production_midi <- function(input, state, ...) {

  if(psychTestR::get_global("asynchronous_api_mode", state)) {
    midi_res <- get_answer_midi_melodic_production(input, state, ...)
  } else {
    midi_res <- get_answer_midi(input, state, ...)
  }

  return(midi_res)
}


get_answer_rhythm_production_key_presses <- function(input, state, ...) {

  keypress_res <- get_answer_key_presses_page(input, state, ...)

  return(keypress_res)
}


get_answer_key_presses_page <- function(input, ...) {

  onsets_keydown <- jsonlite::fromJSON(input$onsets_keydown)
  onsets_keyup <- jsonlite::fromJSON(input$onsets_keyup)
  durations <- diff(onsets_keydown)

  stimulus <- if(is.null(input$stimuli)) NA else as.numeric(jsonlite::fromJSON(input$stimuli))

  if(is.scalar.na.or.null(stimulus)) {
    last_dur <- 0.5
  } else {
    last_dur <- stimulus[length(stimulus)]
  }

  # last_dur <- onsets_keyup[length(onsets_keyup)]  - onsets_keydown[length(onsets_keydown)]
  # last_dur <- if(is.null(last_dur) || last_dur < 0) 0.5 else last_dur

  durations <- c(durations, last_dur)

  list(
    keypress_keydown = jsonlite::fromJSON(input$keypress_keydown),
    keypress_keyup = jsonlite::fromJSON(input$keypress_keyup),
    onsets_keyup = onsets_keyup,
    onsets_keydown = onsets_keydown,
    user_durations = durations
  )

}



get_answer_rhythm_production_audio <- function(input, state, ...) {

  if(psychTestR::get_global("asynchronous_api_mode", state)) {
    onset_res <- get_answer_add_trial_and_compute_trial_scores_s3(input, state, ...)
    return(onset_res)
  } else {
    onset_res <- get_answer_onset_detection(input, state, ...)
  }


  if(is.scalar.na.or.null(onset_res) || is.scalar.na.or.null(onset_res$onset)) {

    onset_res <- list(error = TRUE,
                      reason = "There was nothing in the onset track",
                      dur = NA)

    logging::loginfo("There was nothing in the onset track")

  }
  return(onset_res)
}

bpm_to_ms <- function(bpm) {
  60/bpm
}


get_answer_midi_note_mode <- function(input, state, ...) {

  if(is.scalar.na(input$user_response_midi_note_on) |
     is.scalar.null(input$user_response_midi_note_on)) {
    list(note = NA)
  } else {
    list(note = getmode(jsonlite::fromJSON(input$user_response_midi_note_on)))
  }
}

# Other

get_answer_interval_page <- function(input, state, ...) {

  answer_meta_data <- psychTestR::get_global("answer_meta_data", state)
  trial_time_started <- psychTestR::get_global("trial_time_started", state)

  print('get_answer_interval_page called')

  language <- psychTestR::get_url_params(state)$language

  answer <- input$dropdown

  if(language != "en") {
    answer <- translate_from_dict(non_english_translation = answer, language = language)
  }

  correct <- answer == answer_meta_data$interval

  trial_time_completed <- Sys.time()
  trial_time_taken <- trial_time_completed - trial_time_started

  # TODO: The following works as feedback, but make an option to enable it conditionally (optionally):
  # msg <- if(correct) "Correct Answer!" else "Wrong Answer!"
  # shiny::showNotification(msg)

  c(
    list(
      user_choice = input$dropdown),
    as.list(answer_meta_data),
    correct = correct,
    trial_time_started = trial_time_started,
    trial_time_completed = trial_time_completed,
    trial_time_taken = trial_time_taken
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
                                    additional_scoring_measures = NULL,
                                    file_used = NULL, ...) {

  answer_meta_data <- input$answer_meta_data

  if(is.character(answer_meta_data) && length(answer_meta_data) > 1L) {
    answer_meta_data <- named_chr_v_to_tib(answer_meta_data)
  } else {
    answer_meta_data <- tibble::as_tibble(answer_meta_data)
  }

  if("ngrukkon" %in% names(answer_meta_data)) {
    answer_meta_data <- answer_meta_data %>%
      dplyr::rename(ngrukkon_with_parent_melody = ngrukkon)
  }

  # Grab MIDI-specific data if available
  if(length(input$user_response_midi_note_off) == 0) {
    user_response_midi_note_off <- NA
    onsets_noteoff <- NA
  } else {
    user_response_midi_note_off <- as.numeric(jsonlite::fromJSON(input$user_response_midi_note_off))
    onsets_noteoff <- as.numeric(jsonlite::fromJSON(input$onsets_noteoff))
  }

  # Grab stimuli information
  if(is.scalar.null(input$stimuli)) {
    stimuli <- jsonlite::fromJSON(psychTestR::get_global("stimuli", state))
    stimuli_durations <- jsonlite::fromJSON(psychTestR::get_global("stimuli_durations", state))
  } else {
    stimuli <- as.numeric(jsonlite::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(jsonlite::fromJSON(input$stimuli_durations))
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
                                     answer_meta_data = answer_meta_data,
                                     as_tb = FALSE,
                                     additional_scoring_measures = additional_scoring_measures)

  # Store scores for final scores at end of test. Perhaps now deprecated
  # But could be useful to standardise final scores across tests (i.e., so each test doesn't need it's own final_results function)

  ongoing_scores <- psychTestR::get_global("scores", state)
  psychTestR::set_global("scores", c(ongoing_scores, scores$opti3), state)

  results <- c(
    list(file = if(is.null(file_used)) get_audio_file(input, state) else file_used,
         error = FALSE,
         attempt = if(length(input$attempt) == 0) NA  else as.numeric(input$attempt),
         user_satisfied = if(is.null(input$user_satisfied)) NA else input$user_satisfied,
         user_rating = if(is.null(input$user_rating)) NA else input$user_rating,
         melconv_notes = melconv_res$notes,
         melconv_durations = melconv_res$durations),
        # Append scores
         scores
      )

  results
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
  stimulus_trigger_times <- if(is.null(input$stimulus_trigger_times)) NA else (as.numeric(jsonlite::fromJSON(input$stimulus_trigger_times)) - trial_start_time_timecode) / 1000
  # We just assume the last duration is 0.5 always. There is no way of telling when the participant really designates that a "hit" is over
  # Technically you can do this with a keyboard noteoff, but this is complicated for various reasons.
  stimulus <- if(is.null(input$stimuli)) NA else as.numeric(jsonlite::fromJSON(input$stimuli))

  if(is.scalar.na.or.null(stimulus)) {
    last_dur <- 0.5
  } else {
    last_dur <- stimulus[length(stimulus)]
  }

  onsets <- if(is.scalar.na.or.null(onset_res)) NA else onset_res$onset
  durs <- if(is.scalar.na.or.null(onset_res)) NA else c(diff(onsets), last_dur)

  logging::loginfo("Onsets: %s", paste0(round(onsets, 2), collapse = " | "))

  list(
    file = new_audio_file,
    onset = onsets,
    dur = durs,
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

  args <- c(user_response_midi_note_on,
            user_response_midi_note_off,
            onsets_noteon,
            onsets_noteoff)

  if(is.null(args)) {
    return(TRUE)
  }

  if(purrr::some(args, function(x) {
             length(x) == 0
  })) {
    return(TRUE)
  }

  lengths <- c(length(jsonlite::fromJSON(user_response_midi_note_on)),
               length(jsonlite::fromJSON(onsets_noteon)))

  are_lengths_equal <- lengths[1] == lengths[2]

  any(lengths == 0) | ! are_lengths_equal
}


#' Store answer meta data
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_meta_data <- function(input, ...) {

  amd <- input$answer_meta_data

  if(length(amd) == 0) {
    amd <- list(answer_meta_data = NA)
    return(amd)
  } else if(is.scalar.character(amd)) {
    amd <- jsonlite::fromJSON(amd)
    return(amd)
  } else{
    return(amd)
  }

}

#' Get answer
#'
#' @param input
#' @param state
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_add_trial_and_compute_trial_scores_s3 <- function(input, state, ...) {

  logging::loginfo("get_answer_add_trial_and_compute_trial_scores")

  csv_file <- paste0(input$key, ".csv")

  logging::loginfo("csv_file: %s", csv_file)

  psychTestR::set_global("wav_file", input$key, state)
  psychTestR::set_global("musicxml_file", input$musicxml_file, state)

  list(
    user_satisfied = if(is.null(input$user_satisfied)) NA else input$user_satisfied,
    user_rating = if(is.null(input$user_rating)) NA else input$user_rating,
    attempt = if(length(input$attempt) == 0) NA else as.numeric(input$attempt),
    csv_file = csv_file,
    wav_file = input$key,
    musicxml_file = input$musicxml_file
  )


}






#' Estimate syllable from an audio file
#'
#' @param input
#' @param state
#'
#' @return
#' @export
#'
#' @examples
get_answer_syllable_classification <- function(input,
                                               state, ...) {

  logging::loginfo("Get syllable classification")


  audio_file <- get_audio_file(input, state)

  valid_file <- FALSE

  while(!valid_file) {
    valid_file <- file.exists(audio_file)
    logging::loginfo("valid_file? %s", valid_file)
  }

  # new_audio_file <-  paste0("www/audio/new_", basename(audio_file))

  #system(paste0("ffmpeg -i ", audio_file, " -ar 16000 ", new_audio_file))

  # # Get audio features
  # audio_features <- extract_audio_features(new_audio_file)
  #
  # pyin_res <- pyin::pyin(new_audio_file, normalise = TRUE)
  #
  # note <- pyin_res$note
  #
  # if(length(note) > 1L) {
  #   note <- note[1]
  # }
  #
  # loadNamespace("workflows")
  #
  # audio_features <- audio_features %>%
  #   dplyr::mutate(midi_note = note) %>%
  #   dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ purrr::map_dbl(., as.numeric)))  %>%
  #   dplyr::select(-c(spectral_entropy_ndsi:spectral_entropy_biophony,
  #                    acoustic_diversity_index, acoustic_diversity_index2))

  # # audio_features
  # preds <- predict(lyricassessr::mlp_wflow_fit,
  #                  new_data = audio_features, type = "prob") %>%
  #   dplyr::rename_with(~stringr::str_remove_all(.x, ".pred_")) %>%
  #   tidyr::pivot_longer(dplyr::everything(),
  #                       names_to = "Syllable", values_to = "Probability") %>%
  #   dplyr::arrange(dplyr::desc(Probability))


  preds <-
    lyricassessr::extract_syllables(audio_file)


  list(syllable_probabilities = preds,
       no_syllables_detected = nrow(preds))

}


#' Get answer async midi vs audio
#'
#' @param input
#' @param state
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
get_answer_async_midi_vs_audio <- function(input, state, ...) {

  res_type <- psychTestR::get_global("response_type", state)

  logging::loginfo("response_type: %s", res_type)

  singing_trial <- psychTestR::get_global("singing_trial", state)

  logging::loginfo("singing_trial: %s", singing_trial)

  if(singing_trial || res_type == "Microphone") {

    res <- get_answer_add_trial_and_compute_trial_scores_s3(input, state, ...)

  } else if(res_type == "MIDI") {

    res <- get_answer_midi_melodic_production(input, state, ...)

  } else {
    stop("response_type invalid")
  }

  logging::loginfo("End of get_answer_async_midi_vs_audio")

  return(res)

}
