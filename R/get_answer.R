
# pyin-related

#' Analyse a melody with pyin then compute melodic production scores
#'
#' @param input
#' @param type
#' @param state
#' @param melconv
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin_melodic_production <- function(input,
                                               type = c("both", "notes", "pitch_track"),
                                               state,
                                               melconv = FALSE, ...) {


  pyin_res <- get_answer_pyin(input, type,  state, melconv, ...)


  if(is.na(pyin_res$pyin_res$onset) | is.null(pyin_res$pyin_res)) {

    return(list(error = TRUE,
                reason = "there was nothing in the pitch track",
                user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
                user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating)))

  } else {

    res <- concat_mel_prod_results(input,
                                   state,
                                   pyin_res$melconv_res,
                                   pyin_res$pyin_res$note,
                                   pyin_res$pyin_res$dur,
                                   pyin_res$pyin_res$onset,
                                   pyin_res$pyin_pitch_track)

    store_results_in_db(state, res)
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

  audio_file <- get_audio_file_for_pyin(input, state)

  pyin_res <- pyin::pyin(audio_file, type = "pitch_track")


  if(is.na(pyin_res$onset)) {
    long_note_pitch_measures <- list("note_accuracy" = NA,
                                     "note_precision" = NA,
                                     "dtw_distance" = NA,
                                     "agg_dv_long_note" = NA,
                                     "na_count" = NA,
                                     "dtw_distance_max" = NA,
                                     "note_accuracy_max" = NA,
                                     "note_precision_max" = NA,
                                     "var" = NA,
                                     "run_test" = NA,
                                     "freq_max" = NA,
                                     "freq_min" = NA,
                                     "autocorrelation_mean" = NA)
    noise.classification <- list(prediction = "noise", failed_tests = 'na_count')

  } else {
    long_note_pitch_measures <- long_note_pitch_metrics(as.numeric(input$stimuli), pyin_res$freq)
    noise.classification <- classify_whether_noise(long_note_pitch_measures)
  }


  display_noise_trial_message(noise.classification, state)

  c(
    list(file = audio_file,
         stimuli = as.numeric(input$stimuli),
         onset = pyin_res$onset,
         freq = pyin_res$freq,
         noise_classification = noise.classification$prediction,
         failed_tests = noise.classification$failed_tests),
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin <- function(input,
                            type = c("both", "notes", "pitch_track"),
                            state,
                            melconv = FALSE, ...) {

  # get file
  audio_file <- get_audio_file_for_pyin(input, state)

  # get pyin
  pyin_res <- get_pyin(audio_file, type, state)

  # melconv
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
  print('get_answer_simple_pyin_summary')

  audio_file <- get_audio_file_for_pyin(input, state)

  pyin_res <- pyin::pyin(audio_file)

  if(is.na(pyin_res$note)) {
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





get_audio_file_for_pyin <- function(input, state, ...) {

  audio_file <- psychTestR::get_global("copy_to_location", state)

  if(length(audio_file) == 0) {
    audio_file <- paste0(system.file('node/files', package = 'musicassessr'), '/', input$key, '.wav')
  } else {
    audio_file <- paste0(audio_file, '/', input$key, '.wav')
  }

  print('get_audio_file_for_pyin')
  print(audio_file)

  audio_file
}

get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin::pyin(audio_file)
      if(is.null(pyin_res$freq) | is.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        freqs <- pyin_res$freq
        list(user_response = floor(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else if (floor_or_ceiling == "ceiling") {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin::pyin(audio_file)
      if(is.null(pyin_res$freq) | is.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        audio_file <- get_audio_file_for_pyin(input, state)
        pyin_res <- pyin::pyin(audio_file)
        freqs <- pyin_res$freq
        list(user_response = ceiling(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin::pyin(audio_file)
      if(is.null(pyin_res$freq) | is.logical(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        freqs <- pyin_res$freq
        list(user_response = round(mean(hrep::freq_to_midi(freqs))))
      }
    }
  }
}

get_pyin <- function(audio_file, type, state) {

  print('get_pyin...')
  print('audio_file...')
  print(audio_file)

  if(type == "notes") {
    pyin_res <- pyin::pyin(audio_file)
    list("pyin_res" = pyin_res, "pyin_pitch_track" = NA)
  } else if(type == "pitch_track") {
    pyin_pitch_track <- pyin::pyin(audio_file, type = "pitch_track")
    list("pyin_res" = NA, "pyin_pitch_track" = pyin_pitch_track)
  } else {
    pyin_res <- pyin::pyin(audio_file, type = "notes")

    pyin_pitch_track <- pyin::pyin(audio_file, type = "pitch_track")

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




# midi-related


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

  if(is.null(input$user_response_midi_note_on)) {

    return(list(error = TRUE,
                reason = "no midi notes",
                user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
                user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating)))

  } else {

    midi_res <- get_answer_midi(input, state, ...)

    res <- concat_mel_prod_results(input,
                                   state,
                                    melconv_res = list(notes = NA, durations = NA),
                                    midi_res$user_response_midi_note_on,
                                    c(diff(midi_res$onsets_noteon), midi_res$onsets_noteon[length(midi_res$onsets_noteon)]-midi_res$onsets_noteon[length(midi_res$onsets_noteon)]),
                                    midi_res$onsets_noteon,
                                    tibble::tibble())

    store_results_in_db(state, res)
  }
  res

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
  list(
    user_response_midi_note_on = as.numeric(rjson::fromJSON(input$user_response_midi_note_on)),
    user_response_midi_note_off =  as.numeric(rjson::fromJSON(input$user_response_midi_note_off)),
    onsets_noteon =  as.numeric(rjson::fromJSON(input$onsets_noteon))
  )

}


get_answer_midi_note_mode <- function(input, state, ...) {
  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {
    list(note = NA)
  } else {
    list(note = getmode(rjson::fromJSON(input$user_response_midi_note_on)))
  }
}

# other

get_answer_interval_page <- function(input, state, ...) {

  answer_meta_data <- psychTestR::get_global("answer_meta_data", state)
  msg <- ifelse(input$dropdown == answer_meta_data$interval, "Correct Answer!", "Wrong Answer!")
  shiny::showNotification(msg)

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
get_answer_save_aws_key <- function(input, ...) {
  list(key = input$key,
       file_url = input$file_url,
       user_satisfied = input$user_satisfied,
       user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating))
}



# generic

concat_mel_prod_results <- function(input, state, melconv_res, user_melody_input, user_duration_input,
                                    user_onset_input, pyin_pitch_track, ...) {

  if(length(input$user_response_midi_note_off) == 0) {
    user_response_midi_note_off <- NA
    onsets_noteoff <- NA
  } else {
    user_response_midi_note_off <- as.numeric(rjson::fromJSON(input$user_response_midi_note_off))
    onsets_noteoff <- as.numeric(rjson::fromJSON(input$onsets_noteoff))
  }


  if(is.null(input$stimuli)) {
    stimuli <- rjson::fromJSON(psychTestR::get_global("stimuli", state))
    stimuli_durations <- rjson::fromJSON(psychTestR::get_global("stimuli_durations", state))
  } else {
    stimuli <- as.numeric(rjson::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(rjson::fromJSON(input$stimuli_durations))
  }


  scores <- score_melodic_production(user_melody_input,
                                     user_duration_input,
                                     user_onset_input,
                                     stimuli,
                                     stimuli_durations,
                                     pyin_pitch_track,
                                     user_response_midi_note_off,
                                     onsets_noteoff,
                                     tibble::as_tibble(input$answer_meta_data))

  # for final scores at end of test - currently not in usage,
  # but could be useful to standardise final scores across tests (i.e., so each test doesn't need it's own final_results function)
  ongoing_scores <- psychTestR::get_global("scores", state)
  psychTestR::set_global("scores", c(ongoing_scores, scores$opti3), state)

  c(

    list(file = get_audio_file_for_pyin(input, state),
         user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
         user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
         melconv_notes = melconv_res$notes,
         melconv_durations = melconv_res$durations),

        scores)
}



store_results_in_db <- function(state, res) {

  store_results_in_db <- psychTestR::get_global("store_results_in_db", state)

  if(store_results_in_db) {
    session_info <- psychTestR::get_session_info(state, complete = FALSE)

    add_trial_to_db(test_username = psychTestR::get_global("test_username", state),
                    test = psychTestR::get_global("test", state),
                    session_id = session_info$p_id,
                    time_started = session_info$current_time - res$trial_length,
                    time_completed = session_info$current_time,
                    melody = paste0(res$stimuli, collapse = ","),
                    res$similarity,
                    res$accuracy_octaves_allowed,
                    pyin_res)
  }
}



# test
# #r <- melconv_from_pyin_res('/Users/sebsilas/true.wav')


