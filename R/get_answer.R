


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


  pyin_res <- get_answer_pyin(input, type,  state, melconv, ...)

  if(is.na(pyin_res$pyin_res)) {

    res <- list(
      error = TRUE,
      reason = "there was nothing in the pitch track",
      user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
      user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
      attempt = ifelse(length(input$attempt) == 0, NA, as.numeric(input$attempt)),
      opti3 = NA,
      answer_meta_data = tibble::as_tibble(input$answer_meta_data),
      stimuli = as.numeric(rjson::fromJSON(input$stimuli)))

  } else {

    res <- concat_mel_prod_results(input,
                                   state,
                                   pyin_res$melconv_res,
                                   pyin_res$pyin_res$freq,
                                   pyin_res$pyin_res$note,
                                   pyin_res$pyin_res$dur,
                                   pyin_res$pyin_res$onset,
                                   pyin_res$pyin_pitch_track,
                                   additional_scoring_measures)

  }

  store_results_in_db(state, res, pyin_res)

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


  if(is_na_length_1(pyin_res)) {

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
         onset = if(is_na_length_1(pyin_res)) NA else pyin_res$onset,
         freq = if(is_na_length_1(pyin_res)) NA else pyin_res$freq,
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

  audio_file <- get_audio_file_for_pyin(input, state)

  pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)

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

  audio_file <- paste0('www/audio/', input$file_url)

  audio_file
}

get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
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
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
      if(is.null(pyin_res$freq) | is.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        audio_file <- get_audio_file_for_pyin(input, state)
        pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
        freqs <- pyin_res$freq
        list(user_response = ceiling(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin::pyin(audio_file, if_bad_result_return_single_na = FALSE)
      if(is.null(pyin_res$freq) | is.logical(pyin_res$freq)) {
        cat(file=stderr(), 'get_answer_average_frequency_ff4')
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




# midi-related

check_midi_melodic_production_lengths <- function(user_response_midi_note_on,
                                             user_response_midi_note_off,
                                             onsets_noteon,
                                             onsets_noteoff) {

  length1 <- length(rjson::fromJSON(user_response_midi_note_on))
  length2 <- length(rjson::fromJSON(user_response_midi_note_off))
  length3 <- length(rjson::fromJSON(onsets_noteon))
  length4 <- length(rjson::fromJSON(onsets_noteoff))

  equal_lengths <- length1 == length2 & length2 == length3 & length3 == length4

  length1 == 0 | length2 == 0 | length3 == 0 | length4 == 0 | !equal_lengths
}


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
                reason = "no midi notes / lengths unequal",
                user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
                user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
                attempt = ifelse(length(input$attempt) == 0, NA, as.numeric(input$attempt)),
                opti3 = NA,
                answer_meta_data = tibble::as_tibble(input$answer_meta_data),
                stimuli = as.numeric(rjson::fromJSON(input$stimuli)))

     } else {

       midi_res <- get_answer_midi(input, state, ...)


       res <- concat_mel_prod_results(input,
                                      state,
                                      melconv_res = list(notes = NA, durations = NA),
                                      user_melody_freq = pyin_res$pyin_res$freq,
                                      user_melody_input = midi_res$user_response_midi_note_on,
                                      user_duration_input = midi_res$pyin_style_res$dur,
                                      user_onset_input = midi_res$onsets_noteon,
                                      pyin_pitch_track = midi_res$pyin_style_res)

     }


  store_results_in_db(state, res, midi_res$pyin_style_res)
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


  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {

    list(
      user_response_midi_note_on = NA,
      user_response_midi_note_off =  NA,
      onsets_noteon =  NA,
      onsets_off = NA,
      pyin_style_res = NA)

  } else {

    notes <- as.numeric(rjson::fromJSON(input$user_response_midi_note_on))
    notes_off <- as.numeric(rjson::fromJSON(input$user_response_midi_note_off))
    onsets <- as.numeric(rjson::fromJSON(input$onsets_noteon))/1000
    onsets_off <- as.numeric(rjson::fromJSON(input$onsets_noteoff))/1000


    durs <- onsets_off - onsets

    pyin_style_res <- tibble::tibble(
      onset = onsets,
      dur = durs,
      freq = hrep::midi_to_freq(notes),
      note = notes,
      file_name = NA
    ) %>%
      dplyr::relocate(file_name)


    list(
      user_response_midi_note_on = notes,
      user_response_midi_note_off =  notes_off,
      onsets_noteon =  onsets,
      onsets_off = onsets_off,
      pyin_style_res = pyin_style_res)
  }

}


get_answer_midi_note_mode <- function(input, state, ...) {

  if(is.null(input$user_response_midi_note_on)) {
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

concat_mel_prod_results <- function(input,
                                    state,
                                    melconv_res,
                                    user_melody_freq = NULL, # Can be null if MIDI
                                    user_melody_input,
                                    user_duration_input,
                                    user_onset_input,
                                    pyin_pitch_track,
                                    additional_scoring_measures = NULL, ...) {

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


  scores <- score_melodic_production(user_melody_freq,
                                     user_melody_input,
                                     user_duration_input,
                                     user_onset_input,
                                     stimuli,
                                     stimuli_durations,
                                     pyin_pitch_track,
                                     user_response_midi_note_off,
                                     onsets_noteoff,
                                     tibble::as_tibble(input$answer_meta_data),
                                     as_tb = FALSE,
                                     additional_scoring_measures)

  # for final scores at end of test - currently not in usage,
  # but could be useful to standardise final scores across tests (i.e., so each test doesn't need it's own final_results function)
  ongoing_scores <- psychTestR::get_global("scores", state)
  psychTestR::set_global("scores", c(ongoing_scores, scores$opti3), state)


  c(

    list(file = get_audio_file_for_pyin(input, state),
         attempt = ifelse(length(input$attempt) == 0, NA, as.numeric(input$attempt)),
         user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
         user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
         melconv_notes = melconv_res$notes,
         melconv_durations = melconv_res$durations),

    scores)
}



store_results_in_db <- function(state, res, pyin_res) {

  store_results_in_db <- psychTestR::get_global("store_results_in_db", state)

  if(store_results_in_db) {

    session_info <- psychTestR::get_session_info(state, complete = FALSE)
    test_username <- psychTestR::get_global("test_username", state)
    test <- psychTestR::get_local("test", state)

    if(is.null(test)) {
      test <- psychTestR::get_global("test", state)
    }

    add_trial_to_db(test_username = test_username,
                    test = test,
                    session_id = session_info$p_id,
                    melody = paste0(res$stimuli, collapse = ","),
                    res$opti3,
                    pyin_res,
                    res$answer_meta_data$N,
                    res$answer_meta_data$step.cont.loc.var,
                    res$answer_meta_data$tonalness,
                    res$answer_meta_data$log_freq,
                    res$attempt)
  }
}



# test
# #r <- melconv_from_pyin_res('/Users/sebsilas/true.wav')


