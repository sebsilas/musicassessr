
# pyin-related

get_answer_pyin_melodic_production <- function(input,
                                               type = c("both", "note", "pitch_track"),
                                               state,
                                               melconv = FALSE, ...) {

  pyin_res <- get_answer_pyin(input, type,  state, melconv, ...)

  if(is.na(pyin_res$pyin_res) | is.null(pyin_res$pyin_res)) {

    return(list(error = TRUE, reason = "pyin returned no result"))

  } else {
    res <- concat_mel_prod_results(input,
                                   pyin_res$melconv_res,
                                   pyin_res$pyin_res$note,
                                   pyin_res$pyin_res$dur,
                                   pyin_res$pyin_res$onset,
                                   rjson::fromJSON(input$stimuli),
                                   rjson::fromJSON(input$stimuli_durations),
                                   pyin_res$pyin_pitch_track)

    store_results_in_db(state, res)
  }

  res

}



get_answer_pyin_note_only <- function(input, type = "note", state, ...) {
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

  print('get_answer_pyin_long_note')

  audio_file <- get_audio_file_for_pyin(input, state)

  pyin_res <- pyin(audio_file,
                   sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state),
                   type = "pitch_track")

  if(is.na(pyin_res$onset)) {
    long_note_pitch_measures <- list("note_accuracy" = NA,
                                     "note_precision" = NA,
                                     "dtw_distance" = NA)

  } else {
    long_note_pitch_measures <- musicassessr::long_note_pitch_metrics(as.numeric(input$stimuli), pyin_res)
  }

  c(
    list(file = file,
         stimuli = as.numeric(input$stimuli),
         onset = pyin_res$onset,
         freq = pyin_res$freq
    ),

    long_note_pitch_measures
  )


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
                            type = c("both", "note", "pitch_track"),
                            state,
                            melconv = FALSE, ...) {

  # get file
  audio_file <- get_audio_file_for_pyin(input, state)

  # get stimuli
  stimuli <- as.numeric(rjson::fromJSON(input$stimuli))
  stimuli_durations <- as.numeric(rjson::fromJSON(input$stimuli_durations))

  # get pyin
  pyin_res <- get_pyin(audio_file, type, state)

  # melconv
  melconv_res <- get_melconv(melconv, pyin_res)

  list(pyin_res = pyin_res$pyin_res,
       pyin_pitch_track = pyin_res$pyin_pitch_track,
       melconv_res = melconv_res)

}



get_answer_simple_pyin_summary <- function(input, state, ...) {
  print('get_answer_simple_pyin_summary')

  audio_file <- get_audio_file_for_pyin(input, state)

  pyin_res <- pyin(audio_file,
                   sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))

  res <- ifelse(is.na(pyin_res$note),
                yes = list("Min." = NA,
                           "1st Qu." = NA,
                           "Median" = NA,
                           "Mean" = NA,
                           "3rd Qu." = NA,
                           "Max." = NA),
                no = as.list(round(summary(pyin_res$note))))

  res$file <- file
  res
}





get_audio_file_for_pyin <- function(input, state, ...) {

  file_dir <- get_correct_app_dir(state)

  audio_file <- paste0(file_dir, input$key, '.wav')
}

get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))
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
      pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))
      if(is.null(pyin_res$freq) | is.na(pyin_res$freq)) {
        list(user_response = NA)
      } else {
        audio_file <- get_audio_file_for_pyin(input, state)
        pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))
        freqs <- pyin_res$freq
        list(user_response = ceiling(mean(hrep::freq_to_midi(freqs))))
      }
    }

  } else {

    function(input, state, ...) {
      audio_file <- get_audio_file_for_pyin(input, state)
      pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))
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


  if(type == "note") {
    pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state))
    list("pyin_res" = pyin_res, "pyin_pitch_track" = NA)
  } else if(type == "pitch_track") {
    pyin_pitch_track <- pyin(audio_file,
                             sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state),
                             type = "pitch_track")
    list("pyin_res" = NA, "pyin_pitch_track" = pyin_pitch_track)
  } else {
    pyin_res <- pyin(audio_file, sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state),
                     type = "note")

    pyin_pitch_track <- pyin(audio_file,
                             sonic_annotator_location = get_correct_sonic_annotator_location_musicassessr(state),
                             type = "pitch_track")

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


get_answer_midi_melodic_production <- function(input, state, ...) {

  midi_res <- get_answer_midi(input, state, ...)

  if(is.na(midi_res$user_response_midi_note_on) | is.null(midi_res$user_response_midi_note_on)) {

    return(list(error = TRUE, reason = "no midi notes"))

  } else {
    res <- concat_mel_prod_results(input,
                          melconv_res = list(notes = NA, durations = NA),
                          user_response_midi_note_on,
                          diff(onsets_noteon),
                          onsets_noteon,
                          stimuli,
                          stimuli_durations,
                          pyin_pitch_track)

    store_results_in_db(state, res)
  }

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
    user_response_midi_note_on = rjson::fromJSON(input$user_response_midi_note_on),
    user_response_midi_note_off =  rjson::fromJSON(input$user_response_midi_note_off),
    onsets_noteon =  rjson::fromJSON(input$onsets_noteon),
    onsets_noteoff = rjson::fromJSON(input$onsets_noteoff)
  )

}


get_answer_midi_note_mode <- function(input, state, ...) {
  print('get_answer_midi_note_mode')
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

concat_mel_prod_results <- function(
  input,
  melconv_res,
  user_melody_input,
  user_duration_input,
  user_onset_input,
  stimuli,
  stimuli_durations,
  pyin_pitch_track) {

  c(

    list(file = file,
         user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
         user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
         melconv_notes = melconv_res$notes,
         melconv_durations = melconv_res$durations),

    score_melodic_production(user_melody_input,
                             user_duration_input,
                             user_onset_input,
                             stimuli,
                             stimuli_durations,
                             pyin_pitch_track,
                             input$answer_meta_data))
}



store_results_in_db <- function(state, res) {

  print('store_results_in_db')

  store_results_in_db <- psychTestR::get_global("store_results_in_db", state)
  print(store_results_in_db)

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


