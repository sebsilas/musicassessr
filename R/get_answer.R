#' Get answer by running pyin on a recorded audio file
#'
#' @param input
#' @param type
#' @param state
#' @param melconv
#' @param singing_measures
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin <- function(input, type = c("both", "note", "pitch_track"), state, melconv = FALSE,
                            singing_measures = TRUE, pyin_pitch_track = NULL, ...) {

  musicassessr_state <- ifelse(exists("musicassessr_state"), musicassessr_state, "production")

  file_dir <- ifelse(musicassessr_state == "production",
                     '/srv/shiny-server/files/',
                     '/Users/sebsilas/aws-musicassessr-local-file-upload/files/')

  file <- paste0(file_dir, input$key, '.wav')

  if(type == "note") {
    pyin_res <- pyin(file)
  } else if(type == "pitch_track") {
    pyin_pitch_track <- pyin(file, type = "pitch_track")
  } else {
    pyin_res <- pyin(file)
    pyin_pitch_track <- pyin(file, type = "pitch_track")
  }

  if(is.null(psychTestR::get_global("melody", state))) {
    stimuli <- as.numeric(rjson::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(rjson::fromJSON(input$stimuli_durations))
  } else {
    stimuli_both <- psychTestR::get_global("melody", state)
    stimuli <- stimuli_both$melody
    stimuli_durations <- stimuli_both$durations
    stimuli_durations <- ifelse(!is.na(stimuli_durations) | !is.null(stimuli_durations), stimuli_durations, NA)
  }

  if(melconv) {
    melconv_res <- melconv_from_pyin_res(pyin_res)
    melconv_notes <- itembankr::str_mel_to_vector(melconv_res$notes)
    melconv_dur <- itembankr::str_mel_to_vector(melconv_res$dur)
  } else {
    melconv_notes <- NA
    melconv_dur <- NA
  }

  if(is.na(pyin_res$onset)) {

    res <- list(error = NA, reason = "pyin returned no result", user_satisfied = input$user_satisfied)

  } else {

    res <- c(
      list(file = file,
           user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
           user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating),
           melconv_notes = melconv_notes,
           melconv_dur = melconv_dur
      ),

      melody_scoring_from_user_input(input, result = if(!is.null(pyin_res)) pyin_res, trial_type = "audio", singing_measures = singing_measures,
                                     pyin_pitch_track = if(!is.null(pyin_pitch_track)) pyin_pitch_track, stimuli = stimuli, stimuli_durations = stimuli_durations)
    )
  }

  # store_results_in_db <- psychTestR::get_global("store_results_in_db", state)
  # store_results_in_db <- ifelse(is.null(store_results_in_db), FALSE, TRUE)
  # print('store_results_in_db?')
  # print(store_results_in_db)
  # if(store_results_in_db) {
  #   session_info <- psychTestR::get_session_info(state, complete = FALSE)
  #   print(session_info)
  #   add_trial_to_db(test_username = psychTestR::get_global("test_username", state),
  #                   test = psychTestR::get_global("test", state),
  #                   session_id = session_info$p_id,
  #                   time_started = session_info$current_time - res$trial_length,
  #                   time_completed = session_info$current_time,
  #                   melody = paste0(res$stimuli, collapse = ","),
  #                   res$similarity,
  #                   res$accuracy_octaves_allowed,
  #                   pyin_res)
  # }

  res


}


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


#' Get mel production scores, but without singing/pitch accuracy measures
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
get_answer_pyin_mel_prod_only <- function(input, type = c("both", "note", "pitch_track"), state, melconv = FALSE, ...) {
  get_answer_pyin(input, type, state, melconv = FALSE, singing_measures = FALSE, ...)
}



get_answer_pyin_note_only <- function(input, type = "note", state, melconv = FALSE, ...) {
  get_answer_pyin(input, type = "note", state, melconv = FALSE, ...)
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
get_answer_pyin_long_note <- function(input, ...) {

  print('get_answer_pyin_long_note')

  musicassessr_state <- ifelse(exists("musicassessr_state"), musicassessr_state, "production")

  file_dir <- ifelse(musicassessr_state == "production",
                     '/srv/shiny-server/files/',
                     '/Users/sebsilas/aws-musicassessr-local-file-upload/files/')

  file <- paste0(file_dir, input$key, '.wav')

  pyin_res <- pyin(file, type = "pitch_track")
  print(pyin_res)

  if(is.na(pyin_res$onset)) {
    long_note_pitch_measures <- list("note_accuracy" = NA,
                                     "note_precision" = NA,
                                     "dtw_distance" = NA)

  } else {
    long_note_pitch_measures <- long_note_pitch_metrics(as.numeric(input$stimuli), pyin_res)
  }
  print('do one..')
  print(
    c(
      list(file = file,
           stimuli = as.numeric(input$stimuli),
           onset = pyin_res$onset,
           freq = pyin_res$freq
      ),

      long_note_pitch_measures
    )
  )

  c(
    list(file = file,
         stimuli = as.numeric(input$stimuli),
         onset = pyin_res$onset,
         freq = pyin_res$freq
         ),

    long_note_pitch_measures
  )


}

# get_answer functions

#' Dummy get_answer function
#'
#' @param input
#' @param state
#' @param text
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_null <- function(input, state, text = "example_page", ...) {
  print("in get answer null")
  list(text)
}


get_answer_midi_note_mode <- function(input, state, ...) {
  print('get_answer_midi_note_mode')
  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {
    list(note = NA)
  } else {
    list(note = getmode(rjson::fromJSON(input$user_response_midi_note_on)))
  }
}


get_answer_simple_pyin_summary <- function(input, ...) {
  print('get_answer_simple_pyin_summary')

  musicassessr_state <- ifelse(exists("musicassessr_state"), musicassessr_state, "production")

  file_dir <- ifelse(musicassessr_state == "production",
                     '/srv/shiny-server/files/',
                     '/Users/sebsilas/aws-musicassessr-local-file-upload/files/')

  file <- paste0(file_dir, input$key, '.wav')
  pyin_res <- pyin(file)

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
  print('get_answer_midi')

  user_response_midi_note_on <- rjson::fromJSON(input$user_response_midi_note_on)

  if(is.null(psychTestR::get_global("melody", state))) {
    stimuli <- as.numeric(rjson::fromJSON(input$stimuli))
    stimuli_durations <- as.numeric(rjson::fromJSON(input$stimuli_durations))
  } else {

    stimuli_both <- psychTestR::get_global("melody", state)
    stimuli <- stimuli_both$melody
    stimuli_durations <- stimuli_both$durations
    stimuli_durations <- ifelse(!is.na(stimuli_durations) | !is.null(stimuli_durations), stimuli_durations, NA)
  }

  c(melody_scoring_from_user_input(input = input,
                                 user_melody_input = user_response_midi_note_on,
                                 trial_type = "midi", singing_measures = FALSE,
                                 stimuli = stimuli, stimuli_durations = stimuli_durations),
    user_satisfied = ifelse(is.null(input$user_satisfied), NA, input$user_satisfied),
    user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating))
}


get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, ...) {
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = floor(mean(notes)))
      }
    }

  } else if (floor_or_ceiling == "ceiling") {

    function(input, ...) {
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = ceiling(mean(notes)))
      }
    }

  } else {

    function(input, ...) {
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = round(mean(notes)))
      }
    }
  }

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


get_answer_store_async_long_note <- function(...) {
  get_answer_store_async(scoring = "long_note", ...)
}


# do <- cbind(WJD[5, ], data.frame(abs_melody = "64,62,57"))
# ta <- melody_scoring_from_user_input(input = list(answer_meta_data = do),
#                                      user_melody_input = 60:65,
#                                      onsets_noteon = 1:5,
#                                      trial_type = "audio"
#                                      )

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
# #r <- melconv_from_pyin_res('/Users/sebsilas/true.wav')


