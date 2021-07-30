#' A page builder for creating a specified number of play_melody_until_satisfied_loops
#'
#' @param n_items
#' @param var_name
#' @param stimuli_type
#' @param page_type
#' @param max_goes
#' @param page_text
#' @param get_answer
#' @param rel_to_abs_mel_function
#' @param start_from_trial_no
#'
#' @return
#' @export
#'
#' @examples
build_multi_page_play_melody_until_satisfied_loop <- function(n_items, var_name = "melody", stimuli_type = "midi_notes",
                                                              page_type = "record_audio_page", max_goes = 3,
                                                              page_title = psychTestR::i18n("copy_melody_title"),
                                                              page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                                              get_answer = get_answer_save_aws_key, rel_to_abs_mel_function,
                                                              start_from_trial_no = 1, clip_stimuli_length = FALSE,
                                                              arrhythmic = FALSE, example = FALSE, feedback = FALSE) {

  # items should be a dataframe
  # this will return a sequence of test items
  items <- lapply(start_from_trial_no:n_items, function(melody_no) {
    play_melody_until_satisfied_loop(melody_no = melody_no,
                                     var_name = var_name,
                                     max_goes = max_goes,
                                     page_type = page_type,
                                     page_title = page_title,
                                     page_text = page_text,
                                     get_answer = get_answer,
                                     stimuli_type = stimuli_type,
                                     rel_to_abs_mel_function = rel_to_abs_mel_function,
                                     clip_stimuli_length = clip_stimuli_length,
                                     arrhythmic = arrhythmic,
                                     example = example)
  })

  items <- add_feedback(items, feedback)

}

#' Build multiple play_long_tone_record_audio_pages based on a user's range
#'
#' @param no_pages
#' @param bottom_range
#' @param top_range
#'
#' @return
#' @export
#'
#' @examples
build_multi_play_long_tone_record_audio_pages <- function(no_items, page_type = "record_audio_page", example = FALSE, feedback = FALSE) {
  items <- unlist(lapply(1:no_items, function(x) play_long_tone_record_audio_page(long_note_no = x, page_type = page_type, example = example)))
  items <- add_feedback(items, feedback)
}



#' Create a psychTestR test loop for having several attempts at playing back a melody.
#'
#' @param melody
#' @param melody_no
#' @param var_name
#' @param stimuli_type
#' @param max_goes
#' @param page_type
#' @param page_title
#' @param page_text
#' @param answer_meta_data
#' @param get_answer
#' @param rel_to_abs_mel_function
#'
#' @return
#' @export
#'
#' @examples
play_melody_until_satisfied_loop <- function(melody = NULL, melody_no = "x", var_name = "melody", stimuli_type = "midi_notes", max_goes = 3,
                                             page_type = "record_audio_page", page_title = "Copy The Melody", page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                             answer_meta_data = " ", get_answer = get_answer_store_async,
                                             rel_to_abs_mel_function = function(){}, clip_stimuli_length = FALSE,
                                             start_note = 1, end_note = "end", dur_list = 'null', arrhythmic = FALSE, note_length = 0.5,
                                             play_button_text = psychTestR::i18n("Play"), example = FALSE) {

  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }

  c(
    # set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      print('play_melody_until_satisfied_loop')
      # repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)

      # sample melody for this trial
      if(is.null(melody)) {
        print('sample melody because null')

        bottom_range <- psychTestR::get_global("bottom_range", state)
        top_range <- psychTestR::get_global("top_range", state)
        trials <- psychTestR::get_global(var_name, state)
        #PBET stuff:
        #trial_char <- get_trial_characteristics(trial_df = trials, trial_no = melody_no)
        # inst <- psychTestR::get_global("inst", state)
        # sample <- sample_melody_in_key(inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$difficulty, length = trial_char$melody_length)

        if(stimuli_type == "midi_file") {
          abs_melody <- trials[melody_no, "midi_file"]
          if(clip_stimuli_length) {
            start_note <- trials[melody_no, "start"]
            end_note <- trials[melody_no, "end"]
          }
          # transpose..
          psychTestR::set_global("melody", list("midi_file" = abs_melody,
                                                "start_note" = start_note,
                                                "end_note" = end_note
                                                ), state)
        } else {
          rel_melody <- trials[melody_no, ]
          if(arrhythmic) {
            melody <- itembankr::str_mel_to_vector(rel_melody$melody, ",")
            dur_list <- rep(note_length, length(melody)+1)
          } else {
            print('rhythmic..')
            dur_list <- itembankr::str_mel_to_vector(rel_melody$durations, ",")
          }

          abs_melody <- rel_to_abs_mel_function(rel_melody = rel_melody$melody, bottom_range = bottom_range, top_range = top_range)
          answer_meta_data <- cbind(rel_melody,
                                    data.frame(abs_melody = paste0(abs_melody, collapse = ","))
          )
          psychTestR::set_global("melody", list("melody" = abs_melody, "dur_list" = dur_list), state)
          psychTestR::set_global("answer_meta_data", rjson::toJSON(answer_meta_data), state)
        }
      }
    }),

    # keep in loop until the participant confirms they are happy with their entry
    psychTestR::while_loop(test = function(state, ...) {
      number_attempts <- psychTestR::get_global("number_attempts", state)
      user_answer <- psychTestR::get_global("user_satisfied", state)
      user_wants_to_play_again <- user_answer %in% dict_key_to_translations("Try_Again")
      user_wants_to_play_again
    },
    logic = list(
      psychTestR::reactive_page(function(state, ...) {

        number_attempts <- psychTestR::get_global("number_attempts", state)

        melody <- melody_checks(melody, state, stimuli_type)

        if(stimuli_type == "midi_file") {
            start_note <- melody$start_note
            end_note <- melody$end_note
            melody <- melody$midi_file
        }

        if(is.list(melody)) {
          dur_list <- melody$dur_list
          melody <- melody$melody
        }

        answer_meta_data <- psychTestR::get_global("answer_meta_data", state)

        if(page_type == "record_midi_page") {

          midi_device <- psychTestR::get_global("midi_device", state)
          if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

          present_stimuli(stimuli = melody,
                          stimuli_type = stimuli_type,
                          display_modality = "auditory",
                          page_title = page_title,
                          page_text = page_text,
                          page_type = page_type,
                          answer_meta_data = answer_meta_data,
                          get_answer = get_answer,
                          save_answer = save_answer,
                          midi_device = midi_device,
                          page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                          play_button_text = play_button_text,
                          start_note = start_note,
                          end_note = end_note,
                          dur_list = dur_list)

        } else {

          # page 1, play melody
          present_stimuli(stimuli = melody,
                          stimuli_type = stimuli_type,
                          display_modality = "auditory",
                          page_title = page_title,
                          page_text = page_text,
                          page_type = "record_audio_page",
                          record_audio_method = "aws_pyin",
                          answer_meta_data = answer_meta_data,
                          get_answer = get_answer,
                          save_answer = save_answer,
                          page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                          button_text = psychTestR::i18n("Record"),
                          play_button_text = play_button_text,
                          start_note = start_note,
                          end_note = end_note,
                          dur_list = dur_list)
        }

      }),

      # logic page 2, was the user ok with this response?
      psychTestR::reactive_page(function(answer, state, ...) {
        number_attempts <- psychTestR::get_global("number_attempts", state)
        attempts_left <- max_goes - number_attempts
        return_correct_attempts_left_page(attempts_left, var_name, number_attempts, page_title)
      }),

      psychTestR::code_block(function(state, answer, opt, ...) {
        psychTestR::set_global("user_satisfied", answer, state)
        number_attempts <- psychTestR::get_global("number_attempts", state)
        number_attempts <- number_attempts + 1
        psychTestR::set_global("number_attempts", number_attempts, state)
        psychTestR::save_results_to_disk(complete = FALSE, state, opt)
      })
    )
    ) # end psychTestR::while_loop
  ) # end join
}




get_note_until_satisfied_loop <- function(prompt_text, var_name, page_type, button_text = "Record") {

  c(
    # set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("user_satisfied", "No", state)
      psychTestR::set_global(var_name, NA, state)
    }),

    # keep in loop until the participant confirms the note is correct
    psychTestR::while_loop(test = function(state, ...) {
      user_satisfied <- psychTestR::get_global("user_satisfied", state)
      note <- psychTestR::get_global(var_name, state)
      user_satisfied %in% dict_key_to_translations("No") | is.na(note) },
      logic = list(
        # logic page 1, get new note
        midi_or_audio(page_type, prompt_text, var_name),
        # logic page 2, was everything ok with this note?
        psychTestR::reactive_page(function(answer, state, ...) {
          note <- answer[[1]]
          if(is.na(note)) {
            psychTestR::one_button_page(psychTestR::i18n("nothing_entered"))
          } else {
            psychTestR::set_global(var_name, note, state)
            present_stimuli(stimuli = note,
                            stimuli_type = "midi_notes",
                            display_modality = "both",
                            page_text = psychTestR::i18n("correct_note_message"),
                            page_type = "NAFC_page",
                            choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                            label = var_name,
                            play_button_text = psychTestR::i18n("Play")
            )
          }
        }),
        psychTestR::code_block(function(state, answer, ...) {
          psychTestR::set_global("user_satisfied", answer, state)
        })
      )
    )
  )
}


#' Get Instrument Range Pages
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
get_instrument_range_pages <- function(type) {
  # a short multi-page protocol to get the user's frequency range

  if (type == "record_audio_page") {
    c(
      get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), psychTestR::i18n("get_range_low_note")), var_name = "bottom_range", page_type = "record_audio_page"),
      get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), psychTestR::i18n("get_range_high_note")), var_name = "top_range", page_type = "record_audio_page"),
      psychTestR::reactive_page(function(state, ...) {
        lowest_user_note <- psychTestR::get_global("bottom_range", state)
        highest_user_note <- psychTestR::get_global("top_range", state)
        span <- highest_user_note - lowest_user_note
        psychTestR::set_global("span", span, state)
        range <- c(lowest_user_note, highest_user_note)
        present_stimuli(stimuli = range,
                        stimuli_type = "midi_notes",
                        display_modality = "both",
                        page_text = psychTestR::i18n("your_range_message"),
                        page_type = "one_button_page",
                        button_text = psychTestR::i18n("Next")
        )
      })
    )
  }
  else {
    c(
      get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page"),
      get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page"),
      psychTestR::reactive_page(function(state, ...) {
        lowest_user_note <- psychTestR::get_global("bottom_range", state)
        highest_user_note <- psychTestR::get_global("top_range", state)
        span <- highest_user_note - lowest_user_note
        psychTestR::set_global("span", span, state)
        range <- c(lowest_user_note, highest_user_note)
        present_stimuli(stimuli = range,
                        stimuli_type = "midi_notes",
                        page_type = "one_button_page",
                        display_modality = "both", page_text = psychTestR::i18n("your_range_message"), button_text = psychTestR::i18n("Next"))
      })
    )
  }

}


# internal funs

midi_or_audio <- function(type, prompt_text, var_name) {
  if (type == "record_audio_page") {

    musicassessr::record_audio_page(page_text = prompt_text,
                      label = var_name,
                      get_answer = get_answer_average_frequency_ff("round"),
                      show_record_button = TRUE,
                      show_aws_controls = FALSE,
                      method = "crepe",
                      button_text = psychTestR::i18n("Record"),
                      stop_button_text = psychTestR::i18n("Stop")
    )

  }
  else {
    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(page_text = prompt_text,
                       label = var_name,
                       get_answer = get_answer_midi_note_mode,
                       show_record_button = TRUE,
                       midi_device = midi_device,
                       button_text = psychTestR::i18n("Record"),
                       stop_button_text = psychTestR::i18n("Stop"))
    })
  }
}



melody_checks <- function(melody, state, stimuli_type = "midi_notes") {
  if(is.null(melody)) {
    melody <- psychTestR::get_global("melody", state)
  }
  if(length(melody) == 1 & is.character(melody) & stimuli_type != "midi_file") {
    melody <- itembankr::str_mel_to_vector(melody, ",")
  }
  melody
}


return_correct_attempts_left_page <- function(attempts_left, var_name, number_attempts, page_title = "Play The Melody") {

  if(attempts_left == 0) {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("attempts_remaining_0"))),
              choices = psychTestR::i18n("Continue"), save_answer = FALSE)
  }
  else if (attempts_left == 1) {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(psychTestR::i18n("attempts_remaining_1"))),
              choices = c(psychTestR::i18n("Continue"), psychTestR::i18n("Try_Again")), save_answer = FALSE)
  }
  else {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(paste0(psychTestR::i18n("attempts_remaining_several.1"), " ", attempts_left, " ", psychTestR::i18n("attempts_remaining_several.2")))),
              choices = c(psychTestR::i18n("Continue"), psychTestR::i18n("Try_Again")), save_answer = FALSE)
  }
}





