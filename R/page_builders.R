
#' A page builder for creating a specified number of play_melody_loops
#'
#' @param n_items
#' @param var_name
#' @param stimuli_type
#' @param page_type
#' @param max_goes
#' @param page_title
#' @param page_text
#' @param get_answer
#' @param rel_to_abs_mel_function
#' @param start_from_trial_no
#' @param clip_stimuli_length
#' @param arrhythmic
#' @param example
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
multi_page_play_melody_loop <- function(n_items, var_name = "melody", stimuli_type = "midi_notes",
                                                              page_type = "record_audio_page", max_goes = 3,
                                                              page_title = psychTestR::i18n("copy_melody_title"),
                                                              page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                                              get_answer = get_answer_save_aws_key, rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
                                                              start_from_trial_no = 1, clip_stimuli_length = FALSE,
                                                              arrhythmic = FALSE, example = FALSE, feedback = FALSE) {

  # items should be a dataframe
  # this will return a sequence of test items
  items <- lapply(start_from_trial_no:n_items, function(melody_no) {
    play_melody_loop2(melody_no = melody_no,
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

  items <- add_feedback(items, feedback, after = 2) # a play_melody_loop is 3 pages long
  items

}


#' Build multiple play_long_tone_record_audio_pages based on a user's range
#'
#' @param no_items
#' @param page_type
#' @param example
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
multi_play_long_tone_record_audio_pages <- function(no_items, page_type = "record_audio_page", example = FALSE, feedback = FALSE, get_answer = get_answer_pyin_long_note) {
  items <- unlist(lapply(1:no_items, function(x) play_long_tone_record_audio_page(long_note_no = x, page_type = page_type, example = example, get_answer = get_answer)))
  items <- add_feedback(items, feedback)
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
        check_note_ok(answer, state, var_name),
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
      present_range(state)
    )
  }
  else {
    c(
      get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page"),
      get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page"),
      present_range(state)
    )
  }

}


# internal funs

check_note_ok <- function(answer, state, var_name) {
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
  })
}

present_range <- function(state) {
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
}

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



