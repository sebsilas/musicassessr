
#' Create a page which produces a given tone
#'
#' @param note
#' @param note_length
#' @param page_text
#' @param play_button_text
#'
#' @return
#' @export
#'
#' @examples
play_long_tone_record_audio_page <- function(note = NULL,
                                             long_note_no = "x",
                                             note_length = 5,
                                             page_title = psychTestR::i18n("long_tone_heading"),
                                             page_text = "Sing along with the tone for 5 seconds.",
                                             play_button_text = "Play",
                                             page_type = "record_audio_page",
                                             show_aws_controls = FALSE,
                                             show_record_button = FALSE,
                                             auto_next_page = TRUE,
                                             example = FALSE) {

  # a page type for playing a 5-second tone and recording a user singing with it

    if(example) {
      save_answer <- FALSE
    } else {
      save_answer <- TRUE
    }

    psychTestR::reactive_page(function(state, ...) {
      print('in reactive long tone page')
      if(is.null(note)) {
        user_range <- psychTestR::get_global("user_range_sample", state)
        note <- user_range[long_note_no]
      }

      present_stimuli(stimuli = note,
                      stimuli_type = "midi_notes",
                      display_modality = "auditory",
                      page_title = page_title,
                      page_text = page_text,
                      page_type = page_type,
                      page_label = "long_tone_",
                      play_button_text = play_button_text,
                      note_length = note_length,
                      sound = "tone",
                      show_aws_controls = show_aws_controls,
                      show_record_button = show_record_button,
                      auto_next_page = auto_next_page,
                      save_answer = save_answer,
                      get_answer = musicassessr::get_answer_save_aws_key)
    })

}


midi_vs_audio_select_page <- function(prompt = "How will you input into the test?") {
  dropdown_page(label = "select_input",
                next_button_text = i18n("Next"),
                prompt = prompt,
                choices = c(i18n("Microphone"), i18n("MIDI")),
                on_complete = function(answer, state, ...) {
                  set_global("response_type", answer, state)
                })
}


select_musical_instrument_page <- function() {

  insts_dict <- lapply(insts, psychTestR::i18n)

  dropdown_page(label = "select_musical_instrument",
                prompt = i18n("instrument_selection_message"),
                next_button_text = i18n("Next"),
                choices = as.vector(unlist(insts_dict)),
                alternative_choice = TRUE,
                alternative_text = i18n("other_please_state"),
                on_complete = function(state, answer, ...) {
                  language <- get_url_params(state)$language

                  if(language != "en") {
                    answer <- translate_from_dict(non_english_translation = answer, language = language)
                  }
                  set_global("inst", answer, state)
                })
}




play_interval_page <- function(interval = NULL,
                               page_title = "What is the interval?",
                               play_button_text = "Play",
                               example = FALSE,
                               label = "interval_",
                               save_answer = TRUE,
                               get_answer = interval_page_get_answer) {

  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }

  psychTestR::reactive_page(function(state, ...) {
    print('in reactive interval page')
    if(is.null(interval)) {
      start_note <- sample(48:72, 1)
      direction <- sample(c("ascending", "descending"), 1)
      interval <- sample(itembankr::intervals, 1)
      print('sampled interval')
      print(interval)
      print('semitones:')
      print(interval[[1]])
      abs_interval <- c(start_note, start_note+interval[[1]])
      #abs_interval <- ifelse(direction == "ascending", c(start_note, start_note+interval[[1]]), c(start_note, start_note-interval[[1]]))
      print('abs_interval')
      print(abs_interval)
      print('setting correct answer tiwht:')
      print(names(interval))
      answer_meta_data <- list(correct_answer = names(interval),
                              direction = direction,
                              abs_interval = abs_interval)

      print('answer_meta_data:')
      print(answer_meta_data)

      psychTestR::set_global("answer_meta_data", answer_meta_data, state)
    }

    psychTestR::page(ui = shiny::tags$div(
        shiny::tags$h2(page_title),
      present_stimuli(stimuli = abs_interval,
                      stimuli_type = "midi_notes",
                      display_modality = "auditory",
                      page_label = "interval_",
                      play_button_text = play_button_text,
                      sound = "piano"),
      shiny::selectInput(inputId = "dropdown",
                         label = NULL,
                         choices = names(itembankr::intervals)),
      psychTestR::trigger_button("next", "Next")),
    label = label, get_answer = get_answer, save_answer = save_answer)
  })

}

interval_page_get_answer <- function(input, state, ...) {
    print('interval_page_get_answer')
    answer_meta_data <- psychTestR::get_global("answer_meta_data", state)
    # print('dropdown said: ')
    # print(input$dropdown)
    # print('answerMeta correct answer is::')
    # print(answer_meta_data$correct_answer)
    msg <- ifelse(input$dropdown == answer_meta_data$correct_answer, "Correct Answer!", "Wrong Answer!")
    shiny::showNotification(msg)
    print('store the following list:')
    print(list(user_choice = input$dropdown,
         correct_answer = answer_meta_data$correct_answer,
         direction = answer_meta_data$direction,
         abs_interval = answer_meta_data$abs_interval))
    list(user_choice = input$dropdown,
         correct_answer = answer_meta_data$correct_answer,
         direction = answer_meta_data$direction,
         abs_interval = answer_meta_data$abs_interval)
}

