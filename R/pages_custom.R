
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



