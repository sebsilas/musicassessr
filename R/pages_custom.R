

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
