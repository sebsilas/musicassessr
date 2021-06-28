play_long_tone_record_audio_page <- function(note = 60,
                                             page_text = "Sing along with the tone for 5 seconds.",
                                             play_button_text = "Play") {

  # a page type for playing a 5-second tone and recording a user singing with it

  present_stimuli(stimuli = note,
                  stimuli_type = "midi_notes",
                  display_modality = "auditory",
                  page_text = page_text,
                  page_label = "long_tone_",
                  play_button_text = play_button_text,
                  note_length = 5,
                  sound = "tone"
  )
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
