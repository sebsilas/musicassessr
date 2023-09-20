
requirements_page <- function(headphones = TRUE, input_type = c("microphone",
                                                               "midi_keyboard",
                                                               "midi_keyboard_and_microphone",
                                                               "midi_keyboard_or_microphone"),
                              musical_instrument = FALSE) {

  psychTestR::NAFC_page(label = "headphones_and_microphone_check",
                        choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                        prompt = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("mar_requirements")),
                                                 mar_requirements_list(headphones, input_type, musical_instrument),
                                                 shiny::tags$p(psychTestR::i18n("have_following"))
                                                 ),
                        on_complete = musicassessr::have_requirements)
}


mar_requirements_list <- function(headphones, input_type, musical_instrument) {

  shiny::tags$div(
    shiny::tags$ul(class = "roman",
                   shiny::tags$li(psychTestR::i18n("mar_requirements_quiet")),
                   if(headphones) shiny::tags$li(psychTestR::i18n("mar_requirements_headphones")),
                   if(is_microphone_only_test(input_type)) shiny::tags$li(psychTestR::i18n("mar_requirements_microphone")),
                   if(input_type == "midi_keyboard_or_microphone") {
                                  shiny::tags$ul(class = "square",
                                  shiny::tags$li(paste0(psychTestR::i18n("mar_requirements_microphone"),
                                                 if(musical_instrument) " and an acoustic musical instrument (e.g., saxophone)"),
                                                 shiny::tags$em(psychTestR::i18n("or"))),
                                  shiny::tags$li(psychTestR::i18n("MIDI_Keyboard")))
                   },
                   if(input_type == "midi_keyboard") {
                     shiny::tags$li(psychTestR::i18n("MIDI_Keyboard"))
                   }
                   ),
    if(sjmisc::str_contains(input_type, "microphone"))  shiny::helpText(psychTestR::i18n("mar_requirements_microphone2"))
  )
}
