
requirements_page <- function(headphones = TRUE, input = c("microphone",
                                                           "midi_keyboard",
                                                           "midi_keyboard_and_microphone",
                                                           "midi_keyboard_or_microphone")) {

  psychTestR::NAFC_page(label = "headphones_and_microphone_check",
                        choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                        prompt = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("mar_requirements")),
                                                 mar_requirements_list(headphones, input),
                                                 shiny::tags$p(psychTestR::i18n("have_following"))
                                                 ),
                        on_complete = musicassessr::have_requirements)
}


mar_requirements_list <- function(headphones, input) {

  shiny::tags$div(
    shiny::tags$ul(class = "roman",
                   shiny::tags$li(psychTestR::i18n("mar_requirements_quiet")),
                   if(headphones) shiny::tags$li(psychTestR::i18n("mar_requirements_headphones")),
                   if(is_microphone_only_test(input)) shiny::tags$li(psychTestR::i18n("mar_requirements_microphone")),
                   if(input == "midi_keyboard_or_microphone") {
                                  shiny::tags$ul(class = "square",
                                  shiny::tags$li(psychTestR::i18n("mar_requirements_microphone"), shiny::tags$em(psychTestR::i18n("or"))),
                                  shiny::tags$li(psychTestR::i18n("MIDI_Keyboard")))
                    }
                   ),
    if(sjmisc::str_contains(input, "microphone"))  shiny::helpText(psychTestR::i18n("mar_requirements_microphone2"))
  )
}
