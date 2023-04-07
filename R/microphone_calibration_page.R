

#' Microphone Calibration Page
#'
#' @param button_text
#' @param concise_wording
#' @param musical_instrument
#'
#' @return
#' @export
#'
#' @examples
microphone_calibration_page <- function(button_text = psychTestR::i18n("Start_test"), concise_wording = FALSE, musical_instrument = FALSE) {

  if(concise_wording) {
    wording <- shiny::tags$div(shiny::tags$p(psychTestR::i18n("microphone_calibration_concise_message_1")),
                               shiny::tags$p(psychTestR::i18n("microphone_calibration_concise_message_2")),
                               if(musical_instrument) shiny::tags$p(shiny::tags$strong()))
  } else {
    wording <- shiny::tags$div(shiny::tags$p(psychTestR::i18n("microphone_calibration_message_1")),
                    shiny::tags$p(psychTestR::i18n("microphone_calibration_message_2")),
                    shiny::tags$p(psychTestR::i18n("microphone_calibration_message_3")))
  }

  ui <- shiny::tags$div(

    shiny::tags$h2(psychTestR::i18n("Microphone_Test")),
    shiny::tags$p(psychTestR::i18n("meter")),
    shiny::tags$meter(id = "volumeMeter", high = "0.25", max = "1", value = "0", style = "width: 400px; height: 50px; margin: 20px 0 20px 0;"),
      wording, shiny::tags$br(),
    shiny::tags$button(button_text, id = "startButton", class="btn btn-default action-button"),
    shiny::tags$br(),
    psychTestR::trigger_button(label = psychTestR::i18n("microphone_calibration_button"), inputId = "nextButton", style = "visibility: hidden;"),
    shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr"))

  ) # end main div

  psychTestR::page(ui = ui, admin_ui = NULL, on_complete = NULL, label = NULL, save_answer = FALSE)
}

