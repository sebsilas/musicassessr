
#' Microphone Calibration Page
#'
#' @param admin_ui
#' @param on_complete
#' @param label
#' @param body
#' @param button_text
#' @param save_answer
#' @param deploy_crepe_stats
#'
#' @return
#' @export
#'
#' @examples
microphone_calibration_page <- function(button_text = "Start test") {

  ui <- shiny::tags$div(

    shiny::tags$h2(psychTestR::i18n("Microphone_Test")),
    shiny::tags$p(psychTestR::i18n("meter")),
    shiny::tags$meter(id = "volumeMeter", high = "0.25", max = "1", value = "0", style = "width: 400px; height: 50px; margin: 20px 0 20px 0;"),
    shiny::tags$div(shiny::tags$p(psychTestR::i18n("microphone_calibration_message_1")),
                   shiny::tags$p(psychTestR::i18n("microphone_calibration_message_2")),
                   shiny::tags$p(psychTestR::i18n("microphone_calibration_message_3"))
                   ), shiny::tags$br(),
    shiny::tags$button(button_text, id = "startButton", class="btn btn-default action-button"),
    shiny::tags$br(),
    psychTestR::trigger_button(label = psychTestR::i18n("microphone_calibration_button"), inputId = "nextButton", style = "visibility: hidden;"),
    shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr"))

  ) # end main div

  psychTestR::page(ui = ui, admin_ui = NULL, on_complete = NULL, label = NULL, save_answer = FALSE)
}

