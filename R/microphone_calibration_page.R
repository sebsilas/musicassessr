
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

    shiny::tags$h2("Microphone Test"),
    shiny::tags$meter(id = "volumeMeter", high = "0.25", max = "1", value = "0", style = "width: 400px; height: 50px; margin: 20px 0 20px 0;"),
    shiny::tags$div(shiny::tags$p("Please make sure your microphone is plugged in (if using an external microphone)."),
                   shiny::tags$p('Click on the Start test button. If you see a popup message that says "Allow", click "Allow"'),
                   shiny::tags$p('Make a sound (e.g., hum, clap, sing).  If you see activity  (moving coloured lines) inside the meter when you make a sound, then the microphone is working. Otherwise, it is not.'),
                   shiny::tags$p('If the microphone is not working, see if you can adjust your microphone set up in order to get a response on the screen. If you cannot get the mic to work, please end the program and send a note to ', shiny::tags$strong('silass@stud.hmtm-hannover.de'), 'with the subject header “Mic not working”.  You will be contacted within the next 24 hours to troubleshoot the problem.
                                 If you wish to be contacted at a different email address, leave that information in the body of the email.  You may also try another computer that is available to you and try again.')),    shiny::tags$br(),
    shiny::tags$button(button_text, id = "startButton", class="btn btn-default action-button"),
    shiny::tags$br(),
    psychTestR::trigger_button(label = "I can see activity on the meter.", inputId = "nextButton", style = "visibility: hidden;"),
    shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr"))

  ) # end main div

  psychTestR::page(ui = ui, admin_ui = NULL, on_complete = NULL, label = NULL, save_answer = FALSE)
}

