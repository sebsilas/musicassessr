
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
microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL,
                                        body = shiny::tags$div(shiny::tags$p("Please make sure your microphone is plugged in (if using an external microphone)."),
                                                               shiny::tags$p("Click on the microphone image so it turns red. If you see a popup message that says 'Allow',  click 'Allow'"),
                                                               shiny::tags$p("Wait a few seconds. You should see a black box appear. Make a sound (e.g., hum, clap, sing).  If you see activity  (moving coloured lines) inside the box when you make a sound, then the microphone is working. Otherwise, it is not."),
                                                               shiny::tags$p("If the mic is not working, see if you can adjust your microphone set up in order to get a response on the screen. If you cannot get the mic to work, please end the program and send a note to ", shiny::tags$strong("silass@stud.hmtm-hannover.de"), "with the subject header “Mic not working”.  You will be contacted within the next 24 hours to troubleshoot the problem.
                                                                             If you wish to be contacted at a different email address,  leave that information in the body of the email.  You may also try another computer that is available to you and try again.")

                                        ),
                                        button_text = "I can see activity in the box.", save_answer = FALSE, deploy_crepe_stats = FALSE) {

  ui <- shiny::tags$div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    # start body
    audio_parameters_js_script,
    shiny::tags$script(src="https://cdn.jsdelivr.net/npm/@tensorflow/tfjs/dist/tf.min.js"),
    shiny::includeScript(path=system.file("www/js/crepe.js", package = "musicassessr")),
    shiny::includeCSS(path=system.file('www/css/crepe.css', package = "musicassessr")),

    shiny::tags$script("var trigButton = document.getElementById('next');
              trigButton.onclick = crepeStop;"),

    shiny::tags$h2("Microphone Test"),

    shiny::tags$img(id = "record",
        src = "musicassessr-assets/img/mic128.png",
        onclick = "toggleRecording(this);crepe();initTF();crepeResume();",
        style = "display:block; margin:1px auto;", width = "100px", height = "100px"),

    body,

    shiny::tags$div(id ="container",
             shiny::tags$canvas(id="activation", width="960", height="360"),
             shiny::tags$br(),
             deploy_crepe_stats(deploy_crepe_stats)
    ),

    shiny::tags$button(button_text, id = "next", onclick = "crepeStop(true);", class="btn btn-default action-button"),
    shiny::tags$hr()
  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}

deploy_crepe_stats <- function(deploy) {
  if (deploy) {
    res <- shiny::tags$div(
      shiny::tags$div(id = "audio-value"),
      shiny::tags$div(id="output",
                      shiny::tags$br(),
                      shiny::tags$p('Status: ', shiny::tags$span(id="status")), shiny::tags$br(),
                      shiny::tags$p('Estimated Pitch: ', shiny::tags$span(id="estimated-pitch")),
                      shiny::tags$br(),
                      shiny::tags$p('Voicing Confidence: ', shiny::tags$span(id="voicing-confidence")),
                      shiny::tags$p('Your sample rate is', shiny::tags$span(id="srate"), ' Hz.')
      ))
  } else {
    res <- shiny::tags$div()
  }
  res
}
