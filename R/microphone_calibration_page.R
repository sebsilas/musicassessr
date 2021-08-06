
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
                                        body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("microphone_calibration_message_1")),
                                                               shiny::tags$p(psychTestR::i18n("microphone_calibration_message_2")),
                                                               shiny::tags$p(psychTestR::i18n("microphone_calibration_message_3"))
                                                               ),
                                        button_text = psychTestR::i18n("microphone_calibration_button"), save_answer = FALSE, deploy_crepe_stats = FALSE) {

  ui <- shiny::tags$div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    # start body
    audio_parameters_js_script,
    shiny::tags$script(src="https://cdn.jsdelivr.net/npm/@tensorflow/tfjs/dist/tf.min.js"),
    shiny::includeScript(path=system.file("crepe_html/crepe.js", package = "musicassessr")),
    shiny::includeCSS(path=system.file('crepe_html/crepe.css', package = "musicassessr")),

    shiny::tags$script("var trigButton = document.getElementById('next');
              trigButton.onclick = crepeStop;"),

    shiny::tags$h2(psychTestR::i18n("Microphone_Test")),

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

    shiny::tags$button(button_text, id = "next", onclick = "crepeStop(true);"),
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
