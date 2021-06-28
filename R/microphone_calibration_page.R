deploy_crepe_stats <- function(deploy) {
  if (deploy == TRUE) {
    res <- div(
      shiny::tags$div(id = "audio-value"),
      tags$div(id="output",
               br(),
               tags$p('Status: ', tags$span(id="status")), tags$br(),
               tags$p('Estimated Pitch: ', tags$span(id="estimated-pitch")),
               tags$br(),
               tags$p('Voicing Confidence: ', tags$span(id="voicing-confidence")),
               tags$p('Your sample rate is', tags$span(id="srate"), ' Hz.')
      ))
  } else {
    res <- div()
  }
  res
}

microphone_calibration_page <- function(admin_ui = NULL, on_complete = NULL, label= NULL,
                                        body = div(tags$p("Click Allow on the pop-up. Then click on the microphone image and wait a few seconds.
           You should see a black box start scrolling across the page."),
                                        tags$p("If you see activity inside the box when you make noise, then the microphone is working.")),
                                        button_text = "I see activity in the box.", save_answer = FALSE, deploy_crepe_stats = FALSE) {

  ui <- div(
    shiny::tags$style('._hidden { display: none;}'), # to hide textInputs
    # start body
    audio_parameters_js_script,
    includeScript(path="crepe_html/tfjs-0.8.0.min.js"),
    includeScript(path="crepe_html/crepe.js"),
    includeCSS(path = 'crepe_html/crepe.css'),

    tags$script("var trigButton = document.getElementById('next');
              trigButton.onclick = crepeStop;"),


    img(id = "record",
        src = "img/mic128.png",
        onclick = "toggleRecording(this);crepe();initTF();crepeResume();",
        style = "display:block; margin:1px auto;", width = "100px", height = "100px"),

    body,

    tags$div(id ="container",
             shiny::tags$canvas(id="activation", width="960", height="360"),
             br(),
             deploy_crepe_stats(deploy_crepe_stats)
    ),

    tags$button(button_text, id = "next", onclick = "crepeStop(true);"),
    hr()
  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = FALSE)
}
