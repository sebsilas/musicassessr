#' Record Audio Page
#'
#' @param body
#' @param label
#' @param stimuli
#' @param stimuli_reactive
#' @param page_text
#' @param page_title
#' @param interactive
#' @param note_no
#' @param show_record_button
#' @param get_answer
#' @param transpose
#' @param answer_meta_data
#' @param method
#' @param show_aws_controls
#' @param crepe_stats
#' @param button_text
#' @param stop_button_text
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_audio_page <- function(body = NULL, label = "record_audio_page", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ", interactive = FALSE,
                              note_no = "max", show_record_button = TRUE, get_answer = get_answer_store_async, transpose = 0, answer_meta_data = 0,
                              method = c("aws_pyin", "crepe"), show_aws_controls = FALSE, crepe_stats = FALSE,
                              button_text = "Record", stop_button_text = "Stop", record_duration = NULL, on_complete = NULL,
                              auto_next_page = FALSE, save_answer = TRUE, ...) {

  psychTestR::page(ui = shiny::tags$div(

    shiny::tags$head(

      auto_next_page(auto_next_page),

      shiny::tags$script(set_answer_meta_data(answer_meta_data))
        #htmltools::HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">')
        shiny::includeCSS(system.file("inst/static-website-s3/spinner/style.css", package = "musicassessr")

    ),
    shiny::tags$body(
      shiny::tags$script('var confidences = [];
                    var user_response_frequencies = [];
                    var timecodes = [];
                    var answer_meta_data;
                    '),
      shiny::tags$h2(page_title),
      shiny::tags$p(page_text),
      shiny::tags$div(body),
      reactive_stimuli(stimuli_function = stimuli_function,
                       stimuli_reactive = stimuli_reactive,
                       prepared_stimuli = abs_mel),

      present_record_button(present = show_record_button, type = method, button_text = button_text, record_duration = record_duration),

      shiny::tags$div(id ="container",
                      deploy_aws_pyin(method = method, show_aws_controls = show_aws_controls, stop_button_text),
                      deploy_crepe(method),
      )
    )
  ),
  label = label,
  get_answer = get_answer,
  save_answer = save_answer,
  on_complete = on_complete
  )
}

tidy_freqs <- function(freqs) {
  freqs.wo.null <- as.numeric(unlist(lapply(freqs, function(x) ifelse(is.null(x), 0, x) )))
  notes <- lapply(freqs.wo.null, function(x) ifelse(is.na(x) | x== 0, NA, round(hrep::freq_to_midi(as.numeric(x)))) )

  unlist(notes)
}

auto_next_page <- function(auto_next_page) {
  if(auto_next_page) {
    shiny::tags$script('var auto_next_page = true;')
  } else {
    shiny::tags$script('var auto_next_page = false;')
  }
}


deploy_crepe <- function(method, crepe_stats = FALSE) {

  if (crepe_stats == TRUE & method == "crepe") {

    shiny::tags$div(shiny::tags$canvas(id = "activation"),
        shiny::tags$div(id="output",
                 shiny::tags$br(),
                 shiny::tags$p('Status: ', shiny::tags$span(id="status")), shiny::tags$br(),
                 shiny::tags$p('Estimated Pitch: ', shiny::tags$span(id="estimated-pitch")),
                 shiny::tags$br(),
                 shiny::tags$p('Voicing Confidence: ', shiny::tags$span(id="voicing-confidence")),
                 shiny::tags$p('Your sample rate is', shiny::tags$span(id="srate"), ' Hz.')))
  }
  else {
    shiny::tags$div()
  }
}

deploy_aws_pyin <- function(method, crepe_stats = FALSE, show_aws_controls = TRUE, stop_button_text = "Stop") {

  if (method == "aws_pyin") {
    # NB: remove style attribute from pauseButton and/or recordingsList to show pause button or recordings respectively
    shiny::tags$div(htmltools::HTML(paste0('
    <div id="spinnerContainer" class="spinner"></div>

    <div id="controls">
    
  	 <button id="recordButton">Record</button>
  	 <button id="pauseButton" disabled style="display: none;">Pause</button>
  	 <button id="stopButton" disabled>',stop_button_text, '</button>
    </div>
    <div id="formats" style="display: none;">Format: start recording to see sample rate</div>
  	<p style="display: none;"><strong>Recordings:</strong></p>
  	<ol id="recordingsList" style="display: none;"></ol>
        <div id="loading" style="display: none;"></div>
        <div id="csv_file" style="display: none;"></div>')), show_aws_buttons(show_aws_controls))
  }
  else {
    shiny::tags$div()
  }
}




show_aws_buttons <- function(show_aws_controls) {
  if(!show_aws_controls) {
    aws_controls <- shiny::tags$script('var controls = document.getElementById("controls");
                                controls.style.visibility = \'hidden\'; // start hidden
                                console.log("hide controls");')
  } else {
    aws_controls <- shiny::tags$script('')
  }
  aws_controls
}
