

#' Page to record audio in psychTestR
#'
#' @param body
#' @param label
#' @param stimuli
#' @param stimuli_reactive
#' @param page_text
#' @param page_title
#' @param interactive
#' @param show_record_button
#' @param get_answer
#' @param answer_meta_data
#' @param show_aws_controls
#' @param button_text
#' @param stop_button_text
#' @param record_duration
#' @param on_complete
#' @param auto_next_page
#' @param save_answer
#' @param page_text_first
#' @param happy_with_response
#' @param attempts_left
#' @param max_goes_forced
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_audio_page <- function(body = " ",
                             label = "record_midi_page",
                             stimuli = " ",
                             stimuli_reactive = FALSE,
                             page_text = " ",
                             page_title = " ",
                             interactive = FALSE,
                             show_record_button = TRUE,
                             get_answer = get_answer_save_aws_key,
                             answer_meta_data = tibble::tibble(),
                             show_aws_controls = FALSE,
                             button_text = "Record",
                             stop_button_text = "Stop",
                             record_duration = NULL,
                             on_complete = NULL,
                             auto_next_page = FALSE,
                             save_answer = TRUE,
                             page_text_first = TRUE,
                             happy_with_response =  FALSE,
                             attempts_left = NULL,
                             max_goes_forced = FALSE, ...) {

  record_midi_or_audio_ui(body,
                          label,
                          stimuli,
                          stimuli_reactive,
                          page_text,
                          page_title,
                          page_type = "record_audio_page",
                          interactive,
                          show_record_button,
                          get_answer,
                          answer_meta_data,
                          show_aws_controls,
                          button_text,
                          stop_button_text,
                          record_duration,
                          on_complete,
                          auto_next_page,
                          save_answer,
                          page_text_first,
                          happy_with_response,
                          attempts_left,
                          max_goes_forced,
                          autoInstantiate =  FALSE,
                          midi_device = " ")

}


deploy_aws_pyin <- function(show_aws_controls = TRUE, stop_button_text = "Stop") {

  # NB: remove style attribute from pauseButton and/or recordingsList to show pause button or recordings respectively
  shiny::tags$div(htmltools::HTML(
  '<div id="spinnerContainer" class="spinner"></div>

  <div id="controls">

   <button id="recordButton" class="btn btn-default action-button">Record</button>
   <button id="pauseButton" class="btn btn-default action-button" disabled style="display: none;">Pause</button>
  </div>
  <div id="formats" style="display: none;">Format: start recording to see sample rate</div>
  <p style="display: none;"><strong>Recordings:</strong></p>
  <ol id="recordingsList" style="display: none;"></ol>
      <div id="csv_file" style="display: none;"></div>'), show_aws_buttons(show_aws_controls))
}


show_aws_buttons <- function(show_aws_controls) {
  if(show_aws_controls) {
    aws_controls <- shiny::tags$script('')
  } else {
    aws_controls <- shiny::tags$script('var controls = document.getElementById("controls");
                                controls.style.visibility = \'hidden\'; // start hidden
                                console.log("hide controls");')
  }
  aws_controls
}



