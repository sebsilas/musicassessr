
#' Record Audio Page
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_audio_page <- function(body = " ", label = "record_audio_page", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ",
                              interactive = FALSE, show_record_button = TRUE, get_answer = get_answer_store_async, answer_meta_data = data.frame(),
                              show_aws_controls = FALSE, button_text = "Record", stop_button_text = "Stop", record_duration = NULL, on_complete = NULL,
                              auto_next_page = FALSE, save_answer = TRUE, page_text_first = TRUE,
                              happy_with_response =  FALSE, attempts_left = integer(), max_goes_forced = FALSE, ...) {


  psychTestR::page(ui = shiny::tags$div(

    shiny::tags$head(

      auto_next_page(auto_next_page),

      shiny::tags$script(set_answer_meta_data(answer_meta_data))

    ),
    shiny::tags$body(

      shiny::tags$h2(page_title),
      if(page_text_first) shiny::tags$p(page_text),

      shiny::tags$div(body),
      reactive_stimuli(stimuli_function = stimuli_function,
                       stimuli_reactive = stimuli_reactive,
                       prepared_stimuli = abs_mel),

      present_record_button(present = show_record_button, type = "record_audio_page",
                            button_text = button_text, record_duration = record_duration,
                            stop_button_text = stop_button_text),

      loading(),

      happy_with_response_message(happy_with_response, attempts_left, max_goes_forced),
      deploy_aws_pyin(show_aws_controls = show_aws_controls, stop_button_text),

      if(!page_text_first) shiny::tags$p(page_text)
    )
  ),
  label = label,
  get_answer = get_answer,
  save_answer = save_answer,
  on_complete = on_complete
  )

}


auto_next_page <- function(auto_next_page) {
  if(auto_next_page) {
    shiny::tags$script('var auto_next_page = true;')
  } else {
    shiny::tags$script('var auto_next_page = false;')
  }
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


loading <- function() {
  htmltools::HTML('
  <div class="hollow-dots-spinner" :style="spinnerStyle;display:none;">
    <div class="dot"></div>
      <div class="dot"></div>
        <div class="dot"></div>
          </div>
    <div id="loading" style="display: none;"></div>
    ')
}


return_correct_attempts_left <- function(attempts_left, max_goes_forced = FALSE) {
  if(attempts_left == 0L) {
    #label = paste0(var_name,"_attempt_", number_attempts, "_choice"),
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("attempts_remaining_0")),
                    shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
                    )
  } else if (attempts_left == 1L) {
    # label = paste0(var_name,"_attempt_", number_attempts, "_choice")
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(psychTestR::i18n("attempts_remaining_1")),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = psychTestR::i18n("Try_Again"), label = psychTestR::i18n("Try_Again"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else {
    # label = paste0(var_name,"_attempt_", number_attempts, "_choice")
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("happy_with_response_message")),
                    shiny::tags$p(paste0(psychTestR::i18n("attempts_remaining_several.1"), " ", attempts_left, " ", psychTestR::i18n("attempts_remaining_several.2"))),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = psychTestR::i18n("Try_Again"), label = psychTestR::i18n("Try_Again"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  }
}



happy_with_response_message <- function(happy_with_response_message, attempts_left, max_goes_forced = FALSE) {
  if(happy_with_response_message) {
    shiny::tags$div(
      return_correct_attempts_left(attempts_left, max_goes_forced),
      shiny::tags$script('var show_happy_with_response = true;')
    )
  } else {
    shiny::tags$div(
      shiny::tags$script('var show_happy_with_response = false;')
    )
  }
}

