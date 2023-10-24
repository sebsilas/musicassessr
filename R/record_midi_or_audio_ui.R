
# common ui template for midi and audio pages
record_midi_or_audio_ui <- function(body = "",
                                    label = "record_audio_page",
                                    stimuli = NULL,
                                    stimuli_reactive = FALSE,
                                    page_text = " ",
                                    page_title = " ",
                                    page_type = "record_audio_page",
                                    interactive = FALSE,
                                    get_answer,
                                    answer_meta_data = tibble::tibble(),
                                    record_button_text = psychTestR::i18n("Record"),
                                    stop_button_text = psychTestR::i18n("Stop"),
                                    record_duration = NULL,
                                    on_complete = NULL,
                                    save_answer = TRUE,
                                    page_text_first = TRUE,
                                    happy_with_response =  FALSE,
                                    attempts_left = 1L,
                                    max_goes_forced = FALSE,
                                    autoInstantiate = FALSE,
                                    midi_device = " ",
                                    max_goes = 1L,
                                    show_progress = FALSE,
                                    melody_no = 0,
                                    total_no_melodies = 0,
                                    volume_meter = FALSE,
                                    volume_meter_type = 'default',
                                    show_sheet_music_after_record = FALSE,
                                    show_record_button = TRUE,
                                    reactive_melody_no = FALSE, ...) {


  if(is.character(page_text)) {
    page_text <- shiny::tags$p(page_text)
  }

  section_progress <- if(reactive_melody_no) paste0(psychTestR::i18n("Section_Progress"), ': ', melody_no) else paste0(psychTestR::i18n("Section_Progress"), ': ', melody_no, "/", total_no_melodies)

  psychTestR::page(ui = shiny::tags$div(

    shiny::tags$head(

      shiny::tags$script( # set attempts
        shiny::HTML(paste0('Shiny.setInputValue("attempt", ', rjson::toJSON(max_goes - attempts_left), ');'))
      ),

      shiny::tags$script(paste0('console.log(\"this is a ', page_type, '\");')),

      if(page_type == "record_midi_page") autoInstantiateMidi(autoInstantiate, midi_device, interactive),

      if(page_type == "record_audio_page") send_page_label_to_js(label),

      shiny::tags$script(set_answer_meta_data(answer_meta_data))

    ),
    shiny::tags$body(

      if(show_progress) shiny::tags$h4(section_progress),

      shiny::tags$br(),

      shiny::tags$h2(page_title),

      if(page_text_first) page_text,

      if(volume_meter) shiny::tags$div(volume_meter(volume_meter_type, start_hidden = TRUE), shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr"))),

      shiny::tags$div(body),

      if(!is.null(stimuli)) shiny::tags$div(stimuli),

      present_record_button(show_record_button, page_type, record_button_text, stop_button_text, show_sheet_music_after_record),

      if(page_type == "record_audio_page") loading(),

      happy_with_response_message(happy_with_response, attempts_left, max_goes_forced, max_goes),

      if(!page_text_first) page_text
    )
  ),
  label = label,
  get_answer = get_answer,
  save_answer = save_answer,
  on_complete = on_complete
  )

}


send_page_label_to_js <- function(label) {
  shiny::tags$script(paste0('var page_label = \"', label, '\";'))
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

  if(max_goes_forced) {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1_max_goes_forced")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2_max_goes_forced")
  } else {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2")
  }

  if(attempts_left == 0L) {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("no_more_attempts_next")),
                    shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else if (attempts_left == 1L) {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p('If you were happy with your response, please click to \"Continue\", otherwise please click to \"Try Again\".'),
                    shiny::tags$p(attempts_remaining_1),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = "Try Again", label = "Try Again", onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("were_you_happy")),
                    if(!is.infinite(attempts_left)) shiny::tags$p(paste0(psychTestR::i18n("You_have"), ' ', " ", attempts_left, ' ', psychTestR::i18n("attempts_remaining_if_like"))),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = 'Try Again', label = 'Try Again', onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = 'Continue', label = 'Continue', onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  }
}


happy_with_response_message <- function(happy_with_response_message, attempts_left, max_goes_forced = FALSE, max_goes = 1) {

  if(max_goes == 1) {
    happy_with_response_message <- FALSE
  }

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



present_record_button <- function(show_record_button,
                                  page_type,
                                  record_button_text =  psychTestR::i18n("Record"),
                                  stop_button_text = psychTestR::i18n("Stop"),
                                  show_sheet_music_after_record = FALSE,
                                  sheet_music_id = "sheet_music") {


  shiny::tags$div(id = "button_area",
    shiny::tags$script(paste0("var stop_button_text = \"", stop_button_text, "\"")),
    shiny::tags$button(record_button_text, id = "recordButton", class = "btn btn-default action-button", style = if(show_record_button) "visibility: visible;" else "visibility: hidden"),
    shiny::tags$button(stop_button_text, id = "stopButton", class = "btn btn-default action-button", style = "visibility: hidden;"),
    shiny::tags$script(shiny::HTML(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                            startRecording(type = \"', page_type, '\", stop_button_text = \"', stop_button_text, '\");
                            recordUpdateUI(type = \"', page_type, '\");',
                          if(show_sheet_music_after_record) paste0("showSheetMusic('", sheet_music_id, "');") else "",
                            '});'))),
  )

}



