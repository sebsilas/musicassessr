#' Record MIDI page
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
#' @param answer_meta_data
#' @param autoInstantiate
#' @param midi_device
#' @param button_text
#' @param auto_next_page
#' @param page_text_first
#' @param happy_with_response
#' @param attempts_left
#' @param stop_button_text
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_midi_page <- function(body = " ", label = "record_midi_page", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ",
                             interactive = FALSE, note_no = "max", show_record_button = TRUE, get_answer = get_answer_midi,
                             answer_meta_data = 0, autoInstantiate = FALSE, midi_device, button_text = "Record",
                             auto_next_page = FALSE, page_text_first = TRUE,
                             happy_with_response =  FALSE, attempts_left = NULL, stop_button_text = "Stop", ...) {


  interactive <- ifelse(interactive, "true", "false")

  psychTestR::page(ui = shiny::tags$div(

    shiny::tags$head(
      auto_next_page(auto_next_page),
      shiny::tags$script('console.log(\"this is an midi page\");'),
      autoInstantiateMidi(instantiate = autoInstantiate, midi_device, interactive),
      shiny::tags$script(set_answer_meta_data(answer_meta_data))

    ),
    shiny::tags$body(
      shiny::tags$h2(page_title),
      shiny::tags$p(page_text),
      shiny::tags$div(body),
      reactive_stimuli(stimuli_function = stimuli_function,
                       stimuli_reactive = stimuli_reactive,
                       prepared_stimuli = abs_mel),

      present_record_button(show_record_button, type = "record_midi_page",
                            button_text = button_text, stop_button_text = stop_button_text),


      happy_with_response_message(happy_with_response, attempts_left)

    )
  ),
  label = label,
  get_answer = get_answer,
  save_answer = TRUE
  )
}



#' Create a page for selecting a MIDI device
#'
#' @param title
#' @param message
#' @param error_notification
#' @param button_text
#'
#' @return
#' @export
#'
#' @examples
select_midi_device_page <- function(title = "Select MIDI device",
                                    message = "Your device should have been plugged in before you reached this page. It may take a moment to appear.",
                                    error_notification = "No midi device found.",
                                    button_text = "Next"
                                    ) {
  # set the selected device
  psychTestR::page(
    label = "get_device",

    ui = shiny::tags$div(
      shiny::tags$h2(title),
      shiny::tags$p(message),
      shiny::tags$select(id = "midiDeviceSelector"),
      shiny::tags$br(),
      shiny::tags$br(),
      psychTestR::trigger_button("next", button_text),
      shiny::includeScript(path = "https://cdn.jsdelivr.net/npm/webmidi@2.5.1"),
      shiny::includeScript(path = system.file("www/js/getMIDIin.js", package = "musicassessr")),
      shiny::tags$script('generateDeviceDropdown();'),
    ),

    get_answer = function(input, state, ...) {

      if(is.null(input$midi_device)) { shiny::showNotification(error_notification) }

      psychTestR::set_global("midi_device", input$midi_device, state)

    },
    save_answer = TRUE
  )
}

autoInstantiateMidi <- function(instantiate = TRUE, midi_device, interactive) {
  if (instantiate == TRUE) {
    shiny::tags$script(paste0('instantiateMIDI(\"',midi_device,'\", ', interactive, ');'))
  }
  else {
    shiny::tags$script(paste0('const midi_device = \"', midi_device, '\";'))
  }
}




