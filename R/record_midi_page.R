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



record_midi_page <- function(body = NULL, label = "record_midi", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ", interactive = FALSE,
                              note_no = "max", show_record_button = TRUE, get_answer = get_answer_midi, transpose = 0,
                             answer_meta_data = 0, autoInstantiate = FALSE, midi_device, button_text = "Record",
                             auto_next_page = FALSE, user_rating = FALSE, page_text_first = TRUE,
                             happy_with_response =  FALSE, attempts_left = NULL, stop_button_text = "Stop", ...) {

  #note_no_js_script <- set.note.no(stimuli, note_no)
    if (interactive) { interactive <- "true" } else  { interactive <- "false" }

    psychTestR::page(ui = shiny::tags$div(

      shiny::tags$head(
        auto_next_page(auto_next_page),
        shiny::tags$script('console.log(\"this is an midi page\");'),
        autoInstantiateMidi(instantiate = autoInstantiate, midi_device, interactive),
        shiny::tags$script(set_answer_meta_data(answer_meta_data))

      ),
      shiny::tags$body(
        shiny::tags$script('// stuff to record
                    var user_response_midi_note_on = [];
                    var user_response_midi_note_off = [];
                    var onsets_noteon = [];
                    var onsets_noteoff = [];
                    Shiny.setInputValue("user_response_midi_note_on", JSON.stringify(user_response_midi_note_on));
                    Shiny.setInputValue("onsets_noteon", JSON.stringify(onsets_noteon));
                    Shiny.setInputValue("user_response_midi_note_off", JSON.stringify(user_response_midi_note_off));
                    Shiny.setInputValue("onsets_noteoff", JSON.stringify(onsets_noteoff));
                    '),
        shiny::tags$h2(page_title),
        shiny::tags$p(page_text),
        shiny::tags$div(body),
        reactive_stimuli(stimuli_function = stimuli_function,
                         stimuli_reactive = stimuli_reactive,
                         prepared_stimuli = abs_mel),

        present_record_button(show_record_button, type = "record_midi_page",
                              button_text = button_text, stop_button_text = stop_button_text),


        user_rating(user_rating),

        happy_with_response_message(happy_with_response, attempts_left)

      )
    ),
    label = label,
    get_answer = get_answer,
    save_answer = TRUE
    )
}

