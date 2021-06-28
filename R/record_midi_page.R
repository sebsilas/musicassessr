select_midi_device_page <- function(title = "Select MIDI device",
                                    message = "Your device should have been plugged in before you reached this page. It may take a moment to appear.",
                                    error_notification = "No midi device found.",
                                    button_text = "Next"
                                    ) {
  # set the selected device
  page(
    label = "get_device",

    ui = tags$div(
      tags$h2(title),
      tags$p(message),
      tags$select(id = "midiDeviceSelector"),
      tags$br(),
      tags$br(),
      trigger_button("next", button_text),
      includeScript(path = "https://cdn.jsdelivr.net/npm/webmidi@2.5.1"),
      includeScript(path = "www/js/getMIDIin.js"),
      tags$script('generateDeviceDropdown();'),
    ),

    get_answer = function(input, state, ...) {

      if(is.null(input$midi_device)) { shiny::showNotification(error_notification) }

      set_global("midi_device", input$midi_device, state)

    },
    save_answer = TRUE
  )
}

autoInstantiateMidi <- function(instantiate = TRUE, midi_device, interactive) {
  if (instantiate == TRUE) {
    tags$script(paste0('instantiateMIDI(\"',midi_device,'\", ', interactive, ');'))
  }
  else {
    tags$script(paste0('const midi_device = \"', midi_device, '\";'))
  }
}



record_midi_page <- function(body = NULL, label = "record_midi", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ", interactive = FALSE,
                              note_no = "max", show_record_button = FALSE, get_answer = get_answer_midi, transpose = 0, answer_meta_data = 0,
                             autoInstantiate = FALSE, midi_device, button_text = "Record", ...) {

  #note_no_js_script <- set.note.no(stimuli, note_no)
    if (interactive) { interactive <- "true" } else  { interactive <- "false" }
    print('in midi page')
    print(midi_device)
    print(interactive)
    print('answer data?')
    print(answer_meta_data)
    print(set_answer_meta_data(answer_meta_data))
    psychTestR::page(ui = tags$div(

      tags$head(

        tags$script('console.log(\"this is an midi page\");'),
        autoInstantiateMidi(instantiate = autoInstantiate, midi_device, interactive),
        tags$script(set_answer_meta_data(answer_meta_data)),

      ),
      tags$body(
        tags$script('// stuff to record
                    var user_response_midi_note_on = [];
                    var user_response_midi_note_off = [];
                    var onsets_noteon = [];
                    var onsets_noteoff = [];
                    Shiny.setInputValue("user_response_midi_note_on", JSON.stringify(user_response_midi_note_on));
                    Shiny.setInputValue("onsets_noteon", JSON.stringify(onsets_noteon));
                    Shiny.setInputValue("user_response_midi_note_off", JSON.stringify(user_response_midi_note_off));
                    Shiny.setInputValue("onsets_noteoff", JSON.stringify(onsets_noteoff));
                    Shiny.setInputValue("answer_meta_data", JSON.stringify(answer_meta_data));
                    '),
        tags$h2(page_title),
        tags$p(page_text),
        tags$div(body),
        reactive_stimuli(stimuli_function = stimuli_function,
                         stimuli_reactive = stimuli_reactive,
                         prepared_stimuli = abs_mel),

        present_record_button(show_record_button, type = "record_midi_page", button_text = button_text),

      )
    ),
    label = label,
    get_answer = get_answer,
    save_answer = TRUE
    )
}

