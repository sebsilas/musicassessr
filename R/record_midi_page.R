



#' Page to record MIDI in psychTestR
#'
#' @param body
#' @param label
#' @param stimuli
#' @param stimuli_reactive
#' @param page_text
#' @param page_title
#' @param interactive
#' @param get_answer
#' @param answer_meta_data
#' @param record_button_text
#' @param stop_button_text
#' @param record_duration
#' @param on_complete
#' @param save_answer
#' @param page_text_first
#' @param happy_with_response
#' @param attempts_left
#' @param max_goes_forced
#' @param autoInstantiate
#' @param midi_device
#' @param max_goes
#' @param show_progress
#' @param melody_no
#' @param total_no_melodies
#' @param volume_meter
#' @param volume_meter
#' @param volume_meter_type
#' @param show_sheet_music_after_record
#' @param show_record_button
#' @param reactive_melody_no
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_midi_page <- function(body = "",
                             label = "record_midi_page",
                             stimuli = NULL,
                             stimuli_reactive = FALSE,
                             page_text = " ",
                             page_title = " ",
                             interactive = FALSE,
                             get_answer = get_answer_midi,
                             answer_meta_data = tibble::tibble(),
                             record_button_text = psychTestR::i18n("Record"),
                             stop_button_text = psychTestR::i18n("Stop"),
                             record_duration = NULL,
                             on_complete = NULL,
                             save_answer = TRUE,
                             page_text_first = TRUE,
                             happy_with_response =  FALSE,
                             attempts_left = NULL,
                             max_goes_forced = FALSE,
                             autoInstantiate = FALSE,
                             midi_device = "",
                             max_goes = 1,
                             show_progress = FALSE,
                             melody_no = 0,
                             total_no_melodies = 0,
                             volume_meter = FALSE,
                             volume_meter_type = 'default',
                             show_sheet_music_after_record = FALSE,
                             show_record_button = TRUE,
                             reactive_melody_no = FALSE, ...) {

  record_midi_or_audio_ui(body,
                          label,
                          stimuli,
                          stimuli_reactive,
                          page_text,
                          page_title,
                          page_type = "record_midi_page",
                          interactive,
                          get_answer,
                          answer_meta_data,
                          record_button_text,
                          stop_button_text,
                          record_duration,
                          on_complete,
                          save_answer,
                          page_text_first,
                          happy_with_response,
                          attempts_left,
                          max_goes_forced,
                          autoInstantiate,
                          midi_device,
                          max_goes,
                          show_progress,
                          melody_no,
                          total_no_melodies,
                          volume_meter,
                          volume_meter_type,
                          show_sheet_music_after_record,
                          show_record_button,
                          reactive_melody_no)

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
                                    button_text = psychTestR::i18n("Next")
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
  if (instantiate) {
    shiny::tags$script(paste0('instantiateMIDI(\"',midi_device,'\", ', TRUE_to_js_true(interactive), ');'))
  } else {
    shiny::tags$script(paste0('var midi_device = \"', midi_device, '\";'))
  }
}


test_midi_page <- function() {
  psychTestR::reactive_page(function(state, ...) {

    present_stimuli(stimuli = "interactive",
                    stimuli_type = "midi_notes",
                    page_type = "record_midi_page",
                    display_modality = "visual",
                    page_title = "MIDI test",
                    page_text = shiny::tags$div(volume_meter(high = "30", max = "127"),
                                                tags$p("See if you get feedback when you use your MIDI device."),
                                                psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
                    autoInstantiate = TRUE,
                    interactive = TRUE,
                    get_answer = get_answer_fake,
                    midi_device = psychTestR::get_global("midi_device", state))

  })
}



