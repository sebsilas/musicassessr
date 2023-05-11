
#' Should the end of the test present a final page or not?
#'
#' @param final
#' @param task_name
#'
#' @return
#' @export
#'
#' @examples
final_page_or_continue_to_new_test <- function(final = TRUE, task_name) {
  if(final) {
    psychTestR::final_page(paste0(psychTestR::i18n("test_complete_1"), " ", task_name, psychTestR::i18n("test_complete_2")))
  } else {
    psychTestR::one_button_page(psychTestR::i18n("proceed_next_test"))
  }
}

present_record_button <- function(present = FALSE,
                                  type = "record_audio_page",
                                  midi_device = " ",
                                  interactive = FALSE,
                                  button_text = psychTestR::i18n("Record"),
                                  record_duration = NULL, show_stop_button_after_record = FALSE,
                                  stop_button_text = psychTestR::i18n("Stop")) {

  if (present & type == "record_audio_page" & is.null(record_duration)) {

    shiny::tags$div(id = "button_area",
                    shiny::tags$button(button_text, id = "recordButton", class="btn btn-default action-button"),
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style="visibility: hidden;">',stop_button_text, '</button>')),

                    if(show_stop_button_after_record) {
                      shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"',type,'\");
                           hideRecordButton();
                           showStopButton();
                            });'))
                    } else {
                      shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"',type,'\");
                           hideRecordButton();
                            });'))
                    }
    )
  } else if (present & type == "record_audio_page" & !is.null(record_duration)) {

    record_duration <- record_duration*1000
    shiny::tags$div(id = "button_area",
                    shiny::tags$button(button_text, id = "recordButton", class="btn btn-default action-button"),
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style="visibility: hidden;">',stop_button_text, '</button>')),
                    shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(', record_duration, ', false, false, this.id, \"',type,'\");
                            hideRecordButton();
                    });
    ')))
  } else if (present & type == "record_midi_page") {
    shiny::tags$div(id = "button_area",
                    shiny::tags$button(button_text, id = "recordButton", class="btn btn-default action-button"),
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style="visibility: hidden;">',stop_button_text, '</button>')),
                    shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"record_midi_page\");
                            hideRecordButton();
                           instantiateMIDI(\"',midi_device,'\", false);
                            })'))
    )
  } else {
    shiny::tags$div(id = "button_area",
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style = "visibility: hidden;">',stop_button_text, '</button>'))
    )
  }
}


validate_page_types <- function(page_type_string, args) {

  # check if certain page types have their required arguments
  # and give a descriptive error message back to user if they haven't specified something correctly

  if(page_type_string %in% c("NAFC_page", "dropdown_page")) {

    if(is.null(args$label) | is.null(args$choices)) {
      stop('You must specify a label and choices for NAFC_page or dropdown_page')
    }
  }

  if(page_type_string == "slider_page") {

    if(is.null(args$label) | is.null(args$min)
       | is.null(args$max) | is.null(args$value)
    ) {
      stop('You must specify a label, min, max and value arguments for slider pages')
    }
  }

  if(page_type_string == "text_input_page") {
    if(is.null(args$label)) {
      stop('You must specify a label for text_input_page')
    }
  }
}
