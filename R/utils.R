
# low level funs

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


insert.every.other.pos.in.list <- function(l, item_to_add, n = 2) {
  for (i in seq_along(l)) {
    l <- append(l, item_to_add, after = (i*n)-1)
  }
  l
}

#g <- as.list(1:10)
# g2 <- insert.every.other.pos.in.list(g, "a")
# g3 <- insert.every.other.pos.in.list(g, "a", n = 3)
# g4 <- insert.every.other.pos.in.list(g, "a", n = 4)



set_answer_meta_data <- function(meta_data) {
  paste0('Shiny.setInputValue(\"answer_meta_data\", ', meta_data, ');')
}



set.note.no <- function(stimuli, note_no) {
  # depending on whether a note_no argument was specified, return the correct JS script

  if (note_no == "max") {
    note_no <- '\"max\"'
  }

  if (is.null(note_no)) {
    js_script <- paste0('var stimuli = ', rjson::toJSON(stimuli),';')
  }

  else {
    js_script <- paste0('var stimuli = ', rjson::toJSON(stimuli),'; Shiny.setInputValue("note_no", ', note_no, ');')
  }
  js_script
}

set.audio.parameters.js.script <- function(highest_allowed_freq, lowest_allowed_freq, min_confidence) {

  shiny::tags$script(paste0('var highestAllowedFreq = ', highest_allowed_freq, '; ',
                            'var lowestAllowedFreq = ', lowest_allowed_freq, '; ',
                            'var minConfidence = ', min_confidence, '; '))
}


audio_parameters_js_script <- set.audio.parameters.js.script(highest_allowed_freq = highest.allowed.freq,
                                                             lowest_allowed_freq = lowest.allowed.freq,
                                                             min_confidence = min.confidence)

present_record_button <- function(present = FALSE, type = "record_audio_page", midi_device = " ",
                                  interactive = FALSE, button_text = "Record",
                                  record_duration = NULL, show_stop_button_after_record = FALSE,
                                  stop_button_text = "Stop") {

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
  }

  else if (present & type == "record_audio_page" & !is.null(record_duration)) {

    record_duration <- record_duration*1000
    shiny::tags$div(id = "button_area",
                    shiny::tags$button(button_text, id = "recordButton", class="btn btn-default action-button"),
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style="visibility: hidden;">',stop_button_text, '</button>')),
                    shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(', record_duration, ', false, false, this.id, \"',type,'\");
                            hideRecordButton();
                    });
    ')))
  }

  else if (present & type == "record_midi_page") {
    shiny::tags$div(id = "button_area",
        shiny::tags$button(button_text, id = "recordButton", class="btn btn-default action-button"),
        htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style="visibility: hidden;">',stop_button_text, '</button>')),
        shiny::tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"record_midi_page\");
                            hideRecordButton();
                           instantiateMIDI(\"',midi_device,'\", false); })'))
    )
  }

  else {
    shiny::tags$div(id = "button_area",
                    htmltools::HTML(paste0('<button id="stopButton" class="btn btn-default action-button" style = "visibility: hidden;">',stop_button_text, '</button>'))
                    )
  }
}




validate.page.types <- function(page_type_string, args) {

  # check if certain page types have their required arguments
  # and give a descriptive error message back to user if they haven't specified something correctly

  if(page_type_string == "NAFC_page" |
     page_type_string == "dropdown_page") {

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

check.correct.argument.for.body <- function(page_type_string, args, stimuli_wrapped) {
  # feed the body to the page, but using the correct argument
  # i.e some pages accept "body" whilst others accept "prompt"
  if (page_type_string == "one_button_page" |
      page_type_string == "record_audio_page" |
      page_type_string == "record_key_presses_page") {
    args[["body"]] <- stimuli_wrapped
  }

  else if (page_type_string == "NAFC_page" |
           page_type_string == "dropdown_page" |
           page_type_string == "slider_page" |
           page_type_string == "text_input_page"
  ) {
    args[["prompt"]] <- stimuli_wrapped
  }
  else {
    ## leaving here for now in case there's another use I haven't thought of yet
  }
  args
}

item_bank_type_to_stimuli_type <- function(string_of_item_bank_type) {
  if(str_detect(string_of_item_bank_type, "RDS_file")) {
    item_bank_type <- str_remove(string_of_item_bank_type, "RDS_file_")
  }
  else {
    item_bank_type <- string_of_item_bank_type
  }
  item_bank_type
}


# response check functions


check.response.type.audio <- function(state, ...) {
  user_response_selection <- get_global("response_type", state)
  ifelse(user_response_selection %in% dict_key_to_translations("Microphone"), TRUE, FALSE)
}

check.response.type.midi<- function(state, ...) {
  user_response_selection <- get_global("response_type", state)
  ifelse(user_response_selection == "MIDI", TRUE, FALSE)
}


#' Check if user has requirements for musicassessr test
#'
#' @param answer
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
have_requirements <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res %in% dict_key_to_translations("Yes")) TRUE
  else psychTestR::display_error("Sorry, but you do not have the correct requirements to run this test")
}



is_sci_notation <- function(x) {
  last_char <- get.last.char.of.string(x)
  if (is.na(as.numeric(last_char))) {
    stop('Last character is not a number, so entry is not in sci_notation format')
  }
}

is_microphone_only_test <- function(input) {
  sjmisc::str_contains(input, "microphone") & input != "midi_keyboard_or_microphone"
}


urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}



#' Should the end of the test present a final page or not?
#'
#' @param final
#' @param task_name
#'
#' @return
#' @export
#'
#' @examples
final_page_or_continue_to_new_text <- function(final = TRUE, task_name) {
  if(final) {
    psychTestR::final_page(paste0("You have completed the ", task_name))
  } else {
    psychTestR::one_button_page("Click to proceed to the next test.")
  }
}

