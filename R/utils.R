
# low level funs

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


insert.every.other.pos.in.list <- function(l, item_to_add) {
  for (i in 1:length(l)) {
    l <- append(l, item_to_add, after = (i*2)-1)
  }
  l
}



set_answer_meta_data <- function(meta_data) {
  paste0('Shiny.setInputValue(\"answer_meta_data\", ', meta_data, ');')
}



set.note.no <- function(stimuli, note_no) {
  # depending on whether a note_no argument was specified, return the correct JS script

  if (note_no == "max") {
    note_no <- '\"max\"'
  }

  if (is.null(note_no)) {
    js_script <- paste0('var stimuli = ', toJSON(stimuli),';')
  }

  else {
    js_script <- paste0('var stimuli = ', toJSON(stimuli),'; Shiny.setInputValue("note_no", ', note_no, ');')
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

present_record_button <- function(present = FALSE, type = "aws_pyin", midi_device = NULL, interactive = FALSE, button_text = "Record") {

  if (present == TRUE & type == "crepe" |
      present == TRUE & type == "aws_pyin") {

    div(id = "button_area",
        tags$button(button_text, id = "recordButton"),
        tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"',type,'\");
                           hideRecordButton();
                            });'))
    )
  }

  else if (present == TRUE & type == "record_midi_page") {
    div(id = "button_area",
        tags$button(button_text, id = "recordButton"),
        tags$script(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                           recordAndStop(null, true, false, this.id, \"record_midi_page\");
                            hideRecordButton();
                           instantiateMIDI(\"',midi_device,'\", false); })'))
    )
  }
  else {
    div(id = "button_area")
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



# dictionary functions

dict_key_to_translations <- function(key) {
  cols <- names(dict_df)[!names(dict_df) %in% "key"]
  as.vector(unlist(dict_df[dict_df$key == key, cols]))
}

translate_from_dict <- function(non_english_translation, language) {
  as.character(dict_df[dict_df[, language] == non_english_translation, "en"])
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


have.requirements <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res %in% dict_key_to_translations("Yes")) TRUE
  else display_error(i18n("requirements_error"))
}


