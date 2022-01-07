
#' Useful (but unsophisticated) util to collapse a df into a pretty string df
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
to_string_df <- function(df) {
  df %>% dplyr::summarise_all(paste0, collapse = ",")
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


# mathematical

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

log_normal <- function(x, a = 1) exp(-(log(x)/a)^2)


# generic


insert.every.other.pos.in.list <- function(l, item_to_add, n = 2) {
  for (i in seq_along(l)) {
    l <- append(l, item_to_add, after = (i*n)-1)
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
    js_script <- paste0('var stimuli = ', rjson::toJSON(stimuli),';')
  } else {
    js_script <- paste0('var stimuli = ', rjson::toJSON(stimuli),'; Shiny.setInputValue("note_no", ', note_no, ');')
  }
  js_script
}


item_bank_type_to_stimuli_type <- function(string_of_item_bank_type) {
  if(str_detect(string_of_item_bank_type, "RDS_file")) {
    item_bank_type <- stringr::str_remove(string_of_item_bank_type, "RDS_file_")
  } else {
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

# type checking

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


# tests

#g <- as.list(1:10)
# g2 <- insert.every.other.pos.in.list(g, "a")
# g3 <- insert.every.other.pos.in.list(g, "a", n = 3)
# g4 <- insert.every.other.pos.in.list(g, "a", n = 4)

