



#' Allow the experimenter to set a condition at the beginning of the test
#'
#' @param block1
#' @param block2
#' @param condition1_name
#' @param condition2_name
#'
#' @return
#' @export
#'
#' @examples
set_condition_page <- function(block1, block2, condition1_name, condition2_name) {

  psychTestR::join(
    psychTestR::NAFC_page(
      label = "experimental_conditon",
      prompt = "NB: This is only for the experimenter. What condition should this participant be?",
      choices = as.character(1:2),
      on_complete = function(state, answer, ...) {
        answer <- as.numeric(answer)
        if(answer == 1) {
          psychTestR::set_global("condition", condition1_name, state)
        } else {
          psychTestR::set_global("condition", condition2_name, state)
        }
        psychTestR::set_global("snap", answer, state)
      }),
    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 1
    }, logic = block1),

    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 2
    }, logic = block2)
  )
}

#' Useful function for randomising which of 2 blocks goes first (or forcing it with force_snap)
#'
#' @param block1
#' @param block2
#' @param condition1_name
#' @param condition2_name
#' @param force_snap
#'
#' @return
#' @export
#'
#' @examples
psych_test_snap <- function(block1, block2, condition1_name = "1", condition2_name = "2", force_snap = NULL) {

  stopifnot(force_snap == 1 | force_snap == 2 | is.null(force_snap))

  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      if(is.null(force_snap)) {
        snap <- sample(1:2, 1)
      }
      if(snap == 1) {
        psychTestR::save_result(place = "results", "experimental_condition", condition1_name)
      } else {
        psychTestR::save_result(place = "results", "experimental_condition", condition2_name)
      }
      psychTestR::set_global("snap", snap, state)

    }),
    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 1
    }, logic = block1),

    psychTestR::conditional(test = function(state, ...) {
      psychTestR::get_global("snap", state) == 2
    }, logic = block2)
  )
}

#' Useful (but unsophisticated) util to collapse a df into a pretty string df
#'
#' @param df
#' @param exclude_cols
#'
#' @return
#' @export
#'
#' @examples
to_string_df <- function(df, exclude_cols = character()) {

  if(length(exclude_cols) > 0L) {
    df1 <- df %>% dplyr::summarise_at(dplyr::vars(-exclude_cols), paste0, collapse = ",")
    df2 <- df %>% dplyr::select(exclude_cols) %>% dplyr::slice(1)
    cbind(df1, df2)
  } else {
    df %>% dplyr::summarise_all(paste0, collapse = ",")
  }
}


# d <- data.frame(
#   a = 1:10,
#   b = LETTERS[1:10]
# )
#
# d2 <- to_string_df(d, exclude_cols = "b")


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


#' Insert item into every other n position in list
#'
#' @param l
#' @param item_to_add
#' @param n
#'
#' @return
#' @export
#'
#' @examples
insert_item_into_every_other_n_position_in_list <- function(l, item_to_add, n = 2) {
  for (i in seq_along(l)) {
    l <- append(l, item_to_add, after = (i*n)-1)
  }
  l
}

TRUE_to_js_true <- function(cond) {
  if(cond) {
    "true"
  } else {
    "false"
  }
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

if_example_save_answer <- function(example) {
  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }
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
# g2 <- insert_item_into_every_other_position_in_list(g, "a")
# g3 <- insert_item_into_every_other_position_in_list(g, "a", n = 3)
# g4 <- insert_item_into_every_other_position_in_list(g, "a", n = 4)

