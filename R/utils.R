


get_os <- function(){
  # osx, windows or linux
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


flatten_no_item_list <- function(x) if(is.list(x)) sum(unlist(x)) else x


is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.integerlike <- function(x) {
  all(round(x) == x)
}

#' Check if an object is a scalar logical
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

#' Check if an object is an NA (of length 1)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is.scalar.na <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is.scalar.null <- function(x) {
  all(is.null(x)) & length(x) == 0
}

is.scalar.na.or.null <- function(x) {
  is.scalar.na(x) | is.scalar.null(x)
}

is.scalar.na.or.null.or.length.zero <- function(x) {
  is.scalar.na(x) | is.scalar.null(x) | length(x) == 0
}



#' Is NULL or not all TRUE?
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_null_or_not_all_TRUE <- function(x) {
  ! all(x) | is.null(x)
}

is_midi_note <- function(n) {
  n %in% 0:127
}


tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

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


#' Set response type for a test manually
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
set_response_type <- function(type = c("Microphone", "MIDI")) {
  psychTestR::code_block(function(state, ...) {

    # Before updating, pick up if there was a previous response type, to switch back at the end of a test, if need be
    previous_response_type <- psychTestR::get_global("response_type", state)
    psychTestR::set_global("previous_response_type", previous_response_type, state)

    if(type == "microphone") {
      type <- "Microphone"
    }

    if(type == "midi_keyboard") {
      type <- "MIDI"
    }

    psychTestR::set_global("response_type", type, state)

  })
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
#' @param scale_length
#'
#' @return
#' @export
#'
#' @examples
insert_item_into_every_other_n_position_in_list <- function(l, item_to_add, n = 2, scale_length = 1) {

  original_length <- length(l)

  original_length <- scale_length * original_length

  for (i in 1:original_length) {
    after <- (i * n)-1
    logging::loginfo("after: %s", (i * n)-1)
    l <- append(l, item_to_add, after = after)
  }
  l
}


insert_item_into_every_other_n_position_in_list_with_proportion <- function(l, item_to_add, n = 2, scale_pr = 1) {

  orig_len <- length(l)/scale_pr

  if(!is.integerlike(orig_len)) {
    stop("This should end up being an intenger")
  }

  for (i in seq_along(l)) {
    pr <- i/orig_len
    pr <- as.integer(pr * 100)
    pr <- scale_pr * pr
    l <- append(l, item_to_add(progress = pr), after = (i * n)-1)
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



#' Expand a string dataframe row to a dataframe
#'
#' @param df
#' @param row_id
#'
#' @return
#' @export
#'
#' @examples
expand_string_df_row <- function(df, row_id = NULL) {

  if(!is.null(row_id)) {
    df <- df %>% dplyr::slice(row_id)
  }

  stopifnot(nrow(df) == 1L)

  out <- apply(df, MARGIN = 2, function(col) {
    c <- unlist(col)
    if(is.scalar.na(c)) {
      NA
    } else if(is.character(c)) {
      itembankr::str_mel_to_vector(c)
    } else {
      c
    }

  })
  tibble::as_tibble(out)
}

# t <- expand_string_df_row(tibble::tibble(a = "60,62", b = NA))

set_answer_meta_data <- function(meta_data) {

  if(is.data.frame(meta_data)) {
    meta_data <- jsonlite::toJSON(meta_data)
  }

  paste0('Shiny.setInputValue(\"answer_meta_data\", ', meta_data, ');')
}


set.note.no <- function(stimuli, note_no) {
  # depending on whether a note_no argument was specified, return the correct JS script

  if (note_no == "max") {
    note_no <- '\"max\"'
  }

  if (is.null(note_no)) {
    js_script <- paste0('var stimuli = ', jsonlite::toJSON(stimuli),';')
  } else {
    js_script <- paste0('var stimuli = ', jsonlite::toJSON(stimuli),'; Shiny.setInputValue("note_no", ', note_no, ');')
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


#' List official musicassessr tests
#'
#' @param include_PDT
#'
#' @return
#' @export
#'
#' @examples
list_official_tests <- function(include_PDT = TRUE) {

  list(
    "Singing Ability Assessment (SAA)" = "SAA::SAA_standalone",
    "Play By Ear Test (PBET)"  = "PBET::PBET_standalone",
    "Sight Singing Test (SST)" = "SST::SST_standalone",
    "Sight Reading Test (SRT)" = "SRT::SRT_standalone") %>%
    { if(include_PDT) c(., list("Pitch Discrimination Test (PDT)" = "PDT::PDT_standalone")) else . }
}


#' Test acronym to its full name
#'
#' @param acronym
#'
#' @return
#' @export
#'
#' @examples
test_acronym_to_name <- function(acronym) {
  l <- list(
    "SAA" = "Singing Ability Assessment",
    "PBET" = "Play By Ear Test",
    "SST" = "Sight Singing Test",
    "SRT" = "Sight Reading Test",
    "PDT" = "Pitch Discrimination Test",
    "RTT" = "Rhythm Tapping Test"
  )
  l[[acronym]]
}


#' List tone sound types
#'
#' @return
#' @export
#'
#' @examples
list_tone_sound_types <- function() {
  # These come from Tone.js-instruments
  c('bass-electric','bassoon','cello','clarinet','contrabass','flute',
    'french-horn','guitar-acoustic','guitar-electric','guitar-nylon',
    'harmonium','harp','organ','piano','saxophone','trombone','trumpet',
    'tuba','violin', 'voice_daa', 'voice_doo', 'xylophone')
}


#' An empty code block
#'
#' @return
#' @export
#'
#' @examples
empty_code_block <- function() {
    psychTestR::code_block(function(state, ...) { })
}


NA_to_js_null <- function(x) {
  if(is.na(x)) 'null' else x
}

log_err <- function(err) {
  logging::logerror(err)
  return(NA)
}

log_warn <- function(warn) {
  logging::logwarn(warn)
  return(NA)
}


get_answer_fake <- function(input, ...) {
  list(fake = NA)
}


get_test_name <- function(test, get_local, state) {
  if(is.null(test)) {
    if(get_local) {
      test <- psychTestR::get_local("test", state)
    } else {
      test <- psychTestR::get_global("test", state)
    }
  }
  test
}

get_nrows <- function(df) {
  # Workaround for backends
  nrows <- df %>%
    dplyr::summarise(num_rows = dplyr::n()) %>%
    dplyr::pull(num_rows) %>%
    as.integer()
}

#' Get value from a promise
#'
#' @param promise
#'
#' @return
#' @export
#'
#' @examples
get_promise_value <- function(promise) {
  if(promises::is.promise(promise)) {
    result <- environment(promise[["then"]])[["private"]][["value"]]
    return(result)
  } else {
    return(promise)
  }
}



#' Print code block for testing
#'
#' @param msg
#'
#' @return
#' @export
#'
#' @examples
print_code_block <- function(msg = "test") {

  psychTestR::code_block(function(state, ...) {
    logging::loginfo(msg)
  })

}


replace_nulls <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
}

#' Unnest timeline helper
#'
#' @param tl
#'
#' @return
#' @export
#'
#' @examples
unnest_timeline <- function(tl){
  purrr::map(tl, function(item) {
    if(is.list(item)) unlist(item) else item
    })
}


# Function to convert a string with \n to HTML paragraphs
convert_to_html_paragraphs <- function(text) {
  # Split the text by newline characters
  lines <- unlist(strsplit(text, "\n"))

  # Wrap each line in a <p> tag
  wrapped_lines <- purrr::map(lines, ~ shiny::tags$p(.x))

}

is_function_or_true <- function(x){
  if(is.function(x) || x == TRUE) { # Note the x == TRUE syntax is required to parse characters
    return(TRUE)
  } else {
    return(FALSE)
  }
}



tidy_get_user_info <- function(user_info) {

  if(is.logical(user_info) || is.null(user_info)) {

    return(NA)

  } else {
    user_info <- user_info %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), function(x) {
        if(grepl("\\{", x) || grepl("\\[", x)) list(jsonlite::fromJSON(x)) else x
      })) %>%
      as.list() %>%
      jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

    return(user_info)
  }


}


log_err_but_return_na <- function(err) {
  logging::logerror(err)
  NA
}


#' A conditional to present either MIDI OR audio logic, depending on test time user decision
#'
#' @param midi_logic
#' @param audio_logic
#'
#' @returns
#' @export
#'
#' @examples
midi_or_audio_reactive <- function(midi_logic, audio_logic) {
  psychTestR::join(
    # Is MIDI?
    psychTestR::conditional(test = function(state, ...) {
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "MIDI"
    },
    logic = midi_logic),
    # Is audio?
    psychTestR::conditional(test = function(state, ...){
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "Microphone"
    },
    logic = audio_logic)
  )
}


named_chr_v_to_tib <- function(chr_v) {
  as.data.frame(t(chr_v), stringsAsFactors = FALSE) %>%
    tibble::as_tibble()
}

