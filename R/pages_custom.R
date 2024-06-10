#' Page to select what instrument the participant is using
#'
#' @param set_range_based_on_selection
#' @param include_other_in_dropdown
#'
#' @return
#' @export
#'
#' @examples
select_musical_instrument_page <- function(set_range_based_on_selection = FALSE,
                                           include_other_in_dropdown = FALSE) {


  # do not remove the following line to e.g., /data-raw. it has to be called within the psychTestR timeline
  insts_dict <- lapply(musicassessr::insts, psychTestR::i18n)

  psychTestR::dropdown_page(label = "select_musical_instrument",
                prompt = psychTestR::i18n("instrument_selection_message"),
                next_button_text = psychTestR::i18n("Next"),
                choices = as.vector(unlist(insts_dict)),
                alternative_choice = TRUE,
                alternative_text = psychTestR::i18n("other_please_state"),
                on_complete = function(state, answer, ...) {
                  language <- psychTestR::get_url_params(state)$language

                  if(language != "en") {
                    answer <- translate_from_dict(non_english_translation = answer, language = language)
                  }

                  instrument_id <- musicassessrdb::instrument_name_to_id(answer)

                  set_instrument(instrument_id, as_code_block = FALSE, state, set_range = set_range_based_on_selection)

              })

}


midi_vs_audio_select_page <- function(prompt = "How will you input into the test?") {
  psychTestR::dropdown_page(label = "select_input",
                next_button_text = psychTestR::i18n("Next"),
                prompt = prompt,
                choices = c("Microphone", "MIDI"),
                on_complete = function(answer, state, ...) {
                  psychTestR::set_global("response_type", answer, state)
                })
}


intervals_df <- tibble::tibble(
  interval = c(names(itembankr::intervals), names(itembankr::intervals)),
  interval_numeric = c(as.vector(unlist(itembankr::intervals)), as.vector(unlist(itembankr::intervals))),
  direction = c(NA, rep("ascending", 12), NA, rep("descending", 12))
)


sample_intervals <- function(ascending = TRUE, bottom_range = 48, top_range = 72, num_items = 26L) {

  psychTestR::code_block(function(state, ...) {
    abs_intervals <- intervals_df
    start_notes <- sample(bottom_range:top_range, num_items, replace = TRUE)
    abs_intervals <- abs_intervals[sample(1:nrow(abs_intervals), num_items), ]
    abs_intervals$start_note <- start_notes

    abs_interval <- apply(abs_intervals, MARGIN = 1, function(row) {
      if(row['direction'] == "ascending" | is.na(row['direction'])) {
        abs <- c(row['start_note'], as.numeric(row['start_note']) + as.numeric(row['interval_numeric']))
      } else {
        abs <- c(row['start_note'], as.numeric(row['start_note']) - as.numeric(row['interval_numeric']))
      }
      paste0(abs, collapse = ",")
    })

    abs_intervals$abs_interval <- abs_interval

    abs_intervals <- abs_intervals[sample(1:nrow(abs_intervals), nrow(abs_intervals)), ]

    psychTestR::set_global("abs_intervals", abs_intervals, state)

  })

}



play_interval_page <- function(interval = NULL,
                               page_title = "What is the interval?",
                               play_button_text = psychTestR::i18n("Play"),
                               example = FALSE,
                               label = "interval_",
                               save_answer = TRUE,
                               get_answer = get_answer_interval_page,
                               trial_no = NULL) {


  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }

  psychTestR::reactive_page(function(state, ...) {

    if(is.null(interval)) {
      abs_intervals <- psychTestR::get_global("abs_intervals", state)
      abs_interval <- abs_intervals[trial_no, ]
      psychTestR::set_global("answer_meta_data", abs_interval,  state)
    }

    psychTestR::page(ui = shiny::tags$div(
        shiny::tags$h2(page_title),
      present_stimuli(stimuli = itembankr::str_mel_to_vector(abs_interval$abs_interval),
                      stimuli_type = "midi_notes",
                      display_modality = "auditory",
                      page_label = "interval_",
                      play_button_text = play_button_text,
                      sound = "piano"),
      shiny::selectInput(inputId = "dropdown",
                         label = NULL,
                         choices = names(itembankr::intervals),
                         width = "30%"),
      psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
    label = label, get_answer = get_answer, save_answer = save_answer)

  })

}


#' Deploy a block of n trials which play back intervals
#'
#' @param num_items
#' @param page_title
#'
#' @return
#' @export
#'
#' @examples
multi_interval_page <- function(num_items = 26L,
                                page_title = "What is the interval?") {
  psychTestR::module(label = "interval_perception",
    lapply(1:num_items, function(trial_no) {
      play_interval_page(trial_no = trial_no, page_title = page_title)
    })
  )
}

#' A page for redirecting a participant after x ms
#'
#' @param text
#' @param ms
#' @param url
#' @param final
#'
#' @return
#' @export
#'
#' @examples
redirect_page <- function(text = "Thank you, you will now be redirected.", ms = 5000, url = "http://www.google.com", final = TRUE) {

  content <- shiny::tags$div(
    shiny::tags$script(paste0('setTimeout(function() {
    window.location.href = \"', url, '\";
    }, ', ms, ');')),
    shiny::tags$p(text))

  if(final) {
    psychTestR::final_page(content)
  } else {
    psychTestR::one_button_page(content)
  }

}




#' An empty page (with trigger button hidden and possible to show on demand.)
#'
#' @param body
#' @param admin_ui
#' @param button_text
#' @param on_complete
#' @param page_title
#' @param get_answer
#' @param save_answer
#' @param label
#'
#' @return
#' @export
#'
#' @examples
empty_page <- function(body = "",
                       admin_ui = NULL,
                       button_text = psychTestR::i18n("Next"),
                       on_complete = NULL, page_title = "",
                       answer_meta_data = tibble::tibble(),
                       get_answer = get_answer_meta_data,
                       save_answer = FALSE,
                       label = "") {
  body <- tagify(body)
  stopifnot(is.scalar.character(button_text))
  ui <- shiny::div(shiny::tags$script(set_answer_meta_data(answer_meta_data)), page_title, body, psychTestR::trigger_button("next", button_text, style = "visibility:hidden;"))
  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, save_answer = save_answer, get_answer = get_answer, label = label)
}

#' Wait for API page
#'
#' @param poll_frequency_seconds
#' @param check_function This should return TRUE if not ready (still need to wait for API) or FALSE if ready (api complete).
#'
#' @return
#' @export
#'
#' @examples
wait_for_api_page <- function(poll_frequency_seconds = 1L, check_function = check_session_id_ready) {

  poll_frequency_ms <- poll_frequency_seconds * 1000

  psychTestR::while_loop(test = function(state, ...) {

    logging::loginfo("Waiting for API response...")

    Sys.sleep(poll_frequency_seconds)

    check_function(state)

  }, wait_for_api_page_ui(poll_frequency_ms) )

}



check_session_id_ready <- function(state) {

  logging::loginfo("Check session_id ready..")

  session_id <- get_promise_value(psychTestR::get_global("session_id", state))

  cat(file=stderr(), "session_id$session_id", session_id$session_id, "\n")

  not_ready <- is.null(session_id)

  cat(file=stderr(), "not_ready", not_ready, "\n")

  if(not_ready) {
    logging::loginfo("Session ID not ready, trying again...")
  } else {
    if(is.scalar.na.or.null.or.length.zero(session_id)) {
      stop("Could not get session_id.")
    } else {
      if("session_id" %in% names(session_id)) {
        psychTestR::set_global("session_id", session_id$session_id, state)
      }
    }
  }

  not_ready
}


# t <- get_select_items_job_status("36f93907-ba3a-41d6-90e5-b4da4c558f06")

# t <- musicassessrdb::get_job_status_api("36f93907-ba3a-41d6-90e5-b4da4c558f06")

get_select_items_job_status <- function(state) {

  logging::loginfo("get_select_items_job_status")

  job_id <- psychTestR::get_global('job_id', state)

  if(is.null(job_id)) {

    logging::loginfo("No job_id, job not needed, so moving on")
    return(FALSE)

  } else {

    logging::loginfo("job_id %s", job_id)

    api_response <- musicassessrdb::get_job_status_api(job_id)

    logging::loginfo("api_response %s", api_response)

    if(api_response$status == "FINISHED") {

      items <- api_response %>%
        purrr::pluck("message") %>%
        rjson::fromJSON()

      logging::loginfo("items %s", items)

      new_items <- items$new_items %>%
        dplyr::bind_rows()

      logging::loginfo("new_items %s", new_items)

      review_items <- items$review_items %>%
        dplyr::bind_rows()

      logging::loginfo("review_items %s", review_items)

      psychTestR::set_global('rhythmic_melody', new_items, state)
      psychTestR::set_global('rhythmic_melody_review', review_items, state)

      return(FALSE)

    } else if(api_response$status == "PENDING") {
      return(TRUE)
    } else {
      stop("API response invalid.")
    }

  }



}

wait_for_api_page_ui <- function(poll_frequency_ms) {

  ui <- shiny::tags$div(shiny::tags$p("Please wait a few seconds."),
                        shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/bird.png', height = 200, width = 200, id = "volumeMeter"),
                        shiny::tags$script('setTimeout(function() { next_page(); }, ', poll_frequency_ms, ');'))

  empty_page(ui)
}
