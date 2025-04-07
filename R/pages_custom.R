#' Page to select what instrument the participant is using
#'
#' @param set_range_based_on_selection Should the range be set based on the selection?
#' @param include_other_in_dropdown Include Other as an option?
#' @param with_voice Include Voice as an option?
#' @param prompt Prompt?
#'
#' @return
#' @export
#'
#' @examples
select_musical_instrument_page <- function(set_range_based_on_selection = TRUE,
                                           include_other_in_dropdown = FALSE,
                                           with_voice = FALSE,
                                           prompt = psychTestR::i18n("instrument_selection_message") ) {
  insts <- musicassessr::insts

  if(!with_voice) {
    insts <- setdiff(insts, "voice")
  }

  # Do not remove the following line to e.g., /data-raw. it has to be called within the psychTestR timeline
  insts_dict <- lapply(insts, psychTestR::i18n)

  psychTestR::dropdown_page(label = "select_musical_instrument",
                            prompt = prompt,
                            next_button_text = psychTestR::i18n("Next"),
                            choices = as.vector(unlist(insts_dict)),
                            alternative_choice = include_other_in_dropdown,,
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


midi_vs_audio_select_page <- function(prompt = psychTestR::i18n("input_selector_message")) {

  # Remake the psychTestR page so I can confirm what the user selects

  validate_fun <- get("dropdown_page.validate", asNamespace("psychTestR"))
  validate <- validate_fun(FALSE, "Other (please state)")

  prompt <- tagify(prompt)
  style <- sprintf("max-width:%ipx", 200)

  dropdown <- shiny::selectizeInput("dropdown",
                                    label = NULL,
                                    choices =  c("Microphone", "MIDI"),
                                    multiple = FALSE)



  response_ui <- shiny::div(style = style,
                            dropdown,
                            sure_you_want_to_continue_button(confirmation_msg = psychTestR::i18n("happy_with_selection"))
                            )

  ui <- shiny::div(prompt, response_ui)
  get_answer_fun <- get("dropdown_page.get_answer", asNamespace("psychTestR"))
  get_answer <- get_answer_fun("Other (please state)")
  psychTestR::page(ui = ui, label = "select_input", get_answer = get_answer, save_answer = TRUE,
                   validate = validate, on_complete = function(answer, state, ...) {
         psychTestR::set_global("response_type", answer, state)
       }, final = FALSE)
}


intervals_df <- tibble::tibble(
  interval = c(names(intervals), names(intervals)),
  interval_numeric = c(as.vector(unlist(intervals)), as.vector(unlist(intervals))),
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
                               page_title = psychTestR::i18n("interval_perception_page_title"),
                               play_button_text = psychTestR::i18n("Play"),
                               example = FALSE,
                               label = paste0("interval_perception_", trial_no),
                               save_answer = TRUE,
                               get_answer = get_answer_interval_page,
                               trial_no = NULL,
                               no_trials) {


  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }

  psychTestR::join(

      psychTestR::code_block(function(state, ...) {
        # If you set in the reactive page it resets because reactive pages get called twice
        psychTestR::set_global("trial_time_started", Sys.time(),  state)

      }),

      psychTestR::reactive_page(function(state, ...) {

      if(is.null(interval)) {
        abs_intervals <- psychTestR::get_global("abs_intervals", state)
        abs_interval <- abs_intervals[trial_no, ]
        psychTestR::set_global("answer_meta_data", abs_interval,  state)
      }

      # Do not remove the following line to e.g., /data-raw. it has to be called within the psychTestR timeline
      # intervals_dict <- lapply(intervals, psychTestR::i18n)
      intervals_dict <- lapply(paste0("interval_", 0:12), psychTestR::i18n)

      ui <- shiny::tags$div(
        shiny::tags$h2(page_title),
        shiny::tags$h3(paste0(psychTestR::i18n("Section_Progress"), ': ', trial_no, "/", no_trials)),
        present_stimuli(stimuli = itembankr::str_mel_to_vector(abs_interval$abs_interval),
                        stimuli_type = "midi_notes",
                        display_modality = "auditory",
                        page_label = label,
                        play_button_text = play_button_text,
                        sound = "piano"),
        shiny::selectInput(inputId = "dropdown",
                           label = NULL,
                           choices = unlist(as.vector(intervals_dict)), #names(intervals),
                           width = "30%"),
        sure_you_want_to_continue_button(confirmation_msg = psychTestR::i18n("happy_with_selection"))
    )

      psychTestR::page(ui = ui,
                       label = label,
                       get_answer = get_answer,
                       save_answer = save_answer)
    })
  )

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
                                page_title = psychTestR::i18n("interval_perception_page_title")) {

  psychTestR::module(label = "interval_perception",
    purrr::map(1:num_items, function(trial_no) {
      play_interval_page(trial_no = trial_no, page_title = page_title, no_trials = num_items)
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
redirect_page <- function(text = psychTestR::i18n("redirect_message2"), ms = 5000, url = "http://www.google.com", final = TRUE) {

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
wait_for_api_page <- function(poll_frequency_seconds = 5L,
                              check_function = check_session_id_ready) {

  poll_frequency_ms <- poll_frequency_seconds * 1000

  psychTestR::join(


    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("no_times_api_polled", 0L, state)
    }),

    # Wait page

    psychTestR::while_loop(test = function(state, ...) {

      logging::loginfo("Waiting for API response...")

      no_times_api_polled <- psychTestR::get_global("no_times_api_polled", state) + 1L

      logging::loginfo("Check no. %s/3", no_times_api_polled)

      psychTestR::set_global("no_times_api_polled", no_times_api_polled, state)

      not_ready <- check_function(state)

      psychTestR::set_global("api_not_ready", not_ready, state)

      not_ready && psychTestR::get_global("no_times_api_polled", state) < 4L

    }, # Wait for API
      wait_for_api_page_ui(poll_frequency_ms) ),


    # If the API has been polled three times, cancel on the next go
    psychTestR::conditional(function(state, ...) {
      psychTestR::get_global("api_not_ready", state) && psychTestR::get_global("no_times_api_polled", state) == 4L
    }, redirect_on_failure_page() )


  )


}


redirect_on_failure_page <- function(redirect_url = "https://google.com") {
  psychTestR::reactive_page(function(state, ...) {

    if(is.scalar.character(psychTestR::get_global("redirect_on_failure_url", state))) {
      redirect_url <-  psychTestR::get_global("redirect_on_failure_url", state)
    }

    logging::loginfo("Redirecting to %s", redirect_url)

    redirect_page(text = "Etwas ist schiefgelaufen. Du wirst weitergeleitet.",
                  url = redirect_url,
                  ms = 3000)

  })
}


check_session_id_ready <- function(state) {

  logging::loginfo("Check session_id ready..")

  session_id <- get_promise_value(psychTestR::get_global("session_id", state))

  if(is.list(session_id)) {
    if(is.null(session_id$session_id)) {
      cat(file=stderr(),"session_id$session_id is NULL")
    } else {
      cat(file=stderr(), "session_id$session_id", session_id$session_id, "\n")
    }
  }

  not_ready <- is.null(session_id) || is.scalar.na(session_id)

  cat(file=stderr(), "not_ready", not_ready, "\n")

  if(not_ready) {
    logging::loginfo("Session ID not ready, trying again...")
  } else {
    if(is.scalar.na.or.null.or.length.zero(session_id)) {
      stop("Could not get session_id.")
    } else {
      if("session_id" %in% names(session_id)) {
        session_id <- session_id$session_id
      }
        cat(file=stderr(), "set session_id...", session_id, "\n")
        psychTestR::set_global("session_id", session_id, state)
      }
  }

  not_ready
}


# t <- get_select_items_job_status("9614d04d-4d16-48ff-a571-ff7ae65a2da5")

# t <- musicassessrdb::get_job_status_api("9614d04d-4d16-48ff-a571-ff7ae65a2da5")

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

    if(is.scalar.na(api_response)) {

      stop("get_job_status_api FAILED")

    } else if(api_response$status == "FINISHED") {

      items <- api_response %>%
        purrr::pluck("message") %>%
        jsonlite::fromJSON()

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

  ui <- shiny::tags$div(shiny::tags$p(psychTestR::i18n("wait_message")),
                        shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/bird.png',
                                        height = 200,
                                        width = 200,
                                        id = "volumeMeter"),
                        shiny::tags$script('setTimeout(function() { next_page(); }, ', poll_frequency_ms, ');'))

  empty_page(ui)
}
