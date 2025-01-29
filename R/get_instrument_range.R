


#' Get Instrument Range Pages
#'
#' @param input_type
#' @param show_musical_notation
#' @param adjust_range
#' @param test_type
#' @param concise_wording
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
get_instrument_range_pages <- function(input_type,
                                       show_musical_notation = FALSE,
                                       adjust_range = FALSE,
                                       test_type = c("voice", "instrument"),
                                       concise_wording = FALSE,
                                       asynchronous_api_mode = FALSE) {

  # A short multi-page protocol to get the user's frequency range

  stopifnot(is.scalar.character(input_type),
            is.logical(show_musical_notation),
            is.scalar.logical(adjust_range),
            is.scalar.character(test_type),
            is.scalar.logical(concise_wording),
            is.scalar.logical(asynchronous_api_mode))

  if (input_type == "microphone") {
    get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type, concise_wording = concise_wording, asynchronous_api_mode = asynchronous_api_mode)
  } else if(input_type == "midi_keyboard") {
    get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation, adjust_range = adjust_range)
  } else {
    midi_or_audio_reactive_get_note_until_satisfied(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type, asynchronous_api_mode = asynchronous_api_mode)
  }


}


get_note_until_satisfied_loop <- function(prompt_text,
                                          var_name,
                                          page_type,
                                          button_text = psychTestR::i18n("Record"),
                                          show_musical_notation = FALSE,
                                          asynchronous_api_mode = FALSE) {


  stopifnot(is.character(prompt_text) | class(prompt_text) == "shiny.tag",
            is.character(var_name), is.character(page_type), is.character(button_text),
            is.logical(show_musical_notation),
            is.scalar.logical(asynchronous_api_mode))

  async_mode_logic <- get_audio_file_job_process(asynchronous_api_mode,
                                                 state_var_name = "wav_file",
                                                 var_name_to_set_result = var_name,
                                                 var_to_take_from_message = "feedback")

  psychTestR::join(
    # Set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("user_satisfied", "No", state)
      psychTestR::set_global("number_attempts", 1L, state)
      psychTestR::set_global(var_name, NA, state)
    }),

    # Keep in loop until the participant confirms the note is correct
    psychTestR::while_loop(test = function(state, ...) {
      user_satisfied <- psychTestR::get_global("user_satisfied", state)
      note <- psychTestR::get_global(var_name, state)
      logging::loginfo("Check note in while_loop: %s", note)
      logging::loginfo("user_satisfied: %s", user_satisfied)
      logging::loginfo("user_satisfied in dict_key_to_translations: %s", user_satisfied %in% dict_key_to_translations("No") )
      logging::loginfo("user_satisfied in dict_key_to_translations || is.na(note): %s", user_satisfied %in% dict_key_to_translations("No") || is.na(note) )
      user_satisfied %in% dict_key_to_translations("No") || is.na(note)
      },
      logic = list(
        # logic page 1, get new note
        midi_or_audio(page_type, prompt_text, var_name, asynchronous_api_mode),

          # logic page 2, was everything ok with this note?
        if(asynchronous_api_mode) async_mode_logic else empty_code_block(),
        check_note_ok(var_name, page_type, show_musical_notation),
        psychTestR::code_block(function(state, answer, ...) {
          if("user_satisfied" %in% names(answer)) {
            answer <- answer$user_satisfied
          }
          logging::loginfo("setting user_satisfied: %s", answer)
          psychTestR::set_global("user_satisfied", answer, state)
        }),

        psychTestR::code_block(function(state, ...) {
          prev_number_attempts <- psychTestR::get_global("number_attempts", state)
          psychTestR::set_global("number_attempts", prev_number_attempts + 1L, state)
        })
      )
    )
  )
}



get_audio_file_job_process <- function(asynchronous_api_mode,
                                       state_var_name = "wav_file",
                                       var_name_to_set_result,
                                       var_to_take_from_message = NULL) {
  if(asynchronous_api_mode) {

    # Note this will run synchronously..

    return(psychTestR::code_block(function(state, ...) {

      filename <- psychTestR::get_global(state_var_name, state)

      count <- 0
      status <- "PENDING"

      while(count < 4 && status == "PENDING") {

        Sys.sleep(5)

        logging::loginfo("count.. %s", count)

        if(tools::file_ext(filename) != "wav") {
          filename <- paste0(filename, ".wav")
        }

        logging::loginfo("filename:s %s", filename)

        result <- musicassessrdb::get_job_status_api(filename = filename)

        logging::loginfo("result... %s", result)

        if(is.scalar.na.or.null(result)) {
          status <- "PENDING"
        } else {
          status <- result$status
        }

        logging::loginfo("status %s", status)

        count <- count + 1

      }

      logging::loginfo("Out of while...")
      logging::loginfo("result: %s", result)
      logging::loginfo("result$message %s", result$message)


      response <- tryCatch({

        if(!is.null(var_to_take_from_message)) {

          result <- result$message

          if(!substr(result, 1, 1) %in% c("{", "[")) {

            print("cond: !substr(result, 1, 1)..")

            # result <- paste0("[", result, "]")
            # print('result')
            # print(result)
            # result <- jsonlite::fromJSON(result)

            result <- result %>%
              tidy_pyin_short_json()

            print('after tidy...')
            print(result)


          } else {
            result <- jsonlite::fromJSON(result)
            result <-  result[[var_to_take_from_message]]
          }

        } else {
          result <- result$message
        }
        result

      }, error = function(err) {
        logging::logerror(err)
        NA
      })

      logging::loginfo("Setting result: %s", response)

      psychTestR::set_global(var_name_to_set_result, response, state)

    }))
  }
}
# success result$message {"feedback":[53]}
# jsonlite::fromJSON("{0.644353741,1.097142857,171.34\n}")


tidy_pyin_short_json <- function(str) {

  res <- str %>%
    stringr::str_split_1("\n")
  res <- res[!res==""]

  res <- res %>%
    purrr::map(stringr::str_split_1, ",") %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol = 3, byrow = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(onset = V1,
                  duration = V2,
                  freq = V3)

  return(res)
}


midi_or_audio_reactive_get_note_until_satisfied <- function(show_musical_notation = FALSE,
                                                           adjust_range = FALSE,
                                                           test_type = c("voice", "instrument"),
                                                           asynchronous_api_mode = FALSE) {

  audio_logic <- unlist(get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type, asynchronous_api_mode = asynchronous_api_mode))
  midi_logic <- get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation, adjust_range = adjust_range)

  midi_or_audio_reactive(midi_logic, audio_logic)
}

range_explanation_page <- function(test_type = c("voice", "instrument"), concise_wording = FALSE) {

  if(test_type == "voice") {
    if(concise_wording) {
      text <- psychTestR::i18n("range_explanation_voice_concise")
    } else {
      text <- psychTestR::i18n("range_explanation_voice")
    }
  } else {
    text <- psychTestR::i18n("range_explanation_instrument")
  }
  psychTestR::one_button_page(text, button_text = psychTestR::i18n("Next"))
}

get_note_until_satisfied_loop_audio <- function(show_musical_notation = FALSE,
                                                adjust_range = FALSE,
                                                test_type = c("voice", "instrument"),
                                                concise_wording = FALSE,
                                                asynchronous_api_mode = FALSE) {


  if(test_type == "voice") {
    low_note_text <- psychTestR::i18n("get_range_low_note_voice")
    high_note_text <- psychTestR::i18n("get_range_high_note_voice")

  } else {
    low_note_text <- psychTestR::i18n("get_range_low_note")
    high_note_text <- psychTestR::i18n("get_range_high_note")
  }

  psychTestR::join(
    range_explanation_page(test_type, concise_wording),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), low_note_text), var_name = "bottom_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation, asynchronous_api_mode = asynchronous_api_mode),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), high_note_text), var_name = "top_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation, asynchronous_api_mode = asynchronous_api_mode),
    present_range(show_musical_notation, adjust_range, test_type)
  )
}

get_note_until_satisfied_loop_midi <- function(show_musical_notation = FALSE, adjust_range = FALSE) {
  c(
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    present_range(show_musical_notation, adjust_range)
  )
}



check_note_ok <- function(var_name, page_type, show_musical_notation = FALSE) {

  stopifnot(is.character(var_name), is.character(page_type), is.logical(show_musical_notation))

  psychTestR::reactive_page(function(answer, state, ...) {

    transpose <- psychTestR::get_global("transpose_visual_notation", state)
    transpose <- if(is.null(transpose)) 0L else transpose
    clef <- psychTestR::get_global("clef", state)
    clef <- if(is.null(clef)) "auto" else clef

    if(transpose != 0) {
      transposed_note_message <- psychTestR::i18n("transposed")
    } else {
      transposed_note_message <- " "
    }

    print("page_type...")
    print(page_type)

    asynchronous_api_mode <- psychTestR::get_global("asynchronous_api_mode", state)

    if(page_type == "record_audio_page") {
      if(asynchronous_api_mode) {

        note <- psychTestR::get_global(var_name, state)
        print('just before sort async note')
        print(note)
        note <- sort_async_note(note)
        print('just after')
        print(note)

      } else {
        note <- answer$user_response
      }
    } else {
      note <- answer$note
    }

    logging::loginfo("note: %s", note)

    if(length(note) == 0) {
      note <- NA
    }

    print('notees..')
    print(note)

    if(is.na(note)) {
      page <- psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p(psychTestR::i18n("nothing_entered")),
        shiny::tags$p(psychTestR::i18n("audio_error_suggestion"))))

    } else {

      musical_notation <- present_musical_notation_range_page(show_musical_notation, note, transpose, var_name, transposed_note_message)

      if(length(note) > 0) {
        psychTestR::set_global(var_name, note, state)
      }

      page <- present_stimuli(stimuli = note,
                              stimuli_type = "midi_notes",
                              display_modality = "auditory",
                              page_text = shiny::tags$div(psychTestR::i18n("correct_note_message"), musical_notation),
                              page_type = "NAFC_page",
                              choices = c(psychTestR::i18n("Yes"),
                                          psychTestR::i18n("No")),
                              label = var_name,
                              play_button_text = psychTestR::i18n("Play"),
                              clef = clef)

      # stimulus <- present_stimuli(stimuli = note,
      #                             stimuli_type = "midi_notes",
      #                             display_modality = "auditory",
      #                             play_button_text = psychTestR::i18n("Play"),
      #                             clef = clef)
      #
      # ui <- shiny::tags$div(stimulus, psychTestR::i18n("correct_note_message"), musical_notation)
      #
      # print(ui)
      #
      # page <- psychTestR::NAFC_page(label = var_name,
      #                               prompt = ui,
      #                               choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
      #                               labels = c(psychTestR::i18n("Yes"), psychTestR::i18n("No"))
      #                               )

    }

    return(page)
  })
}


sort_async_note <- function(note) {
  # To handle the case where the object comes back not as proper JSON
  if(is.data.frame(note)) {
    if(ncol(note) == 3) {
      note <- round(mean(hrep::freq_to_midi(note$freq), na.rm = TRUE))
    }
  } else if(length(note) == 1) {
    return(note)
  } else {
    note <- NA
  }
  return(note)
}
present_musical_notation_range_page <- function(show_musical_notation, note, transpose, var_name, transposed_note_message) {


  if(show_musical_notation) {
    musical_notation <- present_stimuli(stimuli = note + transpose,
                                        stimuli_type = "midi_notes",
                                        display_modality = "visual",
                                        label = var_name,
                                        play_button_text = psychTestR::i18n("Play"))

    shiny::tags$div(musical_notation, shiny::tags$p(transposed_note_message))

  } else {
    musical_notation <- " "
  }
}

determine_span <- function(highest_user_note, lowest_user_note, adjust_range) {
  # ideally we want to have a span of at least an octave
  # if the user performs the range test properly the span is simply highest_user_note - lowest_user_note
  # however, if they don't perform the range test well, try and determine a sensible range

  highest_user_note <- max(highest_user_note, lowest_user_note)
  lowest_user_note <- min(highest_user_note, lowest_user_note)

  span <- highest_user_note - lowest_user_note

  if(adjust_range) {
    if(span < 12) {
      m <- round(mean(lowest_user_note:highest_user_note))
      highest_user_note <- m + 6
      lowest_user_note <- m - 6
      span <- 12
    }
  }

  list("span" = span,
       "highest_user_note" = highest_user_note,
       "lowest_user_note" = lowest_user_note)

}


present_range <- function(show_musical_notation = FALSE, adjust_range = FALSE, test_type = c("voice", "instrument")) {

  stopifnot(is.scalar.logical(show_musical_notation), is.scalar.logical(adjust_range))

  if(adjust_range) warning("Adjusting range")

  if(match.arg(test_type) == "voice") {
    range_adjust_message <- psychTestR::i18n("range_adjust_message_voice")
  } else {
    range_adjust_message <- psychTestR::i18n("range_adjust_message")
  }

  psychTestR::reactive_page(function(state, ...) {

    lowest_user_note <- psychTestR::get_global("bottom_range", state)
    highest_user_note <- psychTestR::get_global("top_range", state)

    span_result <- determine_span(highest_user_note, lowest_user_note, adjust_range)

    range <- c(span_result$lowest_user_note, span_result$highest_user_note)

    psychTestR::set_global("span", span_result$span, state)
    psychTestR::set_global("top_range", span_result$highest_user_note, state)
    psychTestR::set_global("bottom_range", span_result$lowest_user_note, state)


    page <- present_stimuli(stimuli = range,
                    stimuli_type = "midi_notes",
                    display_modality = both_or_auditory(show_musical_notation),
                    page_text = shiny::tags$div(
                      shiny::tags$p(psychTestR::i18n("hear_range")),
                      shiny::tags$p(range_adjust_message)
                    ),
                    page_type = "one_button_page",
                    button_text = psychTestR::i18n("Next"))


    page
  })
}


both_or_auditory <- function(both) {
  if(both) {
    "both"
  } else {
    "auditory"
  }
}


midi_or_audio <- function(type, prompt_text, var_name, asynchronous_api_mode) {

  # Make sure to clear vars in JS
  prompt_text <- shiny::tags$div(
    set_melodic_stimuli("NA", "NA"),
    prompt_text
  )

  get_db_vars_range_page <- function(state) {

    db_vars <- if(asynchronous_api_mode) {

      module <- psychTestR::get_local(".module", state)

      list(
        stimuli = "NA",
        stimuli_durations = "NA",
        trial_time_started = Sys.time(),
        instrument = psychTestR::get_global("inst", state),
        attempt = psychTestR::get_global("number_attempts", state),
        item_id = NA,
        display_modality = "auditory",
        phase = "setup",
        rhythmic = NA,
        session_id = get_promise_value(psychTestR::get_global("session_id", state)),
        test_id = 1L,
        user_id = NA,
        review_items_id = NA,
        new_items_id = NA,
        feedback = TRUE,
        feedback_type = "produced_note",
        melody_block_paradigm = NA,
        trial_paradigm = "setup_sing_range_note",
        additional = NA,
        file_type = NA,
        noise_filename = NA,
        module = module
      )
    } else NULL
  }

  if (type == "record_audio_page") {

    psychTestR::reactive_page(function(state, ...) {

      record_audio_page(page_text = prompt_text,
                        label = var_name,
                        db_vars = get_db_vars_range_page(state),
                        asynchronous_api_mode = asynchronous_api_mode,
                        get_answer = if(asynchronous_api_mode) get_answer_add_trial_and_compute_trial_scores_s3 else get_answer_average_frequency_ff("round"),
                        button_text = psychTestR::i18n("Record"),
                        stop_button_text = psychTestR::i18n("Stop"))

    })

  } else {
    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(page_text = prompt_text,
                       label = var_name,
                       db_vars = get_db_vars_range_page(state),
                       get_answer = get_answer_midi_note_mode,
                       midi_device = midi_device,
                       button_text = psychTestR::i18n("Record"),
                       stop_button_text = psychTestR::i18n("Stop"))
    })
  }
}


#' A page to identify a user's singing range by asking them to sing Happy Birthday
#'
#' @param feedback
#' @param label
#' @param text
#'
#' @return
#' @export
#'
#' @examples
sing_happy_birthday_page <- function(feedback = FALSE, label = "sing_hbd", text = psychTestR::i18n("sing_hbd")) {

  page <- record_audio_page(label = label,
                            page_text = text,
                            get_answer = musicassessr::get_answer_simple_pyin_summary)

  if(feedback) {
    psychTestR::join(
      page,
      psychTestR::reactive_page(function(state, answer, ...) {
        psychTestR::one_button_page(
          shiny::tags$div(
            shiny::tags$h1("Output"),
            shiny::tags$p(paste0('Min: ', answer$Min.)),
            shiny::tags$p(paste0('Max: ', answer$Max.)),
            shiny::tags$p(paste0('Mean: ', answer$Mean)),
            shiny::tags$p(paste0('Median: ', answer$Median))
          )
        )
      })
    )
  } else {
    page
  }
}



