


#' Get SNR pages
#'
#' @param min_SNR
#' @param absolute_url
#' @param report_SNR
#' @param allow_SNR_failure
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages <- function(min_SNR = 14, absolute_url = character(), report_SNR = FALSE, allow_SNR_failure = FALSE, asynchronous_api_mode = FALSE) {

  psychTestR::join(
    record_background_page(asynchronous_api_mode),
    record_signal_page(asynchronous_api_mode = asynchronous_api_mode),
    SNR_preconclusion_page(),
    # if(asynchronous_api_mode) get_audio_file_job_process(asynchronous_api_mode, state_var_name = "SNR_noise", var_name_to_set_result = "SNR"),
    if(asynchronous_api_mode) get_SNR_feedback_async_mode(),
    if(!asynchronous_api_mode) { psychTestR::code_block(function(state, ...) {

      if(length(absolute_url) > 0) {

        signal_file <- paste0(absolute_url, 'audio/', psychTestR::get_global("SNR_signal", state))
        noise_file <- paste0(absolute_url, 'audio/',psychTestR::get_global("SNR_noise", state))


        valid_url <- FALSE

        while(!valid_url) {
          valid_url <- urlFileExist(noise_file)$exists
        }
      } else {

        signal_file <- paste0(absolute_url, 'audio/', psychTestR::get_global("SNR_signal", state))
        noise_file <- paste0(absolute_url, 'audio/',psychTestR::get_global("SNR_noise", state))


        valid_file <- FALSE

        while(!valid_file) {
          valid_file <- file.exists(noise_file)
        }

      }


      SNR <- compute_SNR(signal_file = signal_file, noise_file = noise_file)

      psychTestR::set_global("SNR", SNR, state)

    })},
     # Conclude
    psychTestR::reactive_page(function(state, ...) {
      SNR <- psychTestR::get_global("SNR", state)
      logging::loginfo("SNR: %s", SNR)
      if(asynchronous_api_mode) {
        SNR <- as.numeric(stringr::str_remove(SNR, "SNR:"))
      }
      logging::loginfo("SNR: %s", SNR)
      SNR_conclusion(SNR, min_SNR, report_SNR, allow_SNR_failure)
    })


  )

}


get_SNR_feedback_async_mode <- function() {

  psychTestR::reactive_page(function(state, ...) {

    noise_file <- psychTestR::get_global("SNR_noise", state)

    ui <- shiny::tags$div(
      shiny::tags$p("Seeing how noisy your room is..."),
      shiny::tags$script( htmltools::HTML( paste0('file_url = "', noise_file, '";')  ) ),
      get_async_data_ui("handleFeedbackSNR")
    )

  psychTestR::page(ui, get_answer = function(input, state, ...) {
    res <- round(as.numeric(stringr::str_remove(input$API_DATA_RESPONSE, "SNR:")))
    logging::loginfo("SNR res: %s", res)
    psychTestR::set_global("SNR", res, state)
  })

  })


}


#' Get SNR pages as a loop (i.e., the participant can take multiple attempts)
#'
#' @param min_SNR
#' @param absolute_url
#' @param report_SNR
#' @param allow_SNR_failure
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages_loop <- function(min_SNR = 14, absolute_url = character(), report_SNR, allow_SNR_failure = FALSE, asynchronous_api_mode = FALSE) {

  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("found_SNR", FALSE, state)
    }),
    psychTestR::while_loop(test = function(state, ...) {
      !psychTestR::get_global("found_SNR", state)
    }, logic = psychTestR::join(
        record_background_page(asynchronous_api_mode),
        record_signal_page(asynchronous_api_mode = asynchronous_api_mode),
        SNR_preconclusion_page(),
        # if(asynchronous_api_mode) get_audio_file_job_process(asynchronous_api_mode, state_var_name = "SNR_noise", var_name_to_set_result = "SNR"),
        if(asynchronous_api_mode) get_SNR_feedback_async_mode(),
        if(!asynchronous_api_mode) { psychTestR::code_block(function(state, ...) {

          if(length(absolute_url) > 0) {

            signal_file <- paste0(absolute_url, 'audio/', psychTestR::get_global("SNR_signal", state))
            noise_file <- paste0(absolute_url, 'audio/',psychTestR::get_global("SNR_noise", state))

            valid_url <- FALSE

            while(!valid_url) {
              valid_url <- urlFileExist(noise_file)$exists
            }
          } else {

            signal_file <- paste0('www/audio/', psychTestR::get_global("SNR_signal", state))
            noise_file <- paste0('www/audio/',psychTestR::get_global("SNR_noise", state))


            valid_file <- FALSE

            while(!valid_file) {
              valid_file <- file.exists(noise_file)
            }

          }

          SNR <- compute_SNR(signal_file = signal_file, noise_file = noise_file)

          psychTestR::set_global("SNR", SNR, state)

        })},

        psychTestR::reactive_page(function(state, ...) {
          SNR <- psychTestR::get_global("SNR", state)
          logging::loginfo("SNR: %s", SNR)
          if(asynchronous_api_mode) {
            SNR <- as.numeric(stringr::str_remove(SNR, "SNR:"))
          }
          logging::loginfo("SNR: %s", SNR)
          SNR_conclusion_loop(SNR, min_SNR, report_SNR, allow_SNR_failure)
        })

      ))
  )
}

compute_SNR <- function(signal_file, noise_file) {

  if( grepl("http", signal_file) | grepl("http", noise_file)  ) {
    signal <- warbleR::read_wave(signal_file)
    noise <- warbleR::read_wave(noise_file)
  } else {
    signal <- tuneR::readWave(signal_file)
    noise <- tuneR::readWave(noise_file)
  }
  # nice interpretation: https://reviseomatic.org/help/e-misc/Decibels.php
  signal <- seewave::env(signal, f = 44100)
  noise <- seewave::env(noise, f = 44100)
  SNR <- 20 * log10(abs(seewave::rms(signal)-seewave::rms(noise))/seewave::rms(noise))
  SNR <- round(SNR, 2)
  SNR <- if(is.nan(SNR) | SNR == "NaN") NA else SNR

  return(SNR)
}


record_background_page <- function(asynchronous_api_mode) {
  # A page type for recording background noise to compute signal-to-noise ratio (SNR)

  psychTestR::reactive_page(function(state, ...) {

    if(asynchronous_api_mode) {
      db_vars <- create_db_vars_template()
      db_vars$file_type <- "noise"
      db_vars$phase <- "setup"
    } else {
      db_vars <- NULL
    }

    record_audio_page(page_text = shiny::tags$div(
      set_melodic_stimuli("NA", "NA"),
      shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
      shiny::tags$p(psychTestR::i18n("record_bg")),
      shiny::tags$p(psychTestR::i18n("record_bg2")),
      shiny::tags$p(psychTestR::i18n("record_bg3"))
    ),
    label = "SNR_test_background",
    record_duration = 5,
    get_answer = get_answer_save_audio_file,
    button_text = psychTestR::i18n("record_bg_button"),
    on_complete = function(input, state, ...) {
      psychTestR::set_global("SNR_noise", input$file_url, state)
    },
    db_vars = db_vars)

  })

}

# A page type for recording background noise to compute signal-to-noise ratio (SNR)
record_signal_page <- function(page_text = shiny::tags$div(
                                        shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                        shiny::tags$p(psychTestR::i18n("record_signal_message"))),
                               asynchronous_api_mode = FALSE) {

  psychTestR::reactive_page(function(state, ...) {

    if(asynchronous_api_mode) {
      db_vars <- create_db_vars_template()
      db_vars$file_type <- "signal"
      snr_noise <- psychTestR::get_global("SNR_noise", state)
      logging::loginfo("SNR_noise: %s", snr_noise)
      db_vars$noise_filename <- snr_noise
      db_vars$feedback <- FALSE
      db_vars$phase <- "setup"
    } else {
      db_vars <- NULL
    }


    record_audio_page(page_text = shiny::tags$div(set_melodic_stimuli("NA", "NA"), page_text),
                      label = "SNR_test_signal",
                      record_duration = 5,
                      on_complete = function(input, state, ...) {
                        psychTestR::set_global("SNR_signal", input$file_url, state)
                      },
                      db_vars = db_vars)

  })

}


SNR_conclusion_loop <- function(SNR, min_SNR, report_SNR = TRUE, allow_SNR_failure = FALSE) {

  if(is.na(SNR)) {
    psychTestR::one_button_page(psychTestR::i18n("response_not_understood"))
  } else if(SNR < min_SNR) {
    display_SNR_result(report_SNR, SNR, page = TRUE, allow_SNR_failure = allow_SNR_failure)
  } else {

    SNR_message <- if(report_SNR) paste0(psychTestR::i18n("your_SNR_is2"), " ", SNR, psychTestR::i18n("SNR_adequate")) else psychTestR::i18n("bg_noise_level_good")

    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                          shiny::tags$p(SNR_message),
                                          psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
                     get_answer = function(input, state, ...) {
                       SNR <- psychTestR::get_global("SNR", state)
                       psychTestR::set_global("found_SNR", TRUE, state)
                       list("SNR" = SNR)
                     })
  }
}

SNR_conclusion <- function(SNR, min_SNR, report_SNR = FALSE, allow_SNR_failure = FALSE) {

  if(is.na(SNR)) {
    psychTestR::display_error(psychTestR::i18n("response_not_understood"))
  } else if(SNR < min_SNR) {
    display_SNR_result(report_SNR, SNR, page = FALSE, allow_SNR_failure = allow_SNR_failure)
  } else {

    SNR_message <- if(report_SNR) paste0(psychTestR::i18n("your_SNR_is2"), " ", SNR, psychTestR::i18n("SNR_adequate")) else psychTestR::i18n("bg_noise_level_good")

    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                          shiny::tags$p(SNR_message),
                                          psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
                     get_answer = function(input, state, ...) {
                       SNR <- psychTestR::get_global("SNR", state)
                       list("SNR" = SNR)
                     })
  }
}

display_SNR_result <- function(report_SNR, SNR, page, allow_SNR_failure = FALSE) {

  if(report_SNR) {
    message <- paste0(psychTestR::i18n("your_SNR_is"), " ", SNR, psychTestR::i18n("must_be_higher"))
  } else {
    message <- psychTestR::i18n("SNR_failure")
  }

  if(page & allow_SNR_failure) {
    message <- paste0(psychTestR::i18n("your_SNR_is"), " ", SNR, ". ", psychTestR::i18n("sure_continue"))
    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                          shiny::tags$p(message),
                                          psychTestR::trigger_button("try_again", psychTestR::i18n("Try_Again")),
                                          psychTestR::trigger_button("continue_anyway", psychTestR::i18n("Continue_Anyway"))
                                          ),
                     get_answer = function(input, state, ...) {
                      if(input$last_btn_pressed == "continue_anyway") {
                        psychTestR::set_global("found_SNR", TRUE, state)
                      } else {
                        psychTestR::set_global("found_SNR", FALSE, state)
                      }
                       SNR <- psychTestR::get_global("SNR", state)
                       list("SNR" = SNR)
                     })

  } else if(page & !allow_SNR_failure) {

    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                          shiny::tags$p(message),
                                          psychTestR::trigger_button("next", psychTestR::i18n("Try_Again"))),
                     get_answer = function(input, state, ...) {
                       SNR <- psychTestR::get_global("SNR", state)
                       list("SNR" = SNR)
                     })

  } else {
    psychTestR::display_error(message)
  }

}

SNR_preconclusion_page <- function() {
  psychTestR::one_button_page(shiny::tags$div(
    shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
    shiny::tags$p(psychTestR::i18n("noise_test_message"))
  ), button_text = psychTestR::i18n("Next"))
}


