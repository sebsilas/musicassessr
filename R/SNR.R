


#' Get SNR pages
#'
#' @param min_SNR
#' @param absolute_url
#' @param report_SNR
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages <- function(min_SNR = 14, absolute_url = character(), report_SNR = FALSE) {
  psychTestR::join(
    record_background_page(),
    record_signal_page(),
    SNR_preconclusion_page(),
    psychTestR::reactive_page(function(state, ...) {

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

      SNR_conclusion(SNR, min_SNR, report_SNR)

    })
  )

}



#' Get SNR pages as a loop (i.e., the participant can take multiple attempts)
#'
#' @param min_SNR
#' @param absolute_url
#' @param report_SNR
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages_loop <- function(min_SNR = 14, absolute_url = character(), report_SNR) {

  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("found_SNR", FALSE, state)
    }),
    psychTestR::while_loop(test = function(state, ...) {
      psychTestR::get_global("found_SNR", state) == FALSE
    }, logic = psychTestR::join(
        record_background_page(),
        record_signal_page(),
        SNR_preconclusion_page(),
        psychTestR::reactive_page(function(state, ...) {

          if(length(absolute_url) > 0) {

            signal_file <- paste0(absolute_url, 'audio/', psychTestR::get_global("SNR_signal", state))
            noise_file <- paste0(absolute_url, 'audio/',psychTestR::get_global("SNR_noise", state))

            print(signal_file)
            print(noise_file)

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

          SNR_conclusion_loop(SNR, min_SNR, report_SNR)

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
  SNR <- 20*log10(abs(seewave::rms(signal)-seewave::rms(noise))/seewave::rms(noise))
  SNR <- round(SNR, 2)
  SNR <- if(is.nan(SNR) | SNR == "NaN") NA else SNR

  return(SNR)
}


record_background_page <- function() {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = shiny::tags$div(
                                shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                shiny::tags$p(psychTestR::i18n("record_bg")),
                                shiny::tags$p(psychTestR::i18n("record_bg2")),
                                shiny::tags$p(psychTestR::i18n("record_bg3"))
                              ),
                    label = "SNR_test_background",
                    record_duration = 5,
                    auto_next_page = TRUE,
                    get_answer = get_answer_save_aws_key,
                    button_text = psychTestR::i18n("record_bg_button"),
                    on_complete = function(input, state, ...) {
                      psychTestR::set_global("SNR_noise", input$file_url, state)
                    })
}

record_signal_page <- function(page_text = shiny::tags$div(
                                        shiny::tags$h2(psychTestR::i18n("check_of_bg_noise")),
                                        shiny::tags$p(psychTestR::i18n("record_signal_message")))) {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = page_text,
                    label = "SNR_test_signal",
                    record_duration = 5,
                    auto_next_page = TRUE,
                    get_answer = get_answer_save_aws_key,
                    on_complete = function(input, state, ...) {
                      psychTestR::set_global("SNR_signal", input$file_url, state)
                    })
}


SNR_conclusion_loop <- function(SNR, min_SNR, report_SNR = TRUE) {

  #browser()

  if(is.na(SNR)) {
    psychTestR::one_button_page(psychTestR::i18n("response_not_understood"))
  } else if(SNR < min_SNR) {
    display_SNR_result(report_SNR, SNR, page = TRUE)
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

SNR_conclusion <- function(SNR, min_SNR, report_SNR = FALSE) {

  if(is.na(SNR)) {
    psychTestR::display_error(psychTestR::i18n("response_not_understood"))
  } else if(SNR < min_SNR) {
    display_SNR_result(report_SNR, SNR, page = FALSE)
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

display_SNR_result <- function(report_SNR, SNR, page) {

  if(report_SNR) {
    message <- paste0(psychTestR::i18n("your_SNR_is"), " ", SNR, psychTestR::i18n("must_be_higher"))
  } else {
    message <- psychTestR::i18n("SNR_failure")
  }
  if(page) {
    psychTestR::one_button_page(message,  button_text = psychTestR::i18n("Try_Again"))
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


