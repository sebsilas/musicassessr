#' Get SNR pages
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages <- function(min_SNR = 14, absolute_url) {
  c(
    record_background_page(),
    record_signal_page(),
    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$h2("Check of background noise level"),
      shiny::tags$p(psychTestR::i18n("noise_test_message"))
    )),
    psychTestR::reactive_page(function(state, ...) {

      if(musicassessr_state == "production") {
        signal_file <- paste0(absolute_url, psychTestR::get_global("SNR_signal", state))
        noise_file <- paste0(absolute_url, psychTestR::get_global("SNR_noise", state))

        valid_url <- FALSE

        while(!valid_url) {
          valid_url <- urlFileExist(noise_file)$exists
        }
      } else {

        signal_file <- psychTestR::get_global("SNR_signal", state)
        noise_file <- psychTestR::get_global("SNR_noise", state)

        valid_file <- FALSE

        while(!valid_file) {
          valid_file <- file.exists(noise_file)
        }

      }
      print('SNR...')
      print(signal_file)
      print(noise_file)

      SNR <- compute_SNR(signal_file = signal_file,
                         noise_file = noise_file)

      psychTestR::set_global("SNR", SNR, state)

      SNR_conclusion(SNR, min_SNR)

    })
  )

}

compute_SNR <- function(signal_file, noise_file) {

  signal <- warbleR::read_wave(signal_file)
  noise <- warbleR::read_wave(noise_file)
  # nice interpretation: https://reviseomatic.org/help/e-misc/Decibels.php
  signal <- seewave::env(signal, f = 44100)
  noise <- seewave::env(noise, f = 44100)
  SNR <- 20*log10(abs(seewave::rms(signal)-seewave::rms(noise))/seewave::rms(noise))
  SNR <- round(SNR, 2)

  SNR <- ifelse(is.nan(SNR) | SNR == "NaN", NA, SNR)
  return(SNR)
}


record_background_page <- function() {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = shiny::tags$div(
                                shiny::tags$h2("Check of background noise level"),
                                shiny::tags$p(psychTestR::i18n("record_bg")),
                                shiny::tags$p(psychTestR::i18n("record_bg2")),
                                shiny::tags$p(psychTestR::i18n("record_bg3"))
                              ),
                    method = "aws_pyin",
                    record_duration = 5,
                    auto_next_page = TRUE,
                    get_answer = get_answer_save_aws_key,
                    button_text = psychTestR::i18n("record_bg_button"),
                    on_complete = function(input, state, ...) {
                      psychTestR::set_global("SNR_noise", input$file_url, state)
                    })
}

record_signal_page <- function(page_text = shiny::tags$div(
                                        shiny::tags$h2("Check of background noise level"),
                                        shiny::tags$p("When you are ready, click on “Record” and for about 5 seconds please hum, directed toward your microphone for 5 seconds.  (to “hum”, put your lips together and make a sound with your voice.)"))) {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = page_text,
                    method = "aws_pyin",
                    record_duration = 5,
                    auto_next_page = TRUE,
                    get_answer = get_answer_save_aws_key,
                    on_complete = function(input, state, ...) {
                      psychTestR::set_global("SNR_signal", input$file_url, state)
                    })
}

SNR_conclusion <- function(SNR, min_SNR, report_SNR = FALSE) {

  if(is.na(SNR)) {
    psychTestR::display_error("Your response was not understood. Perhaps your microphone is not setup correctly. Contact an administrator for assistance.")
  } else if(SNR < min_SNR) {
    if(report_SNR) {
      psychTestR::display_error(paste0(psychTestR::i18n("your_SNR_is"), " ", SNR, psychTestR::i18n("must_be_higher")))
    } else {
      psychTestR::display_error("Your vocal signal compared to the background noise is not sufficient for testing.
      Please find a quieter space, get closer to the mic, or adjust the volume and we will measure the Vocal signal sound level again.
      If neither of these is possible for you, please email silass@stud.hmtm-hannover.de with “weak signal” in the subject space, and you will be contacted within 24 hours to trouble shoot this problem.
                                You may also end the program and attempt to adjust your microphone volume and start the program again.")
    }
  } else {
    SNR_message <- ifelse(report_SNR,
                          yes = paste0(psychTestR::i18n("your_SNR_is2"), " ", SNR, psychTestR::i18n("SNR_adequate")),
                          no = "Your background noise level is good")

    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2("Check of background noise level"),
                                          shiny::tags$p(SNR_message),
                                          psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
                     get_answer = function(input, state, ...) {
                       SNR <- psychTestR::get_global("SNR", state)
                       list("SNR" = SNR)
                     })
  }
}

# valid_url <- FALSE
#
# while(!valid_url) {
#   valid_url <- urlFileExist(noise_file)$exists

# do_SNR <- function(signal_file, noise_file) {
#  async::http_get(noise_file)$
#     then(async::http_stop_for_status)$
#     then(function(x) {
#
#
#       SNR <- compute_SNR(signal_file = signal_file,
#                         noise_file = noise_file)
#
#       SNR
#
#     })
# }
