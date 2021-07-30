#' Get SNR pages
#'
#' @return
#' @export
#'
#' @examples
get_SNR_pages <- function(min_SNR = 14) {
  c(
    record_background_page(),
    record_signal_page(),
    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$h2(psychTestR::i18n("Noise_Test")),
      shiny::tags$p(psychTestR::i18n("noise_test_message"))
    )),
    psychTestR::reactive_page(function(state, ...) {

      signal_file <- psychTestR::get_global("SNR_signal", state)
      noise_file <- psychTestR::get_global("SNR_noise", state)

      valid_url <- FALSE

      while(!valid_url) {
        print('while')
        valid_url <- urlFileExist(noise_file)$exists
      }

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
  return(SNR)
}


record_background_page <- function() {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = shiny::tags$div(
                                shiny::tags$h2(psychTestR::i18n("Noise_Test")),
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
                      print(input$file_url)
                      psychTestR::set_global("SNR_noise", input$file_url, state)
                    })
}

record_signal_page <- function(page_text = shiny::tags$div(
                                        shiny::tags$h2(psychTestR::i18n("Noise_Test")),
                                        shiny::tags$p(psychTestR::i18n("record_hum_page")))) {
  # a page type for recording background noise to compute signal-to-noise ratio (SNR)
  record_audio_page(page_text = page_text,
                    method = "aws_pyin",
                    record_duration = 5,
                    auto_next_page = TRUE,
                    get_answer = get_answer_save_aws_key,
                    on_complete = function(input, state, ...) {
                      print(input$file_url)
                      psychTestR::set_global("SNR_signal", input$file_url, state)
                    })
}

SNR_conclusion <- function(SNR, min_SNR) {
  if(SNR < min_SNR) {
    psychTestR::display_error(paste0(psychTestR::i18n("your_SNR_is"), " ", SNR, psychTestR::i18n("must_be_higher")))
  } else {
    psychTestR::page(ui = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Noise_Test")),
                                          shiny::tags$p(paste0(psychTestR::i18n("your_SNR_is2"), " ", SNR, psychTestR::i18n("SNR_adequate"))), psychTestR::trigger_button("next", psychTestR::i18n("Next"))),
                     get_answer = function(input, state, ...) {
                       SNR <- psychTestR::get_global("SNR", state)
                       print('get_answer SNR')
                       print(SNR)
                       list("SNR" = SNR)
                     })
  }
}

# valid_url <- FALSE
#
# while(!valid_url) {
#   valid_url <- urlFileExist(noise_file)$exists

do_SNR <- function(signal_file, noise_file) {
 async::http_get(noise_file)$
    then(async::http_stop_for_status)$
    then(function(x) {
      print('answer back!')

      SNR <- compute_SNR(signal_file = signal_file,
                        noise_file = noise_file)

      # SNR <- compute_SNR(signal_file = "https://shinny-app-source-41630.s3.amazonaws.com/23-6-2021--14-24--45.wav",
      #                    noise_file = "https://shinny-app-source-41630.s3.amazonaws.com/22-6-2021--16-11.wav")
      print(SNR)
      SNR
      #print(x)
      #res <- rjson::fromJSON(rawToChar(x$content))
      #print(res)
    })
}