

#' Setup pages for musicassessr test
#'
#' @param input
#' @param headphones
#' @param SNR_test
#' @param min_SNR
#' @param get_user_info
#' @param demo
#' @param get_instrument_range
#'
#' @return
#' @export
#'
#' @examples
setup_pages <- function(input = c("microphone",
                                  "midi_keyboard",
                                  "midi_keyboard_and_microphone",
                                  "midi_keyboard_or_microphone"),
                        headphones = TRUE,
                         SNR_test = TRUE,
                        min_SNR = 14,
                         get_user_info = TRUE,
                        demo = FALSE,
                        get_instrument_range = FALSE) {

  if(!demo) {

  psychTestR::join(

    requirements_page(headphones = headphones,
                      input = input),

    if(get_user_info) musicassessr::get_user_info_page(),

    if(headphones) musicassessr::test_headphones_page(),

    if(is_microphone_only_test(input)) musicassessr::microphone_calibration_page(),

    if(sjmisc::str_contains(input, "microphone")) microphone_type_page(),

    if(sjmisc::str_contains(input, "microphone") | sjmisc::str_contains(input, "midi")) psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("record_instructions")), shiny::tags$img(src = "musicassessr-assets/img/record.gif")), button_text = psychTestR::i18n("Next")),

    if(SNR_test) musicassessr::get_SNR_pages(),

    if(get_instrument_range) musicassessr::get_instrument_range_pages("record_audio_page")

    )
  }
}

#' Deploy Demographics via psyquest
#'
#' @param deploy
#'
#' @return
#' @export
#'
#' @examples
deploy_demographics <- function(deploy) {
  if(deploy) {
  psychTestR::join(psyquest::DEG(subscales = c("Gender", "Age", "Nationality", "Country Formative Years", "First Language")),
    psyquest::SES(subscales = c("Educational Degree")),
    psychTestR::elt_save_results_to_disk(FALSE))
  }
}


#' test_headphones_page
#' Essentially a wrapper for volume_calibration_page with a default audio file.
#' @return
#' @export
#'
#' @examples
test_headphones_page <- function() {
  psychTestR::volume_calibration_page(prompt = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Headphone_Test")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test1")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test2")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test3"))),
                                      url = "musicassessr-assets/audio/test_headphones.mp3",
                                      button_text = psychTestR::i18n("comfortable_volume"),
                                      show_controls = TRUE
  )
}

#' Microphone Type Page
#'
#' @return
#' @export
#'
#' @examples
microphone_type_page <- function() {
  psychTestR::NAFC_page(label = "microphone_type",
                        prompt = shiny::tags$div(
                          shiny::tags$h2("Microphone Type"),
                          shiny::tags$p("Are you using an internal (in-built) microphone external (plugged-in) ?"),
                          shiny::tags$br(),
                          shiny::tags$table(style = "border: none;",
                                            shiny::tags$tr(
                                              shiny::tags$th("Internal", style = "text-align: center;"),
                                              shiny::tags$th("External", style = "text-align: center;")
                                            ),
                                            shiny::tags$tr(
                                              shiny::tags$td(shiny::tags$img(src = "musicassessr-assets/img/mic_internal.png", height = 127, width = 150), style = "text-align: center;"),
                                              shiny::tags$td(shiny::tags$img(src = "musicassessr-assets/img/mic_external.png", height = 86, width = 100), style = "text-align: center;")
                                            )
                          ), shiny::tags$br()
                        ),
                        choices = c("Internal", "External"))
}


