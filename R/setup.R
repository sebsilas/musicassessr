
#' Setup pages for musicassessr test
#'
#' @param input
#' @param headphones
#' @param SNR_test
#' @param min_SNR
#' @param get_user_info
#' @param demo
#' @param get_instrument_range
#' @param absolute_url
#' @param select_instrument
#' @param get_instrument_range_musical_notation
#' @param adjust_range
#' @param test_type
#' @param microphone_test
#' @param allow_repeat_SNR_tests
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
                        get_instrument_range = FALSE,
                        absolute_url,
                        select_instrument = FALSE,
                        get_instrument_range_musical_notation = FALSE,
                        adjust_range = FALSE,
                        test_type = c("voice", "instrument"),
                        microphone_test = TRUE,
                        allow_repeat_SNR_tests = TRUE) {

  stopifnot(is.character(input), is.logical(headphones), is.logical(SNR_test),
            is.numeric(min_SNR), is.logical(get_user_info), is.logical(demo),
            is.logical(get_instrument_range), is.character(absolute_url),
            is.logical(select_instrument), is.logical(get_instrument_range_musical_notation))


  if(demo) {

    psychTestR::join(
      if(select_instrument) select_musical_instrument_page(),

      correct_setup(input, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests),

      fake_range()
    )

  } else {

    psychTestR::join(

      requirements_page(headphones = headphones, input = input),

      if(get_user_info) musicassessr::get_user_info_page(),

      if(headphones) musicassessr::test_headphones_page(),

      if(select_instrument) select_musical_instrument_page(),

      correct_setup(input, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests),

      record_instructions(),

      get_instrument_range_pages(input, get_instrument_range,
                                 show_musical_notation = get_instrument_range_musical_notation,
                                 adjust_range = adjust_range,
                                 test_type = test_type)

    )

  }
}


correct_setup <- function(input, SNR_test, absolute_url, microphone_test = TRUE, allow_repeat_SNR_tests = TRUE) {
  if(!sjmisc::str_contains(input, "midi_keyboard")) {
    microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests)
  } else if(!sjmisc::str_contains(input, "microphone")) {
    midi_setup()
  } else if(input == "midi_keyboard_and_microphone") {
    c(
      microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests),
      midi_setup()
    )

  } else {
    c(
      midi_vs_audio_select_page(),

      psychTestR::conditional(function(state, ...) {
        psychTestR::get_global("response_type", state) == "Microphone"
      }, logic = microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests)),

      psychTestR::conditional(function(state, ...) {
        psychTestR::get_global("response_type", state) == "MIDI"
      }, logic = midi_setup())
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


#' Test headphones page
#' Essentially a wrapper for volume_calibration_page with a default audio file.
#' @return
#' @export
#'
#' @examples
test_headphones_page <- function() {
  psychTestR::volume_calibration_page(prompt = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Headphone_Test")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test1"), shiny::tags$img(src = 'https://adaptiveeartraining.com/magmaGold/play_triangle.png', width = 20, height = 21)),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test2")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test3")),
                                                               shiny::tags$p(psychTestR::i18n("headphone_test4")),
                                                               shiny::tags$p("Use your normal way of changing loudness on your computer to do this")),
                                      url = "https://adaptiveeartraining.com/magmaGold/audio/test_headphones.mp3",
                                      button_text = psychTestR::i18n("comfortable_volume"),
                                      show_controls = TRUE)
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
                        choices = c("Internal", "External", "Not sure"))
}


microphone_setup <- function(SNR_test, absolute_url, microphone_test = TRUE, allow_repeat_SNR_tests = TRUE) {

  if(microphone_test) {
    microphone_pages <- psychTestR::join(
      microphone_type_page(),
      musicassessr::microphone_calibration_page()
    )
  } else {
    microphone_pages <- psychTestR::code_block(function(state, ...){}) # there needs to be the possibility of something resolving
  }

  psychTestR::join(
      microphone_pages,
    if(SNR_test & !allow_repeat_SNR_tests) musicassessr::get_SNR_pages(absolute_url = absolute_url),
    if(SNR_test & allow_repeat_SNR_tests) musicassessr::get_SNR_pages_loop(absolute_url = absolute_url)
  )
}

record_instructions <- function() {
  psychTestR::one_button_page(body = shiny::tags$div(shiny::tags$p(psychTestR::i18n("record_instructions")), shiny::tags$img(src = "musicassessr-assets/img/record.gif")), button_text = psychTestR::i18n("Next"))
}


get_instrument_range <- function(inst) {
  insts_table[insts_table$en == inst, c("low_note", "high_note")]
}

set_instrument_range_code_block <- function(inst = NULL) {
  psychTestR::code_block(function(state, ...) {
    print('setting MIDI pages default')
    if(is.null(inst)) {
      inst <- psychTestR::get_global("inst", state)
    }
    psychTestR::set_global("bottom_range", get_instrument_range(inst)$low_note, state)
    psychTestR::set_global("top_range", get_instrument_range(inst)$high_note, state)
  })
}

midi_setup <- function() {

  c(
    select_midi_device_page(title = psychTestR::i18n("select_midi_device_title"),
                            message = psychTestR::i18n("select_midi_device_message"),
                            button_text = psychTestR::i18n("Next"),
                            error_notification =  psychTestR::i18n("no_midi_device_found")),
    psychTestR::code_block(fun = function(state, ...) {
      psychTestR::set_global("inst", "Piano", state)
    }),
    set_instrument_range_code_block("Piano")
  )
}



