
#' Setup pages for musicassessr test
#'
#' @param input The form of musicassessr test.
#' @param headphones Deploy a headphone setup page.
#' @param SNR_test Deploy an SNR test.
#' @param min_SNR The minimum accepted SNR value.
#' @param get_user_info Get user info from their browser.
#' @param demo Deploy demo test.
#' @param get_instrument_range Get the user's pitch/instrument range at test time.
#' @param absolute_url If in production, what is the absolute URL.
#' @param select_instrument Deploy instrument selection page.
#' @param get_instrument_range_musical_notation When displaying instrument range, provide visual music notation.
#' @param adjust_range Adjust range.
#' @param test_type Voice vs. instrument test.
#' @param microphone_test Deploy microphone test page.
#' @param allow_repeat_SNR_tests Allow repeated SNR tests, if FALSE, then participant only gets one go and the test will fail if the SNR test fails.
#' @param report_SNR Should the SNR be reported to the user.
#' @param concise_wording Whether the wording should be concise or not.
#' @param skip_setup Whether to skip setup.
#' @param get_self_chosen_anonymous_id Whether to ask participant to provide an anonymous ID.
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
                        absolute_url = character(),
                        select_instrument = FALSE,
                        get_instrument_range_musical_notation = FALSE,
                        adjust_range = FALSE,
                        test_type = c("voice", "instrument"),
                        microphone_test = TRUE,
                        allow_repeat_SNR_tests = TRUE,
                        report_SNR = FALSE,
                        concise_wording = FALSE,
                        skip_setup = FALSE,
                        get_self_chosen_anonymous_id = FALSE) {

  stopifnot(is.character(input), is.logical(headphones), is.logical(SNR_test),
            is.numeric(min_SNR), is.logical(get_user_info), is.logical(demo),
            is.logical(get_instrument_range) | is.character(get_instrument_range) & length(get_instrument_range) == 1,
            is.character(absolute_url),
            is.logical(select_instrument), is.logical(get_instrument_range_musical_notation),
            is.logical(adjust_range),
            is.character(test_type),
            is.logical(microphone_test),
            is.logical(allow_repeat_SNR_tests),
            is.logical(report_SNR), is.logical(concise_wording),
            is.logical(skip_setup),
            is.logical(get_self_chosen_anonymous_id))

  if(length(input) > 1) {
    input <- input[1]
  }

  if(length(test_type) > 1) {
    test_type <- test_type[1]
  }


  if(demo) {

    psychTestR::join(
      if(select_instrument) select_musical_instrument_page(),

      correct_setup(input, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording),

      fake_range()
    )
  } else if(skip_setup) {
    psychTestR::join(
      fake_range(),

      # fake instrument:
      psychTestR::code_block(function(state, ...) {
        psychTestR::set_global("inst", "Piano", state)
        psychTestR::set_global("transpose_first_melody_note", 0, state)
        psychTestR::set_global("clef", "auto", state)
      }),

      correct_setup(input, SNR_test = FALSE, absolute_url, microphone_test, concise_wording, skip_setup = skip_setup)
    )
  } else {

    psychTestR::module("musicassessr_setup",
      psychTestR::join(

      if(get_self_chosen_anonymous_id) get_self_chosen_anonymous_id(),

      requirements_page(headphones = headphones, input = input),

      if(get_user_info) get_user_info_page(),

      if(headphones) test_headphones_page(concise_wording),

      if(select_instrument) select_musical_instrument_page(),

      correct_setup(input, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording),

      record_instructions(),

      get_instrument_range_pages(input, get_instrument_range,
                                 show_musical_notation = get_instrument_range_musical_notation,
                                 adjust_range = adjust_range,
                                 test_type = test_type,
                                 concise_wording = concise_wording)
    ))

  }
}


correct_setup <- function(input, SNR_test, absolute_url, microphone_test = TRUE, allow_repeat_SNR_tests = TRUE, report_SNR = FALSE,
                          concise_wording = FALSE, skip_setup = FALSE) {

  if(!sjmisc::str_contains(input, "midi_keyboard")) {
    microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup)
  } else if(!sjmisc::str_contains(input, "microphone")) {
    midi_setup()
  } else if(input == "midi_keyboard_and_microphone") {
    psychTestR::join(
      microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup),
      midi_setup()
    )

  } else {

    psychTestR::join(
      midi_vs_audio_select_page(),

      psychTestR::conditional(function(state, ...) {
        psychTestR::get_global("response_type", state) == "Microphone"
      }, logic = microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup)),

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
#'
#' @param concise_wording
#'
#' @return
#' @export
#'
#' @examples
test_headphones_page <- function(concise_wording = FALSE) {

  if(concise_wording) {
    wording <- "Please make sure your headphones are working and adjust the volume to a good level."
  } else {
    wording <- shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Headphone_Test")),
                    shiny::tags$p(psychTestR::i18n("headphone_test1"), shiny::tags$img(src = 'https://adaptiveeartraining.com/magmaGold/play_triangle.png', width = 30, height = 31, style = "margin: 0 0 5px 0;")),
                    shiny::tags$p(psychTestR::i18n("headphone_test2")),
                    shiny::tags$p(psychTestR::i18n("headphone_test3")),
                    shiny::tags$p(psychTestR::i18n("headphone_test4")),
                    shiny::tags$p("Use your normal way of changing loudness on your computer to do this"))
  }
  psychTestR::volume_calibration_page(prompt = wording,
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
                          shiny::tags$p("Are you using an internal (in-built) or external (plugged-in) microphone?"),
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


microphone_setup <- function(SNR_test, absolute_url = character(), microphone_test = TRUE, allow_repeat_SNR_tests = TRUE, report_SNR = FALSE, concise_wording = FALSE, skip_setup = FALSE) {

  if(microphone_test) {
    microphone_pages <- psychTestR::join(
      if(!skip_setup) microphone_type_page(),
      microphone_calibration_page(concise_wording = concise_wording)
    )
  } else {
    microphone_pages <- psychTestR::code_block(function(state, ...){}) # there needs to be the possibility of something resolving
  }

  psychTestR::join(
      microphone_pages,
    if(SNR_test & !allow_repeat_SNR_tests) get_SNR_pages(absolute_url = absolute_url, report_SNR = report_SNR),
    if(SNR_test & allow_repeat_SNR_tests) get_SNR_pages_loop(absolute_url = absolute_url, report_SNR = report_SNR)
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



