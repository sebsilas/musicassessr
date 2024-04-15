
#' Setup pages for musicassessr test
#'
#' @param input_type The form of musicassessr test.
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
#' @param skip_setup Whether to skip setup. Can be TRUE (skip whole setup), FALSE or "except_microphone" (only setup the microphone but no other steps).
#' @param get_self_chosen_anonymous_id Whether to ask participant to provide an anonymous ID.
#' @param musical_instrument Whether the participant is required to have a musical instrument.
#' @param allow_SNR_failure If TRUE, allow user to continue even if they fail the SNR test.
#' @param requirements_page Show a requirements page?
#' @param playful_volume_meter_setup Should there be some additional functionality to demo the playful volume meter?
#' @param use_musicassessr_db Is musicassessr_db being used?
#' @param show_microphone_type_page Should you ask the user what kind of microphone they are using?
#'
#' @return
#' @export
#'
#' @examples
setup_pages <- function(input_type = c("microphone",
                                       "midi_keyboard",
                                       "midi_keyboard_and_microphone",
                                       "midi_keyboard_or_microphone",
                                       "key_presses"),
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
                        concise_wording = TRUE,
                        skip_setup = FALSE,
                        get_self_chosen_anonymous_id = FALSE,
                        musical_instrument = FALSE,
                        allow_SNR_failure = FALSE,
                        requirements_page = TRUE,
                        playful_volume_meter_setup = FALSE,
                        use_musicassessr_db = FALSE,
                        show_microphone_type_page = TRUE) {


  input_type <- match.arg(input_type)
  test_type <- match.arg(test_type)

  stopifnot(input_type %in% c("microphone",
                              "midi_keyboard",
                              "midi_keyboard_and_microphone",
                              "midi_keyboard_or_microphone",
                              "key_presses"),
            is.scalar.logical(headphones), is.scalar.logical(SNR_test),
            is.numeric(min_SNR), is.scalar.logical(get_user_info), is.scalar.logical(demo),
            is.scalar.logical(get_instrument_range) | is.character(get_instrument_range) & length(get_instrument_range) == 1,
            is.character(absolute_url),
            is.scalar.logical(select_instrument),
            is.scalar.logical(get_instrument_range_musical_notation),
            is.scalar.logical(adjust_range),
            is.character(test_type),
            is.scalar.logical(microphone_test),
            is.scalar.logical(allow_repeat_SNR_tests),
            is.scalar.logical(report_SNR), is.scalar.logical(concise_wording),
            is.scalar.logical(skip_setup) | skip_setup == "except_microphone",
            is.scalar.logical(get_self_chosen_anonymous_id),
            is.scalar.logical(musical_instrument),
            is.scalar.logical(allow_SNR_failure),
            is.scalar.logical(requirements_page),
            is.scalar.logical(playful_volume_meter_setup),
            is.scalar.logical(use_musicassessr_db),
            is.scalar.logical(show_microphone_type_page)
            )


  if(demo) {

    setup <- psychTestR::join(

      if(select_instrument) select_musical_instrument_page(use_musicassessr_db, set_range_based_on_selection = !get_instrument_range),

      correct_setup(input_type, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, musical_instrument = musical_instrument, allow_SNR_failure = allow_SNR_failure, show_microphone_type_page = show_microphone_type_page)

    )

  } else if(skip_setup == "except_microphone") {

    setup <- psychTestR::join(

      if(get_self_chosen_anonymous_id) get_self_chosen_anonymous_id() else pass_p_id_to_js(),

      correct_setup(input_type, SNR_test = FALSE, absolute_url, microphone_test = TRUE, concise_wording, skip_setup = skip_setup, musical_instrument = musical_instrument, allow_SNR_failure = allow_SNR_failure, show_microphone_type_page = show_microphone_type_page)
    )

  } else if(skip_setup || input_type == "key_presses") {

    setup <- psychTestR::join(empty_code_block())

  } else {

    setup <- psychTestR::module("musicassessr_setup",
      psychTestR::join(

      print_code_block("asdasd1"),

      if(get_self_chosen_anonymous_id) get_self_chosen_anonymous_id() else pass_p_id_to_js(),

      print_code_block("asdasd2"),


      if(requirements_page) requirements_page(headphones = headphones, input_type = input_type, musical_instrument = musical_instrument),

      if(get_user_info) get_user_info_page(),

      if(headphones) test_headphones_page(concise_wording),

      if(select_instrument) select_musical_instrument_page(use_musicassessr_db, set_range_based_on_selection = !get_instrument_range),

      correct_setup(input_type, SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, musical_instrument = musical_instrument, allow_SNR_failure = allow_SNR_failure, show_microphone_type_page = show_microphone_type_page),

      record_instructions(playful_volume_meter_setup),

      if(get_instrument_range) get_instrument_range_pages(input_type, show_musical_notation = get_instrument_range_musical_notation, adjust_range = adjust_range, test_type = test_type, concise_wording = concise_wording)
    ))

  }

  # Set the response/input type when it is definitely either microphone or MIDI (i.e., non-user specified):
  psychTestR::join(
    if(input_type %in% c("microphone", "midi_keyboard")) set_response_type(if(input_type == "microphone") "Microphone" else if(input_type == "midi_keyboard") "MIDI" else stop("Input type not recognised.")),
    setup
  )

}


correct_setup <- function(input_type, SNR_test, absolute_url, microphone_test = TRUE, allow_repeat_SNR_tests = TRUE, report_SNR = FALSE,
                          concise_wording = FALSE, skip_setup = FALSE, musical_instrument = FALSE, allow_SNR_failure = FALSE, show_microphone_type_page = TRUE) {


  if(!sjmisc::str_contains(input_type, "midi_keyboard")) {
    microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup, musical_instrument, allow_SNR_failure, show_microphone_type_page)
  } else if(!sjmisc::str_contains(input_type, "microphone")) {
    midi_setup()
  } else if(input_type == "midi_keyboard_and_microphone") {
    psychTestR::join(
      microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup, musical_instrument, allow_SNR_failure, show_microphone_type_page),
      midi_setup()
    )

  } else {

    psychTestR::join(
      midi_vs_audio_select_page(),

      psychTestR::conditional(function(state, ...) {
        psychTestR::get_global("response_type", state) == "Microphone"
      }, logic = microphone_setup(SNR_test, absolute_url, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording, skip_setup, musical_instrument, allow_SNR_failure, show_microphone_type_page)),

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
    wording <- psychTestR::i18n("concise_headphone_wording")
  } else {
    wording <- shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Headphone_Test")),
                    shiny::tags$p(psychTestR::i18n("headphone_test1"), shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/play_triangle.png', width = 30, height = 31, style = "margin: 0 0 5px 0;")),
                    shiny::tags$p(psychTestR::i18n("headphone_test2")),
                    shiny::tags$p(psychTestR::i18n("headphone_test3")),
                    shiny::tags$p(psychTestR::i18n("headphone_test4")),
                    shiny::tags$p(psychTestR::i18n("normal_loudness")))
  }


  psychTestR::volume_calibration_page(prompt = wording,
                                      url = "https://adaptiveeartraining.com/assets/audio/test_headphones.mp3",
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
                          shiny::tags$h2(psychTestR::i18n("Microphone_Type")),
                          shiny::tags$p(psychTestR::i18n("plugged_vs_external_mic")),
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
                        choices = c(psychTestR::i18n("Internal"), psychTestR::i18n("External"), psychTestR::i18n("Not_sure")))
}


microphone_setup <- function(SNR_test, absolute_url = character(), microphone_test = TRUE,
                             allow_repeat_SNR_tests = TRUE, report_SNR = FALSE,
                             concise_wording = FALSE, skip_setup = FALSE, musical_instrument = FALSE, allow_SNR_failure = FALSE, show_microphone_type_page = TRUE) {

  if(microphone_test) {
    microphone_pages <- psychTestR::join(
      if(show_microphone_type_page) if(skip_setup == "except_microphone" || ! skip_setup) microphone_type_page(),
      microphone_calibration_page(concise_wording = concise_wording, musical_instrument = musical_instrument)
    )
  } else {
    microphone_pages <- empty_code_block() # there needs to be the possibility of something resolving
  }

  psychTestR::join(
      microphone_pages,
    if(SNR_test & !allow_repeat_SNR_tests) get_SNR_pages(absolute_url = absolute_url, report_SNR = report_SNR, allow_SNR_failure = allow_SNR_failure),
    if(SNR_test & allow_repeat_SNR_tests) get_SNR_pages_loop(absolute_url = absolute_url, report_SNR = report_SNR, allow_SNR_failure = allow_SNR_failure)
  )
}

record_instructions <- function(playful_volume_meter_setup = FALSE) {

  ui <- shiny::tags$div(
    shiny::tags$p(psychTestR::i18n("record_instructions")),
    shiny::tags$img(src = "musicassessr-assets/img/record.gif"),
    if(playful_volume_meter_setup) shiny::tags$div(shiny::tags$p(psychTestR::i18n("record_instructions_playful")), volume_meter(type = "playful"), shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr")))
    )


    psychTestR::one_button_page(body = ui, button_text = psychTestR::i18n("Next"))
}


get_instrument_range <- function(inst) {
  insts_table[insts_table$en == inst, c("low_note", "high_note")]
}

set_instrument_range_code_block <- function(inst = NULL) {
  psychTestR::code_block(function(state, ...) {
    logging::loginfo('Setting MIDI pages default')
    if(is.null(inst)) {
      inst <- psychTestR::get_global("inst", state)
    }
    psychTestR::set_global("bottom_range", get_instrument_range(inst)$low_note, state)
    psychTestR::set_global("top_range", get_instrument_range(inst)$high_note, state)
  })
}

midi_setup <- function() {

  psychTestR::join(
    select_midi_device_page(title = psychTestR::i18n("select_midi_device_title"),
                            message = psychTestR::i18n("select_midi_device_message"),
                            button_text = psychTestR::i18n("Next"),
                            error_notification =  psychTestR::i18n("no_midi_device_found")),
    psychTestR::code_block(fun = function(state, ...) {
      psychTestR::set_global("inst", "Piano", state)
    }),
    set_instrument_range_code_block("Piano"),
    test_midi_page()
  )
}


