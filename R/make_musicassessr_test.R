
#' Make a musicassessr test
#
#' @param title Title of the test.
#' @param admin_password Password for the test.
#' @param elts Timeline to go after setup pages.
#' @param elts_before_setup_pages Timeline to go before setup pages.
#' @param languages Languages for the test.
#' @param opt Musicassessr options.
#' @param final_page The UI of the final page.
#' @param welcome_page Required because you need a page before musicassessr_init to instantiate a p_id.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
make_musicassessr_test <- function(title,
                                   admin_password,
                                   elts,
                                   elts_before_setup_pages = function() { empty_code_block() },
                                   languages = "en",
                                   opt = musicassessr_opt(),
                                   final_page = wrap_musicassessr_timeline(psychTestR::final_page(psychTestR::i18n("thank_you_for_completing"))),
                                   welcome_page = psychTestR::one_button_page("Welcome."), ...) {


  stopifnot(
    is.scalar.character(title),
    is.scalar.character(admin_password),
    is.function(elts),
    is.function(elts_before_setup_pages),
    is.scalar.character(languages),
    is.list(opt),
    is(final_page, "page") || is(final_page, "reactive_page") || psychTestR::is.timeline(final_page),
    psychTestR::is.test_element(welcome_page)
  )

  psychTestR::make_test(
    psychTestR::join(

      if (opt$fake_range) set_instrument_range(),

      welcome_page,

      # Get participant ID
      if(opt$get_p_id) psychTestR::get_p_id(),

      # Init musicassessr
      musicassessr_init(
        app_name = opt$app_name,
        experiment_id = opt$experiment_id,
        experiment_condition_id = opt$experiment_condition_id,
        user_id = opt$user_id
        ),

      # Timeline before setup pages
      elts_before_setup_pages(),


      # Setup pages
      if (opt$setup_pages) wrap_musicassessr_timeline( opt$setup_page_options() ),

      # Timeline after setup pages

      elts(),

      # Save results
      psychTestR::elt_save_results_to_disk(complete = TRUE),

      # Final page
      final_page

    ),
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      languages = languages,
      on_session_ended_fun = end_session(asynchronous_api_mode = opt$asynchronous_api_mode),
      additional_scripts = musicassessr::musicassessr_js(app_name = opt$app_name,
                                                         visual_notation = opt$visual_notation,
                                                         midi_file_playback = opt$midi_file_playback,
                                                         record_audio = opt$record_audio,
                                                         asynchronous_api_mode = opt$asynchronous_api_mode,
                                                         midi_input = opt$midi_input),
      display = psychTestR::display_options(
        left_margin = 1L,
        right_margin = 1L,
        css = system.file('www/css/musicassessr.css', package = "musicassessr")
      ), ...)
  )
}


#' End session
#'
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
end_session <- function(asynchronous_api_mode) {

  if (asynchronous_api_mode) {
    return(end_session_api)
  } else {
    return(NULL)
  }
}

end_session_api <- function(state, session) {

  logging::loginfo("End session early...")


  # Get session info
  test_id <- psychTestR::get_global("test_id", state)
  session_id <- psychTestR::get_global("session_id", state) # Created earlier
  user_id <- psychTestR::get_global("user_id", state)
  psychTestR_session_id <- psychTestR::get_global("psychTestR_session_id", state)


  logging::loginfo("call compute_session_scores_and_end_session_api...")
  logging::loginfo("test_id: %s", test_id)
  logging::loginfo("session_id: %s", musicassessr::get_promise_value(session_id))
  logging::loginfo("user_id: %s", user_id)

  end_session_api_called <- psychTestR::get_global("compute_session_scores_and_end_session_api_called", state)
  if(is.null(end_session_api_called)) {
    end_session_api_called <- FALSE
  }

  if(!end_session_api_called) {
    # We only call this if the API hasn't been called through a "proper" test stoppage

    final_session_result <- future::future({

      musicassessrdb::compute_session_scores_and_end_session_api(test_id,
                                                                 musicassessr::get_promise_value(session_id),
                                                                 user_id,
                                                                 psychTestR_session_id,
                                                                 session_complete = 0L) # Test failed early
    }, seed = NULL) %...>% (function(result) {
      logging::loginfo("Returning promise result: %s", result)
      if(result$status == 200) {
        return(result)
      } else {
        return(NA)
      }
    })

    psychTestR::set_global('final_session_result', final_session_result, state)

  }

}


#' Setup page options for a musicassessr test
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
#' @param concise_wording Should the wording used by the concise version for not.
#' @param skip_setup Whether to skip setup. Can be TRUE (skip whole setup), FALSE or "except_microphone" (only setup the microphone but no other steps).
#' @param get_self_chosen_anonymous_id Whether to ask participant to provide an anonymous ID.
#' @param musical_instrument Whether the participant is required to have a musical instrument.
#' @param fake_range Should a range be faked?
#'
#' @return
#' @export
#'
#' @examples
setup_pages_options <- function(input_type = c("microphone", "midi_keyboard", "midi_keyboard_and_microphone", "midi_keyboard_or_microphone"),
                                headphones = TRUE,
                                SNR_test = TRUE,
                                min_SNR = 14,
                                get_user_info = TRUE,
                                demo = FALSE,
                                get_instrument_range = FALSE,
                                absolute_url = "https://www.adaptiveeartraining.com",
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
                                fake_range = FALSE) {


  input_type <- match.arg(input_type)
  test_type <- match.arg(test_type)

  function() {
    musicassessr::setup_pages(input_type, headphones, SNR_test, min_SNR, get_user_info, demo, get_instrument_range,
                              absolute_url, select_instrument, get_instrument_range_musical_notation,
                              adjust_range, test_type, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording,
                              skip_setup, get_self_chosen_anonymous_id, musical_instrument, fake_range)
  }



}

#' Specify options for a musicassessr test.
#'
#' @param setup_pages Should there be setup pages?
#' @param setup_options Setup page options.
#' @param app_name App name for audio recording apps.
#' @param midi_file_playback Will MIDI files need to be played back?
#' @param visual_notation Will there be visual notation?
#' @param record_audio Is audio recording required?
#' @param midi_input Is MIDI input required?
#' @param experiment_id An experiment ID.
#' @param experiment_condition_id An experiment condition ID.
#' @param user_id A user ID.
#' @param instrument_id An instrument ID.
#' @param get_p_id Should a participant ID get collected at the beginning of the test?
#'
#' @return
#' @export
#'
#' @examples
musicassessr_opt <- function(setup_pages = TRUE,
                             setup_options = setup_pages_options(),
                             app_name = "",
                             midi_file_playback = FALSE,
                             visual_notation = FALSE,
                             record_audio = TRUE,
                             midi_input = FALSE,
                             experiment_id = NULL,
                             experiment_condition_id = NULL,
                             user_id = NULL,
                             instrument_id = NULL,
                             get_p_id = FALSE) {

  stopifnot(
    is.scalar(setup_pages),
    is.function(setup_options),
    is.scalar.character(app_name),
    is.scalar.logical(midi_file_playback),
    is.scalar.logical(visual_notation),
    is.scalar.logical(record_audio),
    is.scalar.logical(midi_input),
    is.null.or(experiment_id, function(x) is.scalar.character(x) || is.integer(x) ),
    is.null.or(experiment_condition_id, function(x) is.scalar.character(x) || is.integer(x) ),
    is.null.or(user_id, function(x) is.scalar.character(x) || is.integer(x) ),
    is.null.or(instrument_id, function(x) is.scalar.character(x) || is.integer(x) ),
    is.scalar.logical(get_p_id)
  )

  list(
    setup_pages = setup_pages,
    setup_page_options = setup_options,
    app_name = app_name,
    midi_file_playback = midi_file_playback,
    visual_notation = visual_notation,
    record_audio = record_audio,
    midi_input = midi_input,
    experiment_id = experiment_id,
    experiment_condition_id = experiment_condition_id,
    user_id = user_id,
    instrument_id = instrument_id,
    get_p_id = get_p_id
  )

}



