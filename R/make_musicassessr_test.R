
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
#' @param dict A psychTestR dictionary for translations.
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
                                   languages = c("en", "it", "de", "lv", "ch"),
                                   opt = musicassessr_opt(),
                                   final_page = psychTestR::final_page(psychTestR::i18n("thank_you_for_completing")),
                                   welcome_page = psychTestR::one_button_page("Welcome."),
                                   dict = musicassessr::musicassessr_dict, ...) {


  stopifnot(
    is.scalar.character(title),
    is.scalar.character(admin_password),
    is.function(elts),
    is.function(elts_before_setup_pages),
    is.character(languages),
    is.list(opt),
    is(final_page, "page") || is(final_page, "reactive_page") || psychTestR::is.timeline(final_page),
    psychTestR::is.test_element(welcome_page),
    is(dict, "i18n_dict")
  )


  setup_enclosure <- opt$setup_page_options(asynchronous_api_mode = opt$asynchronous_api_mode)


  pre_elts <- psychTestR::new_timeline(
    psychTestR::join(

      # Welcome page: required because you need a page before musicassessr_init to instantiate a p_id.
      welcome_page,

      # Get participant ID
      if(opt$get_p_id) psychTestR::get_p_id(prompt = get_p_id_content(opt$get_pid_prompt), button_text = psychTestR::i18n("Next")),

      # Init musicassessr
      musicassessr_init(
        app_name = opt$app_name,
        experiment_id = opt$experiment_id,
        experiment_condition_id = opt$experiment_condition_id,
        user_id = opt$user_id,
        default_range = opt$default_range,
        username = opt$username,
        asynchronous_api_mode =  opt$asynchronous_api_mode,
        instrument_id = opt$instrument_id,
        inst = opt$instrument,
        get_user_info = opt$get_user_info,
        redirect_on_failure_url = opt$redirect_on_failure_url,
        async_success_msg = opt$async_success_msg,
        use_presigned_url = opt$use_presigned_url),

      # Timeline before setup pages
      elts_before_setup_pages(),

      # Setup pages
      if (opt$setup_pages) setup_enclosure

      # Timeline after setup pages
    ), dict = dict)

  post_elts <-
    psychTestR::new_timeline(
      psychTestR::join(
        # Save results
        psychTestR::elt_save_results_to_disk(complete = TRUE),

        # Add final session information to DB (if asynchronous_api_mode)
        musicassessrdb::elt_add_final_session_info_to_db(opt$asynchronous_api_mode),

        # Final page
        final_page

      ), dict = dict)

  tl <- psychTestR::join(

      pre_elts,

        elts(),

      post_elts

      )

  psychTestR::make_test(tl,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      languages = languages,
      on_session_ended_fun = end_session(asynchronous_api_mode = opt$asynchronous_api_mode),
      get_user_info = opt$get_user_info,
      enable_admin_panel = FALSE,
      additional_scripts = musicassessr::musicassessr_js(app_name = opt$app_name,
                                                         visual_notation = opt$visual_notation,
                                                         midi_file_playback = opt$midi_file_playback,
                                                         record_audio = opt$record_audio,
                                                         asynchronous_api_mode = opt$asynchronous_api_mode,
                                                         midi_input = opt$midi_input),
      display = psychTestR::display_options(
        left_margin = 1L,
        right_margin = 1L,
        css = opt$css,
        content_border = "solid 3px #bfd5d9"
      ), ...)
  )
}



get_p_id_content <- function(prompt = shiny::tags$div(
  shiny::tags$h3("Choose an anonymous ID"),
  shiny::tags$p("Use the first two letters of your first name, plus, in numbers, your month and year of birth."),
  shiny::tags$p("For example, Mike, born in December 1900, would be ", shiny::tags$em("mi121900"),".")
)) {
  return(prompt)
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
  session_id <- musicassessr::get_promise_value(psychTestR::get_global("session_id", state))  # Created earlier

  user_id <- psychTestR::get_global("user_id", state)
  psychTestR_session_id <- psychTestR::get_global("psychTestR_session_id", state)

  user_info <- psychTestR::get_global("user_info", state) %>%
    tidy_get_user_info()

  if(length(session_id) > 1L && "session_id" %in% names(session_id)) {
    session_id <- session_id$session_id
  }

  logging::loginfo("call compute_session_scores_and_end_session_api...")
  logging::loginfo("test_id: %s", test_id)
  logging::loginfo("session_id: %s", session_id)
  logging::loginfo("user_id: %s", user_id)
  logging::loginfo("user_info: %s", user_info)

  end_session_api_called <- psychTestR::get_global("compute_session_scores_and_end_session_api_called", state)

  logging::loginfo("end_session_api_called?: %s", end_session_api_called)


  if(is.null(end_session_api_called)) {
    end_session_api_called <- FALSE
  }


  if(!end_session_api_called && !is.null(session_id)) {
    # We only call this if the API hasn't been called through a "proper" test stoppage

    final_session_result <- promises::future_promise({
      # Test failed early
      musicassessrdb::compute_session_scores_and_end_session_api(test_id,
                                                                 session_id,
                                                                 user_id,
                                                                 psychTestR_session_id,
                                                                 session_complete = "0",
                                                                 user_info = user_info)

    }, seed = NULL) %>%
      promises::then(
        onFulfilled = function(result) {

          logging::loginfo("Promise fulfilled.")
          logging::loginfo("Returning promise result: %s", result)

          if(is.scalar.na.or.null(result)) {
            return(NA)
          } else if(result$status == 200) {
            return(result)
          } else {
            return(NA)
          }

        },
        onRejected = function(err) {

          logging::logerror("Promise failed: %s", err)

        }
      )

    psychTestR::set_global('final_session_result', final_session_result, state)

  } else {

    logging::loginfo("Appending failed session result")
    final_session_result <- promises::future_promise({
      # Test failed early
      musicassessrdb::append_failed_session_api(user_info = user_info)

    }, seed = NULL) %>%
      promises::then(
        onFulfilled = function(result) {

          logging::loginfo("Promise fulfilled.")
          logging::loginfo("Returning promise result: %s", result)

          if(is.scalar.na.or.null(result)) {
            return(NA)
          } else if(result$status == 200) {
            return(result)
          } else {
            return(NA)
          }

        },
        onRejected = function(err) {

          logging::logerror("Promise failed: %s", err)

        }
      )

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
#' @param allow_SNR_failure If TRUE, allow user to continue even if they fail the SNR test.
#' @param requirements_page Show a requirements page?
#' @param playful_volume_meter_setup Should there be some additional functionality to demo the playful volume meter?
#' @param show_microphone_type_page Should you ask the user what kind of microphone they are using?
#' @param with_additional_recording_instructions Should additional recording instructions be presented?
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
                                allow_SNR_failure = FALSE,
                                requirements_page = TRUE,
                                playful_volume_meter_setup = FALSE,
                                show_microphone_type_page = TRUE,
                                with_additional_recording_instructions = FALSE) {

  # Note, we don't use asynchronous_api_mode here. We take this from musicassessr_opt


  input_type <- match.arg(input_type)
  test_type <- match.arg(test_type)

  function(asynchronous_api_mode) {

      musicassessr::setup_pages(input_type, headphones, SNR_test, min_SNR, get_user_info, demo, get_instrument_range,
                                absolute_url, select_instrument, get_instrument_range_musical_notation,
                                adjust_range, test_type, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording,
                                skip_setup, get_self_chosen_anonymous_id, musical_instrument, allow_SNR_failure, requirements_page,
                                playful_volume_meter_setup, asynchronous_api_mode, show_microphone_type_page, with_additional_recording_instructions)

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
#' @param asynchronous_api_mode Should async API mode be on?
#' @param default_range Should there be default range set?
#' @param css A css stylesheet to use (passes to psychTestR).
#' @param username Hardcode a username.
#' @param get_pid_prompt What prompt to you want to use for the get_p_id page?
#' @param instrument What instrument is the test using?
#' @param get_user_info Get user geolocation and device information?
#' @param redirect_on_failure_url If the test fails, where should the participant be redirected?
#' @param async_success_msg What message should be shown after the user logs in via the async system?
#' @param use_presigned_url For audio uploading, should a presigned URL be used?
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
                             get_p_id = FALSE,
                             asynchronous_api_mode = FALSE,
                             default_range = set_default_range('Piano'),
                             css = c("https://musicassessr.com/assets/css/style_songbird.css",
                                     system.file('www/css/musicassessr.css', package = 'musicassessr')),
                             username = NULL,
                             get_pid_prompt = shiny::tags$div(
                               shiny::tags$h3("Choose an anonymous ID"),
                               shiny::tags$p("Use the first two letters of your first name, plus, in numbers, your month and year of birth."),
                               shiny::tags$p("For example, Mike, born in December 1900, would be ", shiny::tags$em("mi121900"),".")
                             ),
                             instrument = "Voice",
                             get_user_info = TRUE,
                             redirect_on_failure_url = "https://www.google.com/",
                             async_success_msg = if(is.scalar.character(username)) paste0(psychTestR::i18n("Hello"), " ", username, "!") else psychTestR::i18n("lets_proceed"),
                             use_presigned_url = TRUE) {

  stopifnot(
    is.scalar.logical(setup_pages),
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
    is.scalar.logical(get_p_id),
    is.scalar.logical(asynchronous_api_mode),
    is.null.or(default_range, function(x)   {
      is.list(x) && length(x) == 3 && setequal(names(x), c('bottom_range', 'top_range', 'clef'))
    }),
    is.null.or(css, is.character),
    is.null.or(username, is.scalar.character),
    is(get_pid_prompt, "shiny.tag") || is.scalar.character(get_p_id_prompt),
    is.scalar.character(instrument),
    is.scalar.logical(get_user_info),
    is.scalar.character(redirect_on_failure_url),
    is.scalar.character(async_success_msg),
    is.scalar.logical(use_presigned_url)
  )

  if(get_user_info) {
    log_warn("get_user_info is TRUE by default in musicassessr and will collect geolocation and browser information from users. Please disable if need be.")
  }

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
    get_p_id = get_p_id,
    asynchronous_api_mode = asynchronous_api_mode,
    default_range = default_range,
    css = css,
    username = username,
    get_pid_prompt = get_pid_prompt,
    instrument = instrument,
    get_user_info = get_user_info,
    redirect_on_failure_url = redirect_on_failure_url,
    async_success_msg = async_success_msg,
    use_presigned_url = use_presigned_url
  )

}



