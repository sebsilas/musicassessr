

#' Make a musicassessr test
#'
#' @param musicassessr_state Production or test.
#' @param elts_before_setup_pages Timeline to go before setup pages.
#' @param elts Timeline to go after setup pages.
#' @param setup_pages Should there be setup pages.
#' @param setup_pages_options Options for setup pages.
#' @param title Title of the test.
#' @param admin_password Password for the test.
#' @param languages Languages for the test.
#' @param additional_dict Additional dictionary for the test.
#' @param musicassessr_opt Musicassessr options.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
make_musicassessr_test <- function(musicassessr_state = 'production',
                                   elts_before_setup_pages = function() { psychTestR::join(psychTestR::code_block(function(state, ...) {})) },
                                   elts,
                                   setup_pages,
                                   setup_pages_options = musicassessr::setup_pages_options,
                                   title, admin_password, languages = "en",
                                   additional_dict = NULL,
                                   musicassessr_opt = musicassessr::musicassessr_opt(), ...) {

  stopifnot(
    is.character(musicassessr_state) & length(musicassessr_state) == 1,
    is.function(elts_before_setup_pages), is.function(elts),
    is.logical(setup_pages), is.function(setup_pages_options),
    is.null(additional_dict) | is.data.frame(additional_dict)
  )

  psychTestR::make_test(
    psychTestR::new_timeline(
    psychTestR::join(

      musicassessr::musicassessr_opt(),

      elts_before_setup_pages(),

      if(setup_pages) setup_pages_options(),

      elts = elts()

    ), dict = musicassessr::dict(additional_dict)),
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      languages = languages,
      additional_scripts = musicassessr::musicassessr_js(musicassessr_state), ...
    )
  )
}


#' Setup page options for a musicassessr test
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
#' @param concise_wording Should the wording used by the concise version for not.
#'
#' @return
#' @export
#'
#' @examples
setup_pages_options <- function(input = c("microphone", "midi_keyboard", "midi_keyboard_and_microphone", "midi_keyboard_or_microphone"),
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
                                concise_wording = FALSE) {

  function() {
    musicassessr::setup_pages(input, headphones, SNR_test, min_SNR, get_user_info, demo, get_instrument_range,
                              absolute_url, select_instrument, get_instrument_range_musical_notation,
                              adjust_range, test_type, microphone_test, allow_repeat_SNR_tests, report_SNR, concise_wording)
  }



}

#' Specify options for a musicassessr test.
#'
#' @param test_username
#' @param test
#' @param store_results_in_db
#' @param local_app_file_dir
#' @param sonic_annotator_local_location
#' @param copy_audio_to_location
#'
#' @return
#' @export
#'
#' @examples
musicassessr_opt <- function(test_username = NA,
                             test = NA,
                             store_results_in_db = FALSE,
                             local_app_file_dir = "/Users/sebsilas/aws-musicassessr-local-file-upload/files/",
                             sonic_annotator_local_location = "/Users/sebsilas/sonic-annotator",
                             copy_audio_to_location = NULL) {
  musicassessr_init(
    test_username,
    test,
    store_results_in_db,
    local_app_file_dir,
    sonic_annotator_local_location,
    copy_audio_to_location
  )
}
