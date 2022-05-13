# functions for testing purposes (i.e to speed up development)

#' Set a dummy range (to avoid having to manually set in the test)
#'
#' @param bottom_range
#' @param top_range
#'
#' @return
#' @export
#'
#' @examples
fake_range <- function(bottom_range = 48, top_range = 72) {
  psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("bottom_range", bottom_range, state)
    psychTestR::set_global("top_range", top_range, state)
    psychTestR::set_global("span", top_range-bottom_range, state)
  })
}


#' A quick app to test recording functionality
#'
#' @return
#' @export
#'
#' @examples
test_recording_app <- function() {
  psychTestR::make_test(
    psychTestR::new_timeline(
      psychTestR::join(
        musicassessr::musicassessr_init(),
        microphone_calibration_page(),

        musicassessr::record_audio_block(no_pages = 10),

        psychTestR::final_page("Finished!")
      ), dict = musicassessr::dict(NULL)),
    opt = psychTestR::test_options(
      title = "Record audio block test",
      admin_password = "demo",
      languages = c("en"),
      additional_scripts = musicassessr::musicassessr_js('test')
    )
  )
}
