# functions for testing purposes (i.e to speed up development)

#' Set a dummy range (to avoid having to manually set in the test)
#'
#' @param bottom_range
#' @param top_range
#' @param clef
#'
#' @return
#' @export
#'
#' @examples
set_instrument_range <- function(bottom_range = 48, top_range = 72, clef = "auto") {
  psychTestR::code_block(function(state, ...) {


    logging::loginfo("Setting range: %s %s", bottom_range, top_range)
    psychTestR::set_global("bottom_range", bottom_range, state)
    psychTestR::set_global("top_range", top_range, state)
    psychTestR::set_global("span", top_range-bottom_range, state)
    psychTestR::set_global("clef", clef, state)

    # Fake an instrument, if need be
    if( is.null(psychTestR::get_global("inst", state)) && is.null(psychTestR::get_global("instrument_id", state)) ) { # Then one hasn't been specified manually via an instrument ID
      logging::logwarn("Faking instrument...")
      psychTestR::set_global("inst", "Piano", state)
      psychTestR::set_global("transpose_visual_notation", 0L, state)
    }


  })
}



#' A quick app to test recording functionality
#'
#' @return
#' @export
#'
#' @examples
test_recording_app <- function(copy_audio_to = NULL) {
  psychTestR::make_test(
    psychTestR::new_timeline(
      psychTestR::join(
        musicassessr::musicassessr_init(),
        microphone_calibration_page(),

        musicassessr::record_audio_block(no_pages = 10, copy_audio_to = copy_audio_to),

        psychTestR::final_page("Finished!")
      ), dict = musicassessr_dict),
    opt = psychTestR::test_options(
      title = "Record audio block test",
      admin_password = "demo",
      languages = c("en"),
      additional_scripts = musicassessr::musicassessr_js('test')
    )
  )
}

log_scores <- function(onset, dur, freq, note, stimuli, stimuli_durations) {

  tibble::tibble(log_onset = log(onset),
                 log_dur = log(dur),
                 freq_2 = freq * 2,
                 stimuli_durations = log(stimuli_durations))

}

check_no_notes_above_c4 <- function(onset, dur, freq, note, stimuli, stimuli_durations) {

  # C4 is MIDI note number 60
  sum(note > 60)

}
