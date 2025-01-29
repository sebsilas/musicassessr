

#'  Build multiple play_long_tone_record_audio_pages based on a user's range
#'
#' @param no_items
#' @param page_type
#' @param page_text
#' @param example
#' @param page_title
#' @param feedback
#' @param get_answer
#' @param show_progress
#' @param trial_paradigm
#' @param call_and_response_end
#' @param long_tone_length
#' @param singing_trial
#' @param mute_midi_playback
#'
#' @return
#' @export
#'
#' @examples
multi_play_long_tone_record_audio_pages <- function(no_items,
                                                    page_type = "record_audio_page",
                                                    page_text = psychTestR::i18n("long_tone_text"),
                                                    example = FALSE,
                                                    page_title = psychTestR::i18n("long_tone_title"),
                                                    feedback = FALSE,
                                                    get_answer = get_answer_pyin_long_note,
                                                    show_progress = TRUE,
                                                    trial_paradigm = c("simultaneous_recall", "call_and_response"),
                                                    call_and_response_end = c("manual", "auto"),
                                                    long_tone_length = 5,
                                                    singing_trial = FALSE,
                                                    mute_midi_playback = FALSE) {

  trial_paradigm <- match.arg(trial_paradigm)
  call_and_response_end <- match.arg(call_and_response_end)

  items <- purrr::map(1:no_items, function(x) {

    play_long_tone_record_audio_page(long_note_no = x,
                                     page_type = page_type,
                                     page_title = page_title,
                                     page_text = page_text,
                                     example = example,
                                     get_answer = get_answer,
                                     page_label = paste0("long_tone_", x),
                                     total_no_long_notes = no_items,
                                     show_progress = show_progress,
                                     trial_paradigm = trial_paradigm,
                                     call_and_response_end = call_and_response_end,
                                     long_tone_length = long_tone_length,
                                     singing_trial = singing_trial,
                                     mute_midi_playback = mute_midi_playback)
    })

  items <- add_feedback(items, feedback)
}



#' Create a page which produces a given tone
#'
#' @param note
#' @param long_note_no
#' @param long_tone_length
#' @param page_title
#' @param page_text
#' @param play_button_text
#' @param page_type
#' @param example
#' @param get_answer
#' @param page_label
#' @param total_no_long_notes
#' @param show_progress
#' @param trial_paradigm
#' @param call_and_response_end
#' @param singing_trial
#' @param on_complete
#' @param mute_midi_playback
#'
#' @return
#' @export
#'
#' @examples
play_long_tone_record_audio_page <- function(note = NULL,
                                             long_note_no = NULL,
                                             long_tone_length = 5,
                                             page_title = psychTestR::i18n("long_tone_title"),
                                             page_text = psychTestR::i18n("long_tone_text"),
                                             play_button_text = psychTestR::i18n("Play"),
                                             page_type = "record_audio_page",
                                             example = FALSE,
                                             get_answer = get_answer_pyin_long_note,
                                             page_label = "long_tone",
                                             total_no_long_notes = 0,
                                             show_progress = FALSE,
                                             trial_paradigm = c("simultaneous_recall", "call_and_response"),
                                             call_and_response_end = c("manual", "auto"),
                                             singing_trial = TRUE,
                                             on_complete = NULL,
                                             mute_midi_playback = FALSE) {

  # A page type for playing a 5-second tone and recording a user singing with it

  trial_paradigm <- match.arg(trial_paradigm)
  call_and_response_end <- match.arg(call_and_response_end)
  paradigm <- paradigm(paradigm_type = trial_paradigm, page_type = page_type, call_and_response_end = call_and_response_end)

  if(trial_paradigm == "simultaneous_recall") {
  } else if(trial_paradigm == "call_and_response") {
    page_title <- psychTestR::i18n("long_tone_title_call_and_response")
    page_text <-  psychTestR::i18n("long_tone_text_call_and_response")
  } else {
    stop(paste0("Unknown long tone paradigm. Must be 'simultaneous_recall' or 'call_and_response'"))
  }

  save_answer <- !example

  psychTestR::reactive_page(function(state, ...) {


    if(is.null(note)) {

      user_range <- psychTestR::get_global("user_range_sample", state)

      if(is.null(long_note_no)) {
        long_note_no <- psychTestR::get_global("dynamic_long_note_no", state)
        logging::loginfo("long_note_no: %s", long_note_no)
        # A weird psychTestR quirk is that reactive_pages can be fired twice, so we need to manage it
        # But updating outside of this reative_page, e.g., with a code_block
        page_label <- paste0("long_tone_", long_note_no)
      }
      note <- user_range[long_note_no]
    }

    melody_block_paradigm <- "long_note"

    trial_time_started <- Sys.time()
    psychTestR::set_global("trial_time_started", trial_time_started, state)
    psychTestR::set_global("singing_trial", singing_trial, state)
    psychTestR::set_global('display_modality', "auditory", state)
    psychTestR::set_global('phase', NA, state)
    psychTestR::set_global('rhythmic', NA, state)
    psychTestR::set_global('melody_block_paradigm', melody_block_paradigm, state)
    psychTestR::set_global('page_label', page_label, state)
    psychTestR::set_global('item_id', NA, state)

    asynchronous_api_mode <- psychTestR::get_global("asynchronous_api_mode", state)



    db_vars <- if(asynchronous_api_mode) {

      module <- psychTestR::get_local(".module", state)

      list(
        stimuli = note,
        stimuli_durations = long_tone_length,
        trial_time_started = trial_time_started,
        instrument = if(singing_trial) "Voice" else psychTestR::get_global("inst", state),
        attempt = 1L,
        item_id = NA,
        display_modality = "auditory",
        phase = "test",
        rhythmic = NA,
        session_id = get_promise_value(psychTestR::get_global("session_id", state)),
        test_id = 1L,
        user_id = psychTestR::get_global("user_id", state),
        review_items_id = NA,
        new_items_id = NA,
        feedback = FALSE,
        feedback_type = NA,
        trial_paradigm = paste0("long_note_", trial_paradigm),
        melody_block_paradigm = melody_block_paradigm,
        additional = NA,
        file_type = NA,
        noise_filename = NA,
        module = module
      )
    } else NULL

    present_stimuli(stimuli = note,
                    stimuli_type = "midi_notes",
                    display_modality = "auditory",
                    page_title = page_title,
                    page_text = page_text,
                    page_type = page_type,
                    page_label = page_label,
                    play_button_text = play_button_text,
                    note_length = long_tone_length,
                    sound = "tone",
                    db_vars = db_vars,
                    save_answer = save_answer,
                    get_answer = get_answer,
                    melody_no = long_note_no,
                    total_no_melodies = total_no_long_notes,
                    show_progress = show_progress,
                    on_complete = on_complete,
                    mute_midi_playback = mute_midi_playback,
                    trigger_start_of_stimulus_fun = paradigm$trigger_start_of_stimulus_fun,
                    trigger_end_of_stimulus_fun = paradigm$trigger_end_of_stimulus_fun,
                    asynchronous_api_mode = asynchronous_api_mode)

  })

}
