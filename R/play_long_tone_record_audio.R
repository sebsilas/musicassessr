

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
#' @param paradigm
#' @param long_tone_length
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
                                                    paradigm = c("sing_along", "call_and_response"),
                                                    long_tone_length = 5) {

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
                                     paradigm = paradigm,
                                     long_tone_length = long_tone_length)
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
#' @param auto_next_page
#' @param example
#' @param get_answer
#' @param page_label
#' @param total_no_long_notes
#' @param show_progress
#' @param paradigm
#'
#' @return
#' @export
#'
#' @examples
play_long_tone_record_audio_page <- function(note = NULL,
                                             long_note_no = 0,
                                             long_tone_length = 5,
                                             page_title = psychTestR::i18n("long_tone_title"),
                                             page_text = psychTestR::i18n("long_tone_text"),
                                             play_button_text = psychTestR::i18n("Play"),
                                             page_type = "record_audio_page",
                                             auto_next_page = TRUE,
                                             example = FALSE,
                                             get_answer = get_answer_pyin_long_note,
                                             page_label = "long_tone",
                                             total_no_long_notes = 0,
                                             show_progress = FALSE,
                                             paradigm = c("sing_along", "call_and_response")) {

  # a page type for playing a 5-second tone and recording a user singing with it

  if(match.arg(paradigm) == "sing_along") {
    record_immediately <-  TRUE
  } else if(match.arg(paradigm) == "call_and_response") {
    record_immediately <-  FALSE
    page_title <- psychTestR::i18n("long_tone_title_call_and_response")
    page_text <-  psychTestR::i18n("long_tone_text_call_and_response")
  } else {
    stop("Unknown long tone paradigm")
  }


  save_answer <- if_example_save_answer(example)

  psychTestR::reactive_page(function(state, ...) {

    if(is.null(note)) {
      user_range <- psychTestR::get_global("user_range_sample", state)
      note <- user_range[long_note_no]
    }

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
                    auto_next_page = auto_next_page,
                    save_answer = save_answer,
                    get_answer = get_answer,
                    record_immediately = record_immediately,
                    melody_no = long_note_no,
                    total_no_melodies = total_no_long_notes,
                    show_progress = show_progress)
  })

}
