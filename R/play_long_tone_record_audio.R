#' Build multiple play_long_tone_record_audio_pages based on a user's range
#'
#' @param no_items
#' @param page_type
#' @param example
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
multi_play_long_tone_record_audio_pages <- function(no_items, page_type = "record_audio_page", example = FALSE, feedback = FALSE, get_answer = get_answer_pyin_long_note) {
  items <- unlist(lapply(1:no_items, function(x) play_long_tone_record_audio_page(long_note_no = x, page_type = page_type, example = example, get_answer = get_answer)))
  items <- add_feedback(items, feedback)
}

#' Create a page which produces a given tone
#'
#' @param note
#' @param long_note_no
#' @param note_length
#' @param page_title
#' @param page_text
#' @param play_button_text
#' @param page_type
#' @param show_aws_controls
#' @param show_record_button
#' @param auto_next_page
#' @param example
#' @param get_answer
#'
#' @return
#' @export
#'
#' @examples
play_long_tone_record_audio_page <- function(note = NULL,
                                             long_note_no = "x",
                                             note_length = 5,
                                             page_title = psychTestR::i18n("long_tone_heading"),
                                             page_text = "Sing along with the tone for 5 seconds.",
                                             play_button_text = "Play",
                                             page_type = "record_audio_page",
                                             show_aws_controls = FALSE,
                                             show_record_button = FALSE,
                                             auto_next_page = TRUE,
                                             example = FALSE,
                                             get_answer = get_answer_pyin_long_note) {

  # a page type for playing a 5-second tone and recording a user singing with it

  if(example) {
    save_answer <- FALSE
  } else {
    save_answer <- TRUE
  }

  psychTestR::reactive_page(function(state, ...) {
    print('in reactive long tone page')
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
                    page_label = "long_tone_",
                    play_button_text = play_button_text,
                    note_length = note_length,
                    sound = "tone",
                    show_aws_controls = show_aws_controls,
                    show_record_button = show_record_button,
                    auto_next_page = auto_next_page,
                    save_answer = save_answer,
                    get_answer = get_answer)
  })

}
