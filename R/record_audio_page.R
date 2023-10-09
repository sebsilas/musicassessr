
#' Page to record audio in psychTestR
#'
#' @param body
#' @param label
#' @param stimuli
#' @param stimuli_reactive
#' @param page_text
#' @param page_title
#' @param interactive
#' @param get_answer
#' @param answer_meta_data
#' @param record_button_text
#' @param stop_button_text
#' @param record_duration
#' @param on_complete
#' @param save_answer
#' @param page_text_first
#' @param happy_with_response
#' @param attempts_left
#' @param max_goes_forced
#' @param max_goes
#' @param show_progress
#' @param melody_no
#' @param total_no_melodies
#' @param volume_meter
#' @param volume_meter_type
#' @param show_sheet_music_after_record
#' @param show_record_button
#' @param reactive_melody_no
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
record_audio_page <- function(body = "",
                             label = "record_audio_page",
                             stimuli = NULL,
                             stimuli_reactive = FALSE,
                             page_text = " ",
                             page_title = " ",
                             interactive = FALSE,
                             get_answer = get_answer_save_audio_file,
                             answer_meta_data = tibble::tibble(),
                             record_button_text = psychTestR::i18n("Record"),
                             stop_button_text = psychTestR::i18n("Stop"),
                             record_duration = NULL,
                             on_complete = NULL,
                             save_answer = TRUE,
                             page_text_first = TRUE,
                             happy_with_response =  FALSE,
                             attempts_left = NULL,
                             max_goes_forced = FALSE,
                             max_goes = 1,
                             show_progress = FALSE,
                             melody_no = 0,
                             total_no_melodies = 0,
                             volume_meter = FALSE,
                             volume_meter_type = 'default',
                             show_sheet_music_after_record = FALSE,
                             show_record_button = TRUE,
                             reactive_melody_no = FALSE, ...) {

  record_midi_or_audio_ui(body,
                          label,
                          stimuli,
                          stimuli_reactive,
                          page_text,
                          page_title,
                          page_type = "record_audio_page",
                          interactive,
                          get_answer,
                          answer_meta_data,
                          record_button_text,
                          stop_button_text,
                          record_duration,
                          on_complete,
                          save_answer,
                          page_text_first,
                          happy_with_response,
                          attempts_left,
                          max_goes_forced,
                          autoInstantiate =  FALSE,
                          midi_device = " ",
                          max_goes,
                          show_progress,
                          melody_no,
                          total_no_melodies,
                          volume_meter,
                          volume_meter_type,
                          show_sheet_music_after_record,
                          show_record_button,
                          reactive_melody_no)

}




