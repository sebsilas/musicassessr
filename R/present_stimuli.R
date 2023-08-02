
#' Present stimuli
#'
#' @param stimuli
#' @param stimuli_type
#' @param display_modality
#' @param page_type
#' @param page_text
#' @param page_title
#' @param slide_length
#' @param answer_meta_data
#' @param get_answer
#' @param save_answer
#' @param stimuli_reactive
#' @param midi_device
#' @param page_label
#' @param button_text
#' @param play_button_text
#' @param note_length
#' @param sound
#' @param asChord
#' @param ascending
#' @param start_note
#' @param end_note
#' @param durations
#' @param auto_next_page
#' @param choices
#' @param user_rating
#' @param page_text_first
#' @param happy_with_response
#' @param attempts_left
#' @param visual_music_notation_id
#' @param play_button_id
#' @param button_area_id
#' @param hideOnPlay
#' @param record_immediately
#' @param max_goes_forced
#' @param transpose_first_melody_note
#' @param clef
#' @param volume
#' @param audio_playback_as_single_play_button
#' @param max_goes
#' @param melody_no
#' @param total_no_melodies
#' @param show_progress
#' @param start_hidden
#' @param sound_only_first_melody_note
#' @param show_sheet_music
#' @param sheet_music_id
#' @param octave
#' @param volume_meter
#' @param volume_meter_type
#' @param show_sheet_music_after_record
#' @param interactive
#' @param show_record_button
#' @param trigger_start_of_stimuli_fun A string of an anonymous Javascript function (with body) to trigger when the stimulus begins.
#' @param trigger_end_of_stimuli_fun A string of an anonymous Javascript function (with body) to trigger when the stimulus is completed.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
present_stimuli <- function(stimuli,
                            stimuli_type,
                            display_modality,
                            page_type = character(),
                            page_text = " ", page_title = " ",
                            slide_length = numeric(),
                            answer_meta_data = character(),
                            get_answer = function(input, ...) { stop("Specify a proper get_answer function if you're using present_stimuli with page_type")},
                            save_answer = TRUE,
                            stimuli_reactive = FALSE,
                            midi_device = " ",
                            page_label = "x",
                            button_text = psychTestR::i18n("Next"),
                            play_button_text = psychTestR::i18n("Play"),
                            note_length = 0.5, sound = "piano", asChord = FALSE, ascending = TRUE,
                            start_note = 0L, end_note = "end", durations = numeric(),
                            auto_next_page = FALSE, choices = character(), user_rating = FALSE,
                            page_text_first = TRUE, happy_with_response = FALSE,
                            attempts_left = integer(), visual_music_notation_id = "sheet_music",
                            play_button_id = "playButton", button_area_id = "button_area",
                            hideOnPlay = FALSE, record_immediately = FALSE, max_goes_forced = FALSE,
                            transpose_first_melody_note = 0, clef = "auto",
                            volume = 1, audio_playback_as_single_play_button = FALSE,
                            max_goes = 1, melody_no = 0, total_no_melodies = 0,
                            show_progress = FALSE, start_hidden = FALSE,
                            sound_only_first_melody_note = FALSE,
                            show_sheet_music = FALSE,
                            sheet_music_id = 'sheet_music',
                            give_first_melody_note = FALSE,
                            slider_value = 5,
                            slider_min = 1,
                            slider_max = 10,
                            octave = 4,
                            volume_meter = FALSE,
                            volume_meter_type = 'default',
                            show_sheet_music_after_record = FALSE,
                            interactive = FALSE,
                            show_record_button = FALSE,
                            trigger_start_of_stimuli_fun = wrap_js_fun_body("console.log('Stimulus started!');"),
                            trigger_end_of_stimuli_fun = wrap_js_fun_body("console.log('Stimulus finished!');"), ...) {

  stopifnot(is.vector(stimuli), is.character(stimuli_type), is.character(display_modality), is.character(page_type),
            is.character(page_text) | class(page_text) == "shiny.tag", is.character(page_title),  is.numeric(slide_length),
            is.character(answer_meta_data) | is.data.frame(answer_meta_data),
            is.function(get_answer), is.scalar.logical(save_answer),
            is.scalar.logical(stimuli_reactive), is.character(midi_device),
            is.character(page_label),
            is.character(button_text), is.character(play_button_text),
            is.numeric(note_length), is.character(sound), is.scalar.logical(asChord), is.scalar.logical(ascending),
            is.integer(start_note), is.integer(end_note) | end_note == "end",
            is.vector(durations) & is.numeric(durations) | is.na(durations),
            is.scalar.logical(auto_next_page), is.character(choices) & is.vector(choices), is.scalar.logical(user_rating),
            is.scalar.logical(page_text_first), is.scalar.logical(happy_with_response),
            is.numeric(attempts_left), is.character(visual_music_notation_id),
            is.character(play_button_id), is.character(button_area_id),
            is.scalar.logical(hideOnPlay), is.scalar.logical(record_immediately), is.scalar.logical(max_goes_forced),
            is.numeric(transpose_first_melody_note),
            is.character(clef) & length(clef) == 1,
            is.numeric(volume) & length(volume) == 1,
            is.scalar.logical(audio_playback_as_single_play_button),
            is.numeric(max_goes),
            is.numeric(melody_no) & length(melody_no) == 1,
            is.numeric(total_no_melodies) & length(total_no_melodies) == 1,
            is.scalar.logical(show_progress),
            is.scalar.logical(start_hidden),
            is.scalar.logical(sound_only_first_melody_note) | is.numeric(sound_only_first_melody_note) & length(sound_only_first_melody_note) == 1,
            is.scalar.logical(show_sheet_music),
            is.character(sheet_music_id) & length(sheet_music_id) == 1,
            is.scalar.logical(give_first_melody_note),
            octave %in% 0:9,
            is.scalar.logical(volume_meter),
            assertthat::is.string(volume_meter_type),
            is.scalar.logical(show_sheet_music_after_record),
            is.scalar.logical(interactive),
            is.scalar.logical(show_record_button),
            is.null(trigger_start_of_stimuli_fun) || is.na(trigger_start_of_stimuli_fun) || is.scalar.character(trigger_start_of_stimuli_fun),
            is.null(trigger_end_of_stimuli_fun) || is.na(trigger_end_of_stimuli_fun) || is.scalar.character(trigger_end_of_stimuli_fun)
            )

  # Generic stimuli types

  if (stimuli_type %in% c("digits", "letters", "words")) {
    return_stimuli <- present_stimuli_characters(stimuli = stimuli, display_modality = display_modality,
                                                 page_type = page_type,
                                                 slide_length = slide_length,
                                                 rate = rate, page_title = page_title, ...)
  } else if (stimuli_type == "images") {
    return_stimuli <- present_stimuli_images(stimuli = stimuli, slide_length = slide_length, ...)
  } else if (stimuli_type == "video") {
    return_stimuli <- present_stimuli_video(video_url = stimuli, ...)
  } else if (stimuli_type == "audio") {
    return_stimuli <- present_stimuli_audio(audio_url = stimuli, hideOnPlay = hideOnPlay, volume = volume, audio_playback_as_single_play_button = audio_playback_as_single_play_button, auto_next_page = auto_next_page, ...)
  } else if (stimuli_type == "audio_WJD") {
    return_stimuli <- present_stimuli_audio_WJD(pattern = stimuli, answer_meta_data = answer_meta_data, ...)
    # Musical stimuli types
  } else if (stimuli_type == "midi_notes") {
    return_stimuli <- present_stimuli_midi_notes(stimuli = stimuli, display_modality = display_modality,
                                                 page_type = page_type,
                                                 play_button_text = play_button_text, note_length = note_length,
                                                 sound = sound, asChord = asChord, ascending = ascending, durations = durations,
                                                 auto_next_page = auto_next_page,
                                                 visual_music_notation_id = visual_music_notation_id,
                                                 play_button_id = play_button_id, button_area_id = button_area_id,
                                                 record_immediately = record_immediately,
                                                 transpose_first_melody_note = transpose_first_melody_note, clef = clef,
                                                 start_hidden = start_hidden,
                                                 sound_only_first_melody_note = sound_only_first_melody_note,
                                                 show_sheet_music = show_sheet_music,
                                                 sheet_music_id = sheet_music_id,
                                                 give_first_melody_note = give_first_melody_note,
                                                 trigger_start_of_stimuli_fun = trigger_start_of_stimuli_fun,
                                                 trigger_end_of_stimuli_fun = trigger_end_of_stimuli_fun)
  } else if (stimuli_type == "frequencies") {
    return_stimuli <- present_stimuli_frequencies(stimuli, display_modality, ...)
  } else if (stimuli_type == "pitch_classes") {
    return_stimuli <- present_stimuli_pitch_classes(stimuli, display_modality, octave = octave, ...)
  } else if (stimuli_type == "scientific_music_notation") {
    return_stimuli <- present_stimuli_scientific_music_notation(stimuli, display_modality, ...)
  } else if (stimuli_type == "rhythms") {
    return_stimuli <- present_stimuli_rhythms(stimuli, ...)
    # music file types
  } else if (stimuli_type == "midi_file") {
    return_stimuli <- present_stimuli_midi_file(stimuli, display_modality, start_note = start_note, end_note = end_note, ...)
  } else if (stimuli_type == "musicxml_file") {
    return_stimuli <- present_stimuli_music_xml_file(stimuli, display_modality, sound_only_first_melody_note, start_hidden, show_sheet_music, page_type = page_type, ...)
    # Mixed
  } else if (stimuli_type == "mixed") {
    return_stimuli <- present_stimuli_mixed(display_modality, button_text = button_text, ...)
  } else {
    stop(paste0('stimuli_type not recognised: ', stimuli_type))
  }

  # Wrap in page, if necessary

  if(length(page_type) == 0) {
    res <- return_stimuli
  } else if(!is.null(return_stimuli$present_stimuli_characters_auditory)) {

    res <- retrieve_page_type(page_type = "play_text_page",  stimuli_wrapped = return_stimuli, underlying_page_type = page_type,
                              page_text = page_text, page_title = page_title, ...)

  } else if(page_type == "record_midi_page") {

    res <- retrieve_page_type(page_type = page_type,
                              stimuli_wrapped = return_stimuli,
                              page_text = page_text, page_title = page_title, interactive = interactive,
                              stimuli = stimuli, stimuli_reactive = stimuli_reactive,
                              answer_meta_data = answer_meta_data, get_answer = get_answer, midi_device = midi_device,
                              page_label = page_label, button_text = button_text, play_button_text = play_button_text,
                              save_answer = save_answer,
                              user_rating = user_rating,
                              happy_with_response = happy_with_response,
                              attempts_left = attempts_left,
                              auto_next_page = auto_next_page,
                              page_text_first = page_text_first, max_goes_forced = max_goes_forced, max_goes = max_goes,
                              melody_no = melody_no, show_progress = show_progress, total_no_melodies = total_no_melodies,
                              volume_meter = volume_meter, volume_meter_type = volume_meter_type,
                              show_record_button = show_record_button, ...)

  } else if(page_type == "record_audio_page") {

    res <- retrieve_page_type(page_type = page_type,
                              stimuli_wrapped = return_stimuli,
                              page_text = page_text, page_title = page_title,
                              interactive = interactive,
                              stimuli = stimuli, stimuli_reactive = stimuli_reactive, answer_meta_data = answer_meta_data,
                              get_answer = get_answer, page_label = page_label,
                              button_text = button_text, play_button_text = play_button_text, durations = durations,
                              save_answer = save_answer,
                              auto_next_page = auto_next_page, user_rating = user_rating,
                              page_text_first = page_text_first,
                              happy_with_response = happy_with_response,
                              attempts_left = attempts_left, max_goes_forced = max_goes_forced, max_goes = max_goes,
                              melody_no = melody_no, show_progress = show_progress, total_no_melodies = total_no_melodies,
                              volume_meter = volume_meter, volume_meter_type = volume_meter_type, show_sheet_music_after_record = show_sheet_music_after_record,
                              show_record_button = show_record_button, ...)
  } else {
    if(page_text_first) {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(page_text), shiny::tags$br(), return_stimuli)
    } else {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), return_stimuli, shiny::tags$p(page_text))
    }
    res <- retrieve_page_type(page_type = page_type, stimuli_wrapped = full_page, button_text = button_text, choices = choices, slider_value = slider_value, slider_min = slider_min, slider_max = slider_max, ...)
  }

  res

}


retrieve_page_type <- function(page_type = character(),
                               stimuli_wrapped,
                               page_text = "Click to hear the stimuli",
                               page_title = " ",
                               interactive = FALSE,
                               stimuli_reactive = FALSE,
                               answer_meta_data = character(),
                               midi_device = " ",
                               page_label = " ",
                               button_text = psychTestR::i18n("Next"), play_button_text = "Play", get_answer = function() {},
                               save_answer = TRUE, auto_next_page = FALSE,
                               choices = character(), user_rating = FALSE, page_text_first = TRUE,
                               happy_with_response = FALSE, attempts_left = integer(), max_goes_forced = FALSE, max_goes = 1,
                               melody_no = 0, total_no_melodies = 0, show_progress = FALSE,
                               slider_value = 5, slider_min = 0, slider_max = 10,
                               volume_meter = FALSE, volume_meter_type = 'default',
                               show_sheet_music_after_record = FALSE,
                               show_record_button = FALSE, ...) {


  stopifnot(assertthat::is.string(page_type),
            class(stimuli_wrapped) == "shiny.tag",
            is.character(page_text) | class (page_text) == "shiny.tag", is.character(page_title), is.scalar.logical(interactive),
            is.logical(stimuli_reactive),
            is.character(answer_meta_data) | is.data.frame(answer_meta_data),
            is.character(midi_device),
            is.character(page_label), is.character(button_text),
            is.character(play_button_text), is.function(get_answer),
            is.logical(save_answer), is.logical(auto_next_page),
            is.character(choices) & is.vector(choices), is.logical(user_rating), is.logical(page_text_first),
            is.logical(happy_with_response), is.numeric(attempts_left), is.logical(max_goes_forced),
            is.numeric(max_goes),
            is.numeric(melody_no) & length(melody_no) == 1,
            is.numeric(total_no_melodies) & length(total_no_melodies) == 1,
            is.logical(show_progress),
            assertthat::is.scalar(slider_value) & is.numeric(slider_value),
            assertthat::is.scalar(slider_min) & is.numeric(slider_min),
            assertthat::is.scalar(slider_max) & is.numeric(slider_max),
            is.scalar.logical(volume_meter),
            assertthat::is.string(volume_meter_type),
            is.scalar.logical(show_sheet_music_after_record),
            is.scalar.logical(show_record_button)
            )


  # The stimuli should already be wrapped by one of the present_stimuli functions before reaching here

  page.fun <- get_page_function(page_type)

  args <- as.list(match.call(expand.dots = FALSE)$...) # i.e the "additional arguments"

  # feed the body to the page, but using the correct argument
  # i.e some pages accept "body" whilst others accept "prompt"
  args <- check_correct_argument_for_body(page_type, args, stimuli_wrapped)

  if (page_type == "one_button_page" | page_type == "empty_page") {
    args$button_text <- button_text
  } else if(page_type == "slider_page") {
      args$min <- slider_min
      args$max <- slider_max
      args$value <- slider_value
  } else if(page_type == "NAFC_page" | page_type == "dropdown_page") {
    args$choices <- choices
  } else if(page_type == "play_text_page") {
    args <- stimuli_wrapped
    args$present_stimuli_characters_auditory <- NULL
    args$page_text <- page_text
    args$page_title <- page_title
  } else if(page_type %in% c("record_audio_page", "record_midi_page") ) {
    args <- c(args,
              list(
                "page_text" = page_text,
                "page_title" = page_title,
                "interactive" = interactive,
                "answer_meta_data" =  answer_meta_data,
                "stimuli_reactive" = stimuli_reactive,
                "page_type" = page_type,
                "get_answer" = get_answer,
                "midi_device" = midi_device,
                "label" =  page_label,
                "play_button_text" = play_button_text,
                "save_answer" = save_answer,
                "auto_next_page"  = auto_next_page,
                "user_rating" = user_rating,
                "happy_with_response" = happy_with_response,
                "attempts_left" = attempts_left,
                "max_goes_forced" = max_goes_forced,
                "max_goes" = max_goes,
                "melody_no" = melody_no,
                "total_no_melodies" = total_no_melodies,
                "show_progress" = show_progress,
                "volume_meter" = volume_meter,
                "volume_meter_type" = volume_meter_type,
                "show_sheet_music_after_record" = show_sheet_music_after_record,
                "show_record_button" = show_record_button))

  } else if(page_type == "record_key_presses_page") {
    args$body <- page_text
  } else {
    stop('Unknown page type.')
  }

  validate_page_types(page_type, args)

  # set the page up with additional arguments
  page <- do.call(what = page.fun, args = args)
  page

}


get_page_function <- function(page_type) {
  if(page_type %in% c("record_audio_page", "record_midi_page", "record_key_presses_page",
                      "record_spoken_words_page", "play_text_page", "empty_page")) {
    page.fun <- get(page_type, asNamespace("musicassessr"))
  } else{
    page.fun <- get(page_type, asNamespace("psychTestR"))
  }
  page.fun
}

page_types = c("one_button_page",
               "record_audio_page",
               "NAFC_page",
               "dropdown_page",
               "slider_page",
               "text_input_page",
               "record_key_presses_page",
               "record_midi_page")


check_correct_argument_for_body <- function(page_type_string, args, stimuli_wrapped) {
  # feed the body to the page, but using the correct argument
  # i.e some pages accept "body" whilst others accept "prompt"
  if (page_type_string %in% c("one_button_page", "record_audio_page", "record_midi_page",  "record_key_presses_page", "empty_page")) {
    args[["stimuli"]] <- stimuli_wrapped
  } else if (page_type_string %in% c("NAFC_page", "dropdown_page", "slider_page", "text_input_page")) {
    args[["prompt"]] <- stimuli_wrapped
  } else {
    stop("Unknown body argument.")
  }
  args
}

