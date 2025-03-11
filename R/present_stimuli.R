
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
#' @param transpose_visual_notation
#' @param clef
#' @param volume
#' @param audio_playback_as_single_play_button
#' @param max_goes
#' @param melody_no
#' @param total_no_melodies
#' @param show_progress
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param octave
#' @param volume_meter
#' @param volume_meter_type
#' @param show_sheet_music_after_record
#' @param interactive
#' @param show_record_button
#' @param trigger_start_of_stimulus_fun A string of an anonymous Javascript function (with body) to trigger when the stimulus begins.
#' @param trigger_end_of_stimulus_fun A string of an anonymous Javascript function (with body) to trigger when the stimulus is completed.
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param reactive_melody_no
#' @param mute_midi_playback Should MIDI audio feedback be muted on record_midi_pages?
#' @param db_vars Vars for the DB as a named list.
#' @param lowest_reading_note
#' @param highest_reading_note
#' @param lyrics
#' @param feedback
#' @param asynchronous_api_mode
#' @param key
#' @param on_complete
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
                            choices = character(), user_rating = FALSE,
                            page_text_first = TRUE, happy_with_response = FALSE,
                            attempts_left = 1L, visual_music_notation_id = "sheet_music",
                            play_button_id = "playButton", button_area_id = "button_area",
                            hideOnPlay = FALSE, record_immediately = FALSE, max_goes_forced = FALSE,
                            transpose_visual_notation = 0L,
                            clef = "auto",
                            volume = 1, audio_playback_as_single_play_button = FALSE,
                            max_goes = 1L, melody_no = 0L, total_no_melodies = 0L,
                            show_progress = FALSE,
                            sheet_music_start_hidden = FALSE,
                            sound_only_first_melody_note = FALSE,
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
                            trigger_start_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus started!');"),
                            trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
                            first_note_message = psychTestR::i18n("first_note_is"),
                            transposed_message = psychTestR::i18n("transposed"),
                            play_first_note_button_text = psychTestR::i18n("play_first_note"),
                            reactive_melody_no = FALSE,
                            mute_midi_playback = FALSE,
                            db_vars = NULL,
                            lowest_reading_note = NA,
                            highest_reading_note = NA,
                            lyrics = NULL,
                            feedback = FALSE,
                            asynchronous_api_mode = FALSE,
                            key = NULL,
                            on_complete = NULL, ...) {


  stopifnot(is.vector(stimuli), is.character(stimuli_type), is.character(display_modality), is.character(page_type),
            is.character(page_text) || is(page_text, "shiny.tag") || is(page_text, "shiny.tag.list"),
            is.character(page_title),
            is.numeric(slide_length),
            is.character(answer_meta_data) || is.data.frame(answer_meta_data),
            is.null.or(get_answer, is.function),
            is.scalar.logical(save_answer),
            is.scalar.logical(stimuli_reactive), is.character(midi_device),
            is.character(page_label),
            is.character(button_text), is.character(play_button_text),
            is.numeric(note_length), is.character(sound), is.scalar.logical(asChord), is.scalar.logical(ascending),
            is.integer(start_note), is.integer(end_note) | end_note == "end",
            is.vector(durations) & is.numeric(durations) | is.na(durations),
            is.character(choices) & is.vector(choices), is.scalar.logical(user_rating),
            is.scalar.logical(page_text_first), is.scalar.logical(happy_with_response),
            is.numeric(attempts_left), is.character(visual_music_notation_id),
            is.character(play_button_id), is.character(button_area_id),
            is.scalar.logical(hideOnPlay), is.scalar.logical(record_immediately), is.scalar.logical(max_goes_forced),
            is.integer(transpose_visual_notation),
            is.scalar.character(clef),
            is.scalar.numeric(volume),
            is.scalar.logical(audio_playback_as_single_play_button),
            is.numeric(max_goes),
            is.scalar.numeric(melody_no),
            is.numeric(total_no_melodies) & length(total_no_melodies) == 1,
            is.scalar.logical(show_progress),
            is.scalar.logical(sheet_music_start_hidden),
            is.scalar.logical(sound_only_first_melody_note) | is.scalar.numeric(sound_only_first_melody_note),
            is.scalar.character(sheet_music_id),
            is.scalar.logical(give_first_melody_note),
            octave %in% 0:9,
            is.scalar.logical(volume_meter),
            is.scalar.character(volume_meter_type),
            is.scalar.logical(show_sheet_music_after_record),
            is.scalar.logical(interactive),
            is.scalar.logical(show_record_button),
            is.null(trigger_start_of_stimulus_fun) || is.na(trigger_start_of_stimulus_fun) || is.scalar.character(trigger_start_of_stimulus_fun),
            is.null(trigger_end_of_stimulus_fun) || is.na(trigger_end_of_stimulus_fun) || is.scalar.character(trigger_end_of_stimulus_fun),
            is.scalar.character(first_note_message),
            is.scalar.character(transposed_message),
            is.scalar.character(play_first_note_button_text),
            is.scalar.logical(reactive_melody_no),
            is.scalar.logical(mute_midi_playback),
            is.null.or(db_vars, is.list),
            is.na(lowest_reading_note) || is.numeric(lowest_reading_note),
            is.na(highest_reading_note) || is.numeric(highest_reading_note),
            is.null.or(lyrics, is.scalar.character),
            is.scalar.logical(feedback),
            is.scalar.logical(asynchronous_api_mode),
            is.null.or(key, is.scalar.character),
            is.null.or(on_complete, is.function)
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
    return_stimuli <- present_stimuli_audio(audio_url = stimuli, hideOnPlay = hideOnPlay, volume = volume, audio_playback_as_single_play_button = audio_playback_as_single_play_button, trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun, trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun, ...)
  } else if (stimuli_type == "audio_WJD") {
    return_stimuli <- present_stimuli_audio_WJD(pattern = stimuli, answer_meta_data = answer_meta_data, ...)
    # Musical stimuli types
  } else if (stimuli_type == "midi_notes") {
    return_stimuli <- present_stimuli_midi_notes(stimuli = stimuli, display_modality = display_modality,
                                                 page_type = page_type,
                                                 play_button_text = play_button_text, note_length = note_length,
                                                 sound = sound, asChord = asChord, ascending = ascending, durations = durations,
                                                 visual_music_notation_id = visual_music_notation_id,
                                                 play_button_id = play_button_id, button_area_id = button_area_id,
                                                 record_immediately = record_immediately,
                                                 transpose_visual_notation = transpose_visual_notation, clef = clef,
                                                 sheet_music_start_hidden = sheet_music_start_hidden,
                                                 sound_only_first_melody_note = sound_only_first_melody_note,
                                                 sheet_music_id = sheet_music_id,
                                                 give_first_melody_note = give_first_melody_note,
                                                 trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                                 trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                                 first_note_message = first_note_message,
                                                 transposed_message = transposed_message,
                                                 play_first_note_button_text = play_first_note_button_text,
                                                 lowest_reading_note = lowest_reading_note,
                                                 highest_reading_note = highest_reading_note,
                                                 key = key)
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
    return_stimuli <- present_stimuli_music_xml_file(stimuli, display_modality, sound_only_first_melody_note, sheet_music_start_hidden, page_type = page_type, ...)
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
                              page_text_first = page_text_first, max_goes_forced = max_goes_forced, max_goes = max_goes,
                              melody_no = melody_no, show_progress = show_progress, total_no_melodies = total_no_melodies,
                              volume_meter = volume_meter, volume_meter_type = volume_meter_type,
                              show_record_button = show_record_button, show_sheet_music_after_record = show_sheet_music_after_record, reactive_melody_no = reactive_melody_no,
                              mute_midi_playback = mute_midi_playback,
                              db_vars = db_vars, on_complete = on_complete, ...)

  } else if(page_type == "record_audio_page") {

    res <- retrieve_page_type(page_type = page_type,
                              stimuli_wrapped = return_stimuli,
                              page_text = page_text, page_title = page_title,
                              interactive = interactive,
                              stimuli = stimuli, stimuli_reactive = stimuli_reactive, answer_meta_data = answer_meta_data,
                              get_answer = get_answer, page_label = page_label,
                              button_text = button_text, play_button_text = play_button_text, durations = durations,
                              save_answer = save_answer,
                              user_rating = user_rating,
                              page_text_first = page_text_first,
                              happy_with_response = happy_with_response,
                              attempts_left = attempts_left, max_goes_forced = max_goes_forced, max_goes = max_goes,
                              melody_no = melody_no, show_progress = show_progress, total_no_melodies = total_no_melodies,
                              volume_meter = volume_meter, volume_meter_type = volume_meter_type, show_sheet_music_after_record = show_sheet_music_after_record,
                              show_record_button = show_record_button,
                              reactive_melody_no = reactive_melody_no,
                              db_vars = db_vars, on_complete = on_complete,
                              lyrics = lyrics, feedback = feedback, asynchronous_api_mode = asynchronous_api_mode, ...)
  } else {
    if(page_text_first) {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(page_text), shiny::tags$br(), return_stimuli)
    } else {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), return_stimuli, shiny::tags$p(page_text))
    }
    res <- retrieve_page_type(page_type = page_type, stimuli_wrapped = full_page, button_text = button_text, choices = choices, slider_value = slider_value, slider_min = slider_min, slider_max = slider_max, answer_meta_data = answer_meta_data, get_answer = get_answer, save_answer = save_answer, page_label = page_label, ...)
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
                               button_text = psychTestR::i18n("Next"),
                               play_button_text = psychTestR::i18n("Play"),
                               get_answer = function() {},
                               save_answer = TRUE,
                               choices = character(), user_rating = FALSE, page_text_first = TRUE,
                               happy_with_response = FALSE, attempts_left = 1L, max_goes_forced = FALSE, max_goes = 1L,
                               melody_no = 0L, total_no_melodies = 0L, show_progress = FALSE,
                               slider_value = 5, slider_min = 0, slider_max = 10,
                               volume_meter = FALSE, volume_meter_type = 'default',
                               show_sheet_music_after_record = FALSE,
                               show_record_button = FALSE,
                               reactive_melody_no = FALSE,
                               mute_midi_playback = FALSE,
                               db_vars = NULL,
                               lyrics = NULL,
                               feedback = FALSE,
                               asynchronous_api_mode = FALSE,
                               on_complete = NULL,
                               ...) {


  stopifnot(is.scalar.character(page_type),
            class(stimuli_wrapped) == "shiny.tag",
            is.scalar.character(page_text) || is(page_text, "shiny.tag") || is(page_text, "shiny.tag.list"),
            is.scalar.character(page_title),
            is.scalar.logical(interactive),
            is.scalar.logical(stimuli_reactive),
            is.character(answer_meta_data) || is.data.frame(answer_meta_data),
            is.scalar.character(midi_device),
            is.scalar.character(page_label), is.scalar.character(button_text),
            is.scalar.character(play_button_text),
            is.null.or(get_answer, is.function),
            is.scalar.logical(save_answer),
            is.character(choices), is.scalar.logical(user_rating), is.scalar.logical(page_text_first),
            is.scalar.logical(happy_with_response), is.scalar.numeric(attempts_left), is.scalar.logical(max_goes_forced),
            is.scalar.numeric(max_goes),
            is.scalar.numeric(melody_no),
            is.scalar.numeric(total_no_melodies),
            is.scalar.logical(show_progress),
            is.scalar.numeric(slider_value),
            is.scalar.numeric(slider_min),
            is.scalar.numeric(slider_max),
            is.scalar.logical(volume_meter),
            is.scalar.character(volume_meter_type),
            is.scalar.logical(show_sheet_music_after_record),
            is.scalar.logical(show_record_button),
            is.scalar.logical(reactive_melody_no),
            is.scalar.logical(mute_midi_playback),
            is.null.or(db_vars, is.list),
            is.null.or(lyrics, is.scalar.character),
            is.scalar.logical(feedback),
            is.scalar.logical(asynchronous_api_mode),
            is.null.or(on_complete, is.function)
            )


  # The stimuli should already be wrapped by one of the present_stimuli functions before reaching here

  page.fun <- get_page_function(page_type)

  args <- as.list(match.call(expand.dots = FALSE)$...) # i.e the "additional arguments"

  # feed the body to the page, but using the correct argument
  # i.e some pages accept "body" whilst others accept "prompt"
  args <- check_correct_argument_for_body(page_type, args, stimuli_wrapped)

  if (page_type == "one_button_page") {
    args$button_text <- button_text
  } else if(page_type == "empty_page") {
    args$button_text <- button_text
    args$answer_meta_data <- answer_meta_data
    args$get_answer <- get_answer
    args$save_answer <- save_answer
    args$label <- page_label
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
                "show_record_button" = show_record_button,
                "reactive_melody_no" = reactive_melody_no,
                "mute_midi_playback" = mute_midi_playback,
                "db_vars" = db_vars,
                "lyrics" = lyrics,
                "feedback" = feedback,
                "asynchronous_api_mode" = asynchronous_api_mode,
                "on_complete" = on_complete
                ))

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
  if (page_type_string %in% c("one_button_page", "empty_page")) {
    args[["body"]] <- stimuli_wrapped
  } else if(page_type_string %in% c("record_audio_page", "record_midi_page",  "record_key_presses_page") ) {
    args[["stimuli"]] <- stimuli_wrapped
  } else if (page_type_string %in% c("NAFC_page", "dropdown_page", "slider_page", "text_input_page")) {
    args[["prompt"]] <- stimuli_wrapped
  } else {
    stop("Unknown body argument.")
  }
  args
}

