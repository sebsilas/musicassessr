

#' Present Stimuli
#'
#' @param stimuli
#' @param stimuli_type
#' @param display_modality
#' @param page_type
#' @param page_text
#' @param page_title
#' @param slide_length
#' @param special_page_underlying_page_type
#' @param record_audio_method
#' @param answer_meta_data
#' @param get_answer
#' @param stimuli_reactive
#' @param midi_device
#' @param show_aws_controls
#' @param page_label
#' @param button_text
#' @param play_button_text
#' @param note_length
#' @param sound
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
present_stimuli <- function(stimuli, stimuli_type, display_modality, page_type = NULL,
                            page_text = " ", page_title = " ",  slide_length,
                            special_page_underlying_page_type = "one_button_page", record_audio_method = "aws_pyin",
                            answer_meta_data = NULL, get_answer = get_answer_null, save_answer = FALSE,
                            stimuli_reactive = FALSE, midi_device = " ",
                            show_aws_controls = FALSE, page_label = "x",
                            button_text = "Next", play_button_text = "Play",
                            note_length = 0.5, sound = "piano", asChord = FALSE, ascending = TRUE,
                            start_note = 0, end_note = "end", durations = 'null', show_record_button = FALSE,
                            auto_next_page = FALSE, choices = NULL, user_rating = FALSE,
                            page_text_first = TRUE, happy_with_response = FALSE,
                            attempts_left = NULL, visual_music_notation_id = "sheet_music",
                            play_button_id = "playButton", button_area_id = "button_area",
                            hideOnPlay = FALSE, record_immediately = FALSE, ...) {

  if(is.null(page_type)) {
    page_type <- 'null'
  }

  # reactive stimuli i.e that requires something at run time, in a reactive_page
  if (!stimuli_reactive) {
    return_stimuli <- present_stimuli_static(stimuli = stimuli, stimuli_type = stimuli_type, display_modality = display_modality, page_type = page_type, get_answer = get_answer,
                                             midi_device = midi_device, play_button_text = play_button_text,
                                             record_audio_method = record_audio_method, sound = sound, note_length = note_length,
                                             asChord = asChord, slide_length = slide_length, page_title = page_title,
                                             start_note = start_note, end_note = end_note, durations = durations,
                                             auto_next_page = auto_next_page,
                                             visual_music_notation_id = visual_music_notation_id,
                                             play_button_id = play_button_id, button_area_id = button_area_id,
                                             hideOnPlay = hideOnPlay, answer_meta_data = answer_meta_data,
                                             record_immediately = record_immediately, ...)
  }
  else {
    return_stimuli <- present_stimuli_reactive(stimuli_reactive, stimuli,
                                               stimuli_type,
                                               display_modality, page_type,
                                               get_answer = get_answer, midi_device = midi_device,
                                               record_audio_method = record_audio_method,
                                               asChord = asChord, slide_length = slide_length, page_title = page_title,
                                               start_note = start_note, end_note = end_note, durations = durations,
                                               auto_next_page = auto_next_page, visual_music_notation_id = visual_music_notation_id,
                                               hideOnPlay = hideOnPlay,
                                               record_immediately = record_immediately, ...)
  }

  # append page text to the page
  # record midi and present auditory character pages are custom page types created here
  # they use psychTestR reactive pages and have to be dealt with separately
  # play_text_page and record_midi_pages are "special" pages

  # is interactive?
  interactive <- ifelse(stimuli == "interactive", TRUE, FALSE)

  if(is.null(page_type) | page_type == 'null') {
    res <- return_stimuli
  } else if(!is.null(return_stimuli$present_stimuli_characters_auditory)) {

    res <- retrieve_page_type(page_type = "play_text_page",
                              stimuli_wrapped = return_stimuli, underlying_page_type = page_type,
                              page_text = page_text, page_title = page_title, ...)

  } else if(page_type == "record_midi_page") {

    res <- retrieve_page_type(page_type = page_type,
                              stimuli_wrapped = return_stimuli,
                              page_text = page_text, page_title = page_title, interactive = interactive,
                              stimuli = stimuli, stimuli_reactive = stimuli_reactive,
                              answer_meta_data = answer_meta_data, get_answer = get_answer, midi_device = midi_device,
                              page_label = page_label, button_text = button_text, play_button_text = play_button_text,
                              show_record_button = show_record_button,
                              save_answer = save_answer,
                              auto_next_page = auto_next_page, user_rating = user_rating,
                              happy_with_response = happy_with_response,
                              attempts_left = attempts_left, ...)

  } else if(page_type == "record_audio_page") {

    res <- retrieve_page_type(page_type = page_type,
                              stimuli_wrapped = return_stimuli,
                              page_text = page_text, page_title = page_title, interactive = interactive,
                              stimuli = stimuli, stimuli_reactive = stimuli_reactive, answer_meta_data = answer_meta_data,
                              get_answer = get_answer, record_audio_method = record_audio_method,
                              show_aws_controls = show_aws_controls, page_label = page_label,
                              button_text = button_text, play_button_text = play_button_text, durations = durations,
                              show_record_button = show_record_button,
                              save_answer = save_answer,
                              auto_next_page = auto_next_page, user_rating = user_rating,
                              page_text_first = page_text_first,
                              happy_with_response = happy_with_response,
                              attempts_left = attempts_left, ...)

  }

  else {
    if(page_text_first) {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(page_text), shiny::tags$br(), return_stimuli)
    } else {
      full_page <- shiny::tags$div(shiny::tags$h2(page_title), return_stimuli, shiny::tags$p(page_text))
    }
    res <- retrieve_page_type(page_type = page_type, stimuli_wrapped = full_page, button_text = button_text, choices = choices, ...)
  }

  res

}

present_stimuli_reactive <- function(stimuli_reactive_keyword, stimuli, stimuli_type, display_modality, page_type, record_audio_method = "aws_pyin",
                                     page_title = " ", start_note = start_note, end_note = end_note, auto_next_page = FALSE, ...) {

  # pass down the stimuli to be called at run time
  # stimuli_reactive_keyword defines the type of reactive stimuli

  return_fun <- function(reactive_stimuli) {
    present_stimuli_static(stimuli = reactive_stimuli, body = stimuli_wrapped,
                           page_text = page_text, page_title = page_title, display_modality = display_modality,
                           interactive = interactive, answer_meta_data = answer_meta_data,
                           stimuli_reactive = stimuli_reactive, stimuli_type = stimuli_type,
                           page_type = page_type, record_audio_method = record_audio_method,
                           start_note = start_note, end_note = end_note, auto_next_page = auto_next_page)
  }

  list(stimuli_reactive_keyword, return_fun)

}



page_types = c("one_button_page",
              "record_audio_page",
                "NAFC_page",
               "dropdown_page",
               "slider_page",
               "text_input_page",
               "record_key_presses_page",
               "record_midi_page")


retrieve_page_type <- function(page_type_string, stimuli_wrapped, special_page_underlying_page_type = "one_button_page",
                               page_text = "Click to hear the stimuli", page_title = " ", interactive, stimuli,
                               stimuli_reactive = FALSE, answer_meta_data = NULL, midi_device = " ",
                               record_audio_method = "aws_pyin", show_aws_controls = FALSE, page_label = " ",
                               button_text = "Next", play_button_text = "Play", get_answer = get_answer_null,
                               show_record_button = FALSE, save_answer = TRUE, auto_next_page = FALSE,
                               choices = NULL, user_rating = FALSE, page_text_first = TRUE,
                               happy_with_response = FALSE, attempts_left = NULL, ...) {


  # the stimuli should already be wrapped by one of the present_stimuli functions
  # before reaching here

  if(page_type_string == "record_audio_page" |
     page_type_string == "record_midi_page" |
     page_type_string == "record_key_presses_page" |
     page_type_string == "record_spoken_words_page" |
     page_type_string == "play_text_page") {
    page.fun <- get(page_type_string, asNamespace("musicassessr"))
  } else{
    page.fun <- get(page_type_string, asNamespace("psychTestR"))
  }

  args <- match.call(expand.dots = FALSE)$... # i.e the "additional arguments"
  # remove certain arguments that are not really "additional"
  args$asChord <- NULL
  args$octave <- NULL

  # check if certain page types have their required arguments
  #validate.page.types(page_type_string, args)

  # feed the body to the page, but using the correct argument
  # i.e some pages accept "body" whilst others accept "prompt"
  args <- check.correct.argument.for.body(page_type_string, args, stimuli_wrapped)

  # convert from pair list:
  args <- as.list(args)

  if (page_type_string == "one_button_page") {
    args$button_text <- button_text
  }
  else if(page_type_string == "NAFC_page" | page_type_string == "dropdown_page") {
    args$choices <- choices
  }
  else if(page_type_string == "play_text_page") {
    args <- stimuli_wrapped
    args$present_stimuli_characters_auditory <- NULL
    args$page_text <- page_text
    args$page_title <- page_title
  }

  else if(!stimuli_reactive & page_type_string == "record_audio_page" |
          !stimuli_reactive & page_type_string == "record_midi_page") {

    args$stimuli <- stimuli
    args$body <- stimuli_wrapped
    args$page_text <- page_text
    args$page_title <- page_title
    args$interactive <- interactive
    args$answer_meta_data <- answer_meta_data
    args$stimuli_reactive <- stimuli_reactive
    args$page_type <- page_type_string
    args$get_answer <- get_answer
    args$midi_device <- midi_device
    args$method <- record_audio_method
    args$show_aws_controls <- show_aws_controls
    args$label <- page_label
    args$play_button_text <- play_button_text
    args$show_record_button <- show_record_button
    args$save_answer <- save_answer
    args$auto_next_page <- auto_next_page
    args$user_rating <- user_rating
    args$happy_with_response <- happy_with_response
    args$attempts_left <- attempts_left
  }

  else if(stimuli_reactive & page_type_string == "record_audio_page") {
    args$stimuli_reactive <- stimuli_wrapped[[1]]
    args$stimuli <- stimuli_wrapped[[2]]
    args$page_type <- "record_audio_page"
    args$page_text <- page_text
    args$page_title <- page_title
    args$get_answer <- get_answer
    args$method <- record_audio_method
    args$show_aws_controls <- show_aws_controls
    args$label <- page_label
    args$button_text <- button_text
    args$play_button_text <- play_button_text
    args$show_record_button <- show_record_button
    args$save_answer <- save_answer
    args$auto_next_page <- auto_next_page
    args$user_rating <- user_rating
    args$happy_with_response <- happy_with_response
    args$attempts_left <- attempts_left
  }

  else if(stimuli_reactive & page_type_string == "record_midi_page") {
    args$stimuli_reactive <- stimuli_wrapped[[1]]
    args$stimuli <- stimuli_wrapped[[2]]
    args$page_type <- "record_midi_page"
    args$page_text <- page_text
    args$page_title <- page_title
    args$get_answer <- get_answer
    args$midi_device <- midi_device
    args$label <- page_label
    args$button_text <- button_text
    args$play_button_text <- play_button_text
    args$show_record_button <- show_record_button
    args$save_answer <- save_answer
    args$auto_next_page <- auto_next_page
    args$user_rating <- user_rating
    args$happy_with_response <- happy_with_response
    args$attempts_left <- attempts_left
  }

  else {
    #stop('Unknown special page type.')
  }

  # set the page up with additional arguments
  page <- do.call(what = page.fun, args = args)
  page

}


present_stimuli_static <- function(stimuli, stimuli_type, display_modality, page_type, get_answer,
                                   midi_device = " ", show_aws_controls = FALSE, answer_meta_data = 0,
                                   button_text = "Next", play_button_text = "Play",
                                   record_audio_method = "aws_pyin", note_length = 0.5,
                                   sound = "piano", asChord = FALSE, slide_length = 0.5, page_title = " ",
                                   start_note = 0, end_note = "end", durations = 'null', auto_next_page = FALSE,
                                   visual_music_notation_id = "sheet_music", play_button_id = "playButton",
                                   button_area_id = "button_area", hideOnPlay = FALSE, record_immediately = FALSE, ...) {

  # generic stimuli types

  if (stimuli_type == "digits" | stimuli_type == "letters" | stimuli_type == "words") {
    return_stimuli <- present_stimuli_characters(stimuli = stimuli,
                                                 display_modality = display_modality,
                                                 page_type = page_type,
                                                 slide_length = slide_length,
                                                 rate = rate, page_title = page_title, ...)
  }

  else if (stimuli_type == "images") {
    return_stimuli <- present_stimuli_images(stimuli = stimuli, slide_length = slide_length, ...)
  }

  else if (stimuli_type == "video") {
    return_stimuli <- present_stimuli_video(video_url = stimuli, ...)
  }

  else if (stimuli_type == "audio") {
    return_stimuli <- present_stimuli_audio(audio_url = stimuli, hideOnPlay = hideOnPlay, ...)
  }

  else if (stimuli_type == "audio_WJD") {
    return_stimuli <- present_stimuli_audio_WJD(pattern = stimuli, answer_meta_data = answer_meta_data, ...)
  }

  # musical stimuli types

  else if (stimuli_type == "midi_notes") {
    return_stimuli <- present_stimuli_midi_notes(stimuli = stimuli, display_modality = display_modality, get_answer = get_answer,
                                                 page_type = page_type, midi_device = midi_device,
                                                 record_audio_method = record_audio_method,
                                                 show_aws_controls = show_aws_controls,
                                                 page_label = page_label, button_text = button_text,
                                                 play_button_text = play_button_text, note_length = note_length,
                                                 sound = sound, asChord = asChord, ascending = ascending, durations = durations,
                                                 auto_next_page = auto_next_page,
                                                 visual_music_notation_id = visual_music_notation_id,
                                                 play_button_id = play_button_id, button_area_id = button_area_id,
                                                 record_immediately = record_immediately, ...)
  }

  else if (stimuli_type == "frequencies") {
    return_stimuli <- present_stimuli_frequencies(stimuli, display_modality, ...)
  }

  else if (stimuli_type == "pitch_classes") {
    return_stimuli <- present_stimuli_pitch_classes(stimuli, display_modality, ...)
  }

  else if (stimuli_type == "scientific_music_notation") {
    return_stimuli <- present_stimuli_scientific_music_notation(stimuli, display_modality, ...)
  }

  else if (stimuli_type == "rhythms") {
    return_stimuli <- present_stimuli_rhythms(stimuli, ...)
  }

  # music file types

  else if (stimuli_type == "midi_file") {
    return_stimuli <- present_stimuli_midi_file(stimuli, display_modality, start_note = start_note, end_note = end_note, ...)
  }

  else if (stimuli_type == "musicxml_file") {
    return_stimuli <- present_stimuli_music_xml_file(stimuli, display_modality, ...)
  }

  # mixed

  else if (stimuli_type == "mixed") {
    return_stimuli <- present_stimuli_mixed(display_modality, button_text = button_text, ...)
  }

  else {
    stop(paste0('stimuli_type not recognised: ', stimuli_type))
  }

}



reactive_stimuli <- function(stimuli_function, stimuli_reactive, prepared_stimuli, body = NULL) {
  # this is used at the page level (inside a page)

  if (sjmisc::str_contains(stimuli_reactive, "reactive")) {
    stimuli_function(reactive_stimuli = prepared_stimuli)
  }
  else {
    shiny::tags$div(id = "body", body)
  }
}

