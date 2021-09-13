#' Create a psychTestR test loop for having several attempts at playing back a melody.
#'
#' @param melody
#' @param melody_no
#' @param var_name
#' @param stimuli_type
#' @param max_goes
#' @param page_type
#' @param page_title
#' @param page_text
#' @param answer_meta_data
#' @param get_answer
#' @param rel_to_abs_mel_function
#' @param clip_stimuli_length
#' @param start_note
#' @param end_note
#' @param dur_list
#' @param arrhythmic
#' @param note_length
#' @param play_button_text
#' @param example
#'
#' @return
#' @export
#'
#' @examples
play_melody_loop <- function(melody = NULL, melody_no = "x", var_name = "melody", stimuli_type = "midi_notes", max_goes = 3,
                             page_type = "record_audio_page", page_title = "Copy The Melody", page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                             answer_meta_data = " ", get_answer = get_answer_pyin,
                             rel_to_abs_mel_function = musicassessr:rel_to_abs_mel_mean_centred, clip_stimuli_length = FALSE,
                             start_note = 1, end_note = "end", dur_list = 'null', arrhythmic = FALSE, note_length = 0.5,
                             play_button_text = psychTestR::i18n("Play"), example = FALSE) {

  save_answer <- example_save(example)

  c(
    # set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      # repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)
      psychTestR::set_global("max_goes", max_goes, state)
      # grab sampled melody for this trial (if one not specified)
      grab_sampled_melody(melody, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function = rel_to_abs_mel_function)
    }),

    # keep in loop until the participant confirms they are happy with their entry
    psychTestR::while_loop(test = function(state, ...) {
      number_attempts <- psychTestR::get_global("number_attempts", state)
      user_answer <- psychTestR::get_global("user_satisfied", state)
      user_wants_to_play_again <- user_answer %in% dict_key_to_translations("Try_Again")
    },
    logic = list(

      # present the melody
      present_melody(stimuli = melody,
                     stimuli_type = stimuli_type,
                     display_modality = "auditory",
                     page_title = page_title,
                     page_text = page_text,
                     page_type = page_type,
                     record_audio_method = "aws_pyin",
                     answer_meta_data = answer_meta_data,
                     get_answer = get_answer,
                     save_answer = save_answer,
                     page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                     button_text = psychTestR::i18n("Record"),
                     play_button_text = play_button_text,
                     start_note = start_note,
                     end_note = end_note,
                     dur_list = dur_list,
                     state = state,
                     melody_no = melody_no,
                     var_name = var_name),

      # was the user ok with this response?
      # check_melody_ok(state, var_name, page_title, max_goes),

      # update and see how to proceed
      update_play_melody_loop_and_save(state)
    )
    ) # end psychTestR::while_loop
  ) # end join
}

present_melody <- function(stimuli, stimuli_type, display_modality, page_title, page_text,
                           page_type, record_audio_method, answer_meta_data, get_answer,
                           save_answer, page_label, button_text, play_button_text, start_note,
                           end_note, dur_list, state, melody_no, var_name, ...) {

  psychTestR::reactive_page(function(state, ...) {

    number_attempts <- psychTestR::get_global("number_attempts", state)
    max_goes <- psychTestR::get_global("max_goes", state)
    attempts_left <- max_goes - number_attempts

    # grab vars
    melody <- melody_checks(stimuli, state, stimuli_type)$melody
    start_note <- melody_checks(stimuli, state, stimuli_type)$start_note
    end_note <- melody_checks(stimuli, state, stimuli_type)$end_note
    dur_list <- melody_checks(stimuli, state, stimuli_type)$dur_list

    answer_meta_data <- psychTestR::get_global("answer_meta_data", state)

    if(page_type == "record_midi_page") {

      midi_device <- psychTestR::get_global("midi_device", state)
      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      present_stimuli(stimuli = melody,
                      stimuli_type = stimuli_type,
                      display_modality = "auditory",
                      page_title = page_title,
                      page_text = page_text,
                      page_type = page_type,
                      answer_meta_data = answer_meta_data,
                      get_answer = get_answer,
                      save_answer = save_answer,
                      midi_device = midi_device,
                      page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                      play_button_text = play_button_text,
                      start_note = start_note,
                      end_note = end_note,
                      dur_list = dur_list,
                      happy_with_response = TRUE,
                      attempts_left = attempts_left)

    } else {
      # page 1, play melody
      present_stimuli(stimuli = melody,
                      stimuli_type = stimuli_type,
                      display_modality = "auditory",
                      page_title = page_title,
                      page_text = page_text,
                      page_type = "record_audio_page",
                      record_audio_method = "aws_pyin",
                      answer_meta_data = answer_meta_data,
                      get_answer = get_answer,
                      save_answer = save_answer,
                      page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                      button_text = psychTestR::i18n("Record"),
                      play_button_text = play_button_text,
                      start_note = start_note,
                      end_note = end_note,
                      dur_list = dur_list,
                      user_rating = TRUE,
                      happy_with_response = TRUE,
                      attempts_left = attempts_left
                      )
    }

  })
}


example_save <- function(example) {
  ifelse(example, FALSE, TRUE)
}

grab_sampled_melody <- function(melody, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred, ...) {
  if(is.null(melody)) {
    print('sample melody because one not specified')

    bottom_range <- psychTestR::get_global("bottom_range", state)
    top_range <- psychTestR::get_global("top_range", state)
    trials <- psychTestR::get_global(var_name, state)
    #PBET stuff:
    #trial_char <- get_trial_characteristics(trial_df = trials, trial_no = melody_no)
    # inst <- psychTestR::get_global("inst", state)
    # sample <- sample_melody_in_key(inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$difficulty, length = trial_char$melody_length)

    if(stimuli_type == "midi_file") {
      sort_sampled_midi_file(trials, melody_no, clip_stimuli_length, state)
    } else {
      sort_sampled_melody(trials, melody_no, state, arrhythmic, rel_to_abs_mel_function, bottom_range, top_range)
    }

  }
}


sort_arrhythmic <- function(arrhythmic, rel_melody, note_length) {
  melody <- itembankr::str_mel_to_vector(rel_melody$melody, ",")
  if(arrhythmic) {
    dur_list <- rep(note_length, length(melody)+1)
  } else {
    print('rhythmic..')
    dur_list <- itembankr::str_mel_to_vector(rel_melody$durations, ",")
  }
  list("melody" = melody, "dur_list" = dur_list)
}

sort_sampled_melody <- function(trials, melody_no, state, arrhythmic, rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred, bottom_range, top_range, note_length = 0.5) {
  cat('rel_to_abs_mel_function is ', as.character(substitute(rel_to_abs_mel_function)), '\n')
  rel_melody <- trials[melody_no, ]
  melody <- sort_arrhythmic(arrhythmic, rel_melody, note_length)$melody
  dur_list <- sort_arrhythmic(arrhythmic, rel_melody, note_length)$dur_list
  print('rel_to_abs_mel_function in ss_m')
  print(rel_to_abs_mel_function)
  abs_melody <- rel_to_abs_mel_function(rel_melody = rel_melody$melody, bottom_range = bottom_range, top_range = top_range)
  print('other side?')
  # attach abs mel to meta data
  answer_meta_data <- cbind(rel_melody, data.frame(abs_melody = paste0(abs_melody, collapse = ",")))
  psychTestR::set_global("melody", list("melody" = abs_melody, "dur_list" = dur_list), state)
  psychTestR::set_global("answer_meta_data", rjson::toJSON(answer_meta_data), state)
}


sort_sampled_midi_file <- function(trials, melody_no, clip_stimuli_length, state) {
  abs_melody <- trials[melody_no, "midi_file"]
  if(clip_stimuli_length) {
    start_note <- trials[melody_no, "start"]
    end_note <- trials[melody_no, "end"]
  }
  # transpose..
  psychTestR::set_global("melody", list("midi_file" = abs_melody,
                                        "start_note" = start_note,
                                        "end_note" = end_note), state)
}

check_melody_ok <- function(state, var_name, page_title, max_goes) {
  psychTestR::reactive_page(function(answer, state, ...) {
    number_attempts <- psychTestR::get_global("number_attempts", state)
    attempts_left <- max_goes - number_attempts
    return_correct_attempts_left_page(attempts_left, var_name, number_attempts, page_title)
  })
}


update_play_melody_loop_and_save <- function(state) {
  psychTestR::code_block(function(state, answer, opt, ...) {
    psychTestR::set_global("user_satisfied", answer$user_satisfied, state)
    number_attempts <- psychTestR::get_global("number_attempts", state)
    number_attempts <- number_attempts + 1
    psychTestR::set_global("number_attempts", number_attempts, state)
    psychTestR::save_results_to_disk(complete = FALSE, state, opt)
  })
}


melody_checks <- function(melody, state, stimuli_type = "midi_notes") {
  if(is.null(melody)) {
    melody <- psychTestR::get_global("melody", state)
  }

  if(length(melody) == 1 & is.character(melody) & stimuli_type != "midi_file") {
    melody <- itembankr::str_mel_to_vector(melody, ",")
  }

  if(stimuli_type == "midi_file") {
    start_note <- melody$start_note
    end_note <- melody$end_note
    melody <- melody$midi_file
  } else {
    start_note <- NA
    end_note <- NA
  }

  if(is.list(melody)) {
    dur_list <- melody$dur_list
    melody <- melody$melody
  } else {
    dur_list <- NA
  }

  list("melody" = melody,
       "start_note" = start_note,
       "end_note" = end_note,
       "dur_list" = dur_list)
}


return_correct_attempts_left_page <- function(attempts_left, var_name, number_attempts, page_title = "Play The Melody") {
  if(attempts_left == 0) {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("attempts_remaining_0"))),
                          choices = psychTestR::i18n("Continue"), save_answer = FALSE)
  } else if (attempts_left == 1) {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(psychTestR::i18n("attempts_remaining_1"))),
                          choices = c(psychTestR::i18n("Continue"), psychTestR::i18n("Try_Again")), save_answer = FALSE)
  } else {
    psychTestR::NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = shiny::tags$div(shiny::tags$h2(page_title), shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(paste0(psychTestR::i18n("attempts_remaining_several.1"), " ", attempts_left, " ", psychTestR::i18n("attempts_remaining_several.2")))),
                          choices = c(psychTestR::i18n("Continue"), psychTestR::i18n("Try_Again")), save_answer = FALSE)
  }
}


