

#' A page builder for creating a specified number of play_melody_loops
#'
#' @param presampled_items
#' @param n_items
#' @param var_name
#' @param stimuli_type
#' @param page_type
#' @param max_goes
#' @param page_title
#' @param page_text
#' @param get_answer
#' @param rel_to_abs_mel_function
#' @param start_from_trial_no
#' @param clip_stimuli_length
#' @param arrhythmic
#' @param example
#' @param feedback
#' @param sound
#' @param get_trial_characteristics_function
#'
#' @return
#' @export
#'
#' @examples
multi_page_play_melody_loop <- function(presampled_items = NULL, n_items, var_name = "melody", stimuli_type = "midi_notes",
                                        page_type = "record_audio_page", max_goes = 3,
                                        page_title = psychTestR::i18n("copy_melody_title"),
                                        page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                        get_answer = get_answer_save_aws_key, rel_to_abs_mel_function = NULL,
                                        start_from_trial_no = 1, clip_stimuli_length = FALSE,
                                        arrhythmic = FALSE, example = FALSE, feedback = FALSE, sound = "piano",
                                        get_trial_characteristics_function = NULL) {

  if(is.null(presampled_items)) {
    # items should be a dataframe
    # this will return a sequence of test items
    items <- lapply(start_from_trial_no:n_items, function(melody_no) {
      play_melody_loop(melody_no = melody_no,
                       var_name = var_name,
                       max_goes = max_goes,
                       page_type = page_type,
                       page_title = page_title,
                       page_text = page_text,
                       get_answer = get_answer,
                       stimuli_type = stimuli_type,
                       rel_to_abs_mel_function = rel_to_abs_mel_function,
                       clip_stimuli_length = clip_stimuli_length,
                       arrhythmic = arrhythmic,
                       example = example,
                       sound = sound,
                       get_trial_characteristics_function = get_trial_characteristics_function)
      })

      items

    } else {

      items <- lapply(1:nrow(presampled_items), function(x) {
        melody <- presampled_items %>% dplyr::slice(x)
        play_melody_loop(melody = melody,
                         var_name = var_name,
                         max_goes = max_goes,
                         page_type = page_type,
                         page_title = page_title,
                         page_text = page_text,
                         get_answer = get_answer,
                         stimuli_type = stimuli_type,
                         rel_to_abs_mel_function = rel_to_abs_mel_function,
                         clip_stimuli_length = clip_stimuli_length,
                         arrhythmic = arrhythmic,
                         example = example,
                         sound = sound)   })

    }


  items <- add_feedback(items, feedback, after = 2) # a play_melody_loop is 3 pages long
  items

}




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
#' @param durations
#' @param arrhythmic
#' @param note_length
#' @param play_button_text
#' @param example
#' @param sound
#' @param reactive_stimuli
#' @param get_trial_characteristics_function
#'
#' @return
#' @export
#'
#' @examples
play_melody_loop <- function(melody = NULL, melody_no = "x", var_name = "melody", stimuli_type = "midi_notes", max_goes = 3,
                             page_type = "record_audio_page", page_title = "Copy The Melody", page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                             answer_meta_data = " ", get_answer = get_answer_pyin,
                             rel_to_abs_mel_function = NULL, clip_stimuli_length = FALSE,
                             start_note = 1, end_note = "end", durations = 'null', arrhythmic = FALSE, note_length = 0.5,
                             play_button_text = psychTestR::i18n("Play"), example = FALSE, sound = "piano",
                             reactive_stimuli = NULL, get_trial_characteristics_function = NULL) {

  save_answer <- example_save(example)

  c(
    # set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      # repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)
      psychTestR::set_global("max_goes", max_goes, state)
      psychTestR::set_global("attempts_left", max_goes, state)
      # grab sampled melody for this trial (if one not specified)
      grab_sampled_melody(melody, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function = rel_to_abs_mel_function, get_trial_characteristics_function = get_trial_characteristics_function)
    }),

    # keep in loop until the participant confirms they are happy with their entry
    psychTestR::while_loop(test = function(state, ...) {
      print('whiwelw')
      number_attempts <- psychTestR::get_global("number_attempts", state)
      user_answer <- psychTestR::get_global("user_satisfied", state)
      print(number_attempts)
      print(user_answer)
      print(user_answer %in% dict_key_to_translations("Try_Again"))
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
                     durations = durations,
                     state = state,
                     melody_no = melody_no,
                     var_name = var_name,
                     sound = sound,
                     reactive_stimuli = reactive_stimuli,
                     rel_to_abs_mel_function = rel_to_abs_mel_function),

      # update and see how to proceed
      update_play_melody_loop_and_save(state, max_goes)
    )
    ) # end psychTestR::while_loop
  ) # end join
}

present_melody <- function(stimuli, stimuli_type, display_modality, page_title, page_text,
                           page_type, record_audio_method, answer_meta_data, get_answer,
                           save_answer, page_label, button_text, play_button_text, start_note,
                           end_note, durations, state, melody_no, var_name, sound = "piano",
                           reactive_stimuli = NULL, rel_to_abs_mel_function = NULL, hideOnPlay = FALSE, ...) {


  if(!is.null(rel_to_abs_mel_function) & stimuli_type != "audio_WJD") {
    # then this presumes that the melody was transposed at test time, and therefore, should be grabbed
    # via get_local/global
    stimuli <- NULL
  }

  psychTestR::reactive_page(function(state, ...) {

    # grab attempts left etc.
    number_attempts <- psychTestR::get_global("number_attempts", state)
    max_goes <- psychTestR::get_global("max_goes", state)
    attempts_left <- psychTestR::get_global("attempts_left", state) - 1
    answer_meta_data <- psychTestR::get_global("answer_meta_data", state)

    # midi checks
    midi_device <- midi_device_check(page_type, state)

    if(stimuli_type == "audio_WJD") {
      stimuli <- present_stimuli_audio_WJD(stimuli)
      stimuli_type <- "audio"
      hideOnPlay <- TRUE
    }

    # grab vars
    melody_checks <- melody_checks(stimuli, state, stimuli_type)

    melody <- melody_checks$melody
    start_note <- melody_checks$start_note
    end_note <- melody_checks$end_note
    durations <- melody_checks$durations

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
                    durations = durations,
                    user_rating = TRUE,
                    happy_with_response = TRUE,
                    attempts_left = attempts_left,
                    sound = sound,
                    hideOnPlay = hideOnPlay)

  })
}


example_save <- function(example) {
  ifelse(example, FALSE, TRUE)
}

grab_sampled_melody <- function(melody_row, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function = NULL,
                                note_length = 0.5, get_trial_characteristics_function = NULL, ...) {

  # not all trials will need the range, inst. etc but grab it here anyway
  bottom_range <- psychTestR::get_global("bottom_range", state)
  top_range <- psychTestR::get_global("top_range", state)
  range <- psychTestR::get_global("range", state)
  inst <- psychTestR::get_global("inst", state)

  if(stimuli_type == "midi_file") {
    sort_sampled_midi_file(trials, melody_no, clip_stimuli_length, state)
  } else {
    # has melody been specified directly, or sampled at test time?
    if(is.null(melody_row)) {
      # assume melodies sampled at test time and stored in global object
      if(is.null(get_trial_characteristics_function)) {
        rel_melody <- grab_melody_from_state(melody_row, var_name, melody_no, state)$rel_melody
        melody_row <- grab_melody_from_state(melody_row, var_name, melody_no, state)$melody_row
      } else {
        trials <- psychTestR::get_global(var_name, state)
        trial_char <- get_trial_characteristics_function(trial_df = trials, trial_no = melody_no)
        melody_row <- sample_melody_in_key(inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$key_difficulty, length = trial_char$melody_length)
        melody_row <- cbind(melody_row, data.frame("key_difficulty"= trial_char$key_difficulty))
        # not fully abstracted from PBET setup, yet
        abs_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(abs_melody))
        rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
        rel_to_abs_mel_function <- NULL
      }

    } else {
        transpose <- transposition_check(melody_row)
        rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
      }
    }

    durations <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(durations))

    # does the melody need to be rhythmic or arrhythmic?
    melody <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$melody
    durations <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$durations

    # does the melody need putting into a certain pitch range?
  if(is.null(rel_to_abs_mel_function)) {
    if(is.data.frame(abs_melody)) {
      abs_melody <- melody %>% dplyr::pull(abs_melody)
    }
  } else {
    # then assume that the melody is in relative format and fit it into a key, based on a rel_to_abs_mel_functio
    abs_melody <- rel_to_abs_mel_function(rel_melody = rel_melody, bottom_range = bottom_range, top_range = top_range, range = range, transpose = transpose)
  }


  # attach generated absolute melody to meta data
  answer_meta_data <- cbind(melody_row,
                            tibble::tibble(abs_melody = paste0(abs_melody, collapse = ",")))

  # set the melody to be used
  psychTestR::set_global("melody", list("melody" = abs_melody,
                                        "durations" = durations), state)
  psychTestR::set_global("answer_meta_data", rjson::toJSON(answer_meta_data), state)

}


transposition_check <- function(melody_row) {
  if("transpose" %in% names(melody_row)) {
    transpose <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(transpose))
  }
}

grab_melody_from_state <- function(melody_row, var_name, melody_no, state) {
  # assume melodies sampled at test time and stored in global object
  trials <- psychTestR::get_global(var_name, state)
  melody_row <- trials %>% dplyr::slice(melody_no)
  rel_melody <- melody_row %>% dplyr::pull(melody) %>% itembankr::str_mel_to_vector()
  list("rel_melody" = rel_melody,
       "melody_row" = melody_row)
}

sort_arrhythmic <- function(arrhythmic, rel_melody, durations, note_length) {

  if(arrhythmic) {
    durations <- rep(note_length, length(rel_melody)+1)
  }
  list("melody" = rel_melody,
       "durations" = durations)
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

update_play_melody_loop_and_save <- function(state, max_goes) {
  psychTestR::code_block(function(state, answer, opt, ...) {
    psychTestR::set_global("user_satisfied", answer$user_satisfied, state)
    number_attempts <- psychTestR::get_global("number_attempts", state)
    attempts_left <- max_goes - number_attempts
    psychTestR::set_global("attempts_left", attempts_left, state)
    number_attempts <- number_attempts + 1
    psychTestR::set_global("number_attempts", number_attempts, state)
    psychTestR::save_results_to_disk(complete = FALSE, state, opt)
  })
}


melody_checks <- function(melody, state, stimuli_type = "midi_notes") {

  if(is.null(melody)) {
    melody <- psychTestR::get_global("melody", state)
  }

  if(length(melody) == 1 & is.character(melody) &
     stimuli_type != "midi_file" & stimuli_type != "audio") {
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
    durations <- as.vector(unlist(melody$durations))
    melody <- as.vector(unlist(melody$melody))
  } else {
    durations <- NA
  }

  list("melody" = melody,
       "start_note" = start_note,
       "end_note" = end_note,
       "durations" = durations)
}

midi_device_check <- function(page_type, state) {
  if(page_type == "record_midi_page") {
    midi_device <- psychTestR::get_global("midi_device", state)
    if(is.null(midi_device)) {
      shiny::showNotification(psychTestR::i18n("no_midi_device_selected"))
    } else {
      return(midi_device)
    }
  }
}

