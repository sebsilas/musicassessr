midi_or_audio <- function(type, prompt_text, var_name) {
  if (type == "record_audio_page") {
    record_audio_page(page_text = prompt_text,
                      label = var_name,
                      get_answer = get_answer_average_frequency_ff("round"),
                      show_record_button = TRUE,
                      show_aws_controls = FALSE,
                      method = "crepe",
                      button_text = i18n("Record"),
                      stop_button_text = i18n("Stop")
    )
  }
  else {
    reactive_page(function(state, ...) {

      midi_device <- get_global("midi_device", state)

      if(is.null(midi_device)) { shiny::showNotification(i18n("no_midi_device_selected")) }

      record_midi_page(page_text = prompt_text,
                       label = var_name,
                       get_answer = get_answer_midi_note_mode,
                       show_record_button = TRUE,
                       midi_device = midi_device,
                       button_text = i18n("Record"),
                       stop_button_text = i18n("Stop")
      )
    })
  }
}

get_note_until_satisfied_loop <- function(prompt_text, var_name, page_type, button_text = "Record") {

  join(
    # set the user satisfied state to false

    code_block(function(state, ...) {
      set_global("user_satisfied", "No", state)
      set_global(var_name, NA, state)
    }),

    # keep in loop until the participant confirms the note is correct
    while_loop(test = function(state, ...) {
      user_satisfied <- get_global("user_satisfied", state)
      note <- get_global(var_name, state)
      user_satisfied %in% dict_key_to_translations("No") | is.na(note) },
      logic = list(
        # logic page 1, get new note
        midi_or_audio(page_type, prompt_test, var_name),
        # logic page 2, was everything ok with this note?
        reactive_page(function(answer, state, ...) {
          note <- answer[[1]]
          if(is.na(note)) {
            one_button_page(i18n("nothing_entered"))
          } else {
            set_global(var_name, note, state)
            present_stimuli(stimuli = note,
                            stimuli_type = "midi_notes",
                            display_modality = "both",
                            page_text = i18n("correct_note_message"),
                            page_type = "NAFC_page",
                            choices = c(i18n("Yes"), i18n("No")),
                            label = var_name,
                            play_button_text = i18n("Play")
                            )
          }
        }),
        code_block(function(state, answer, ...) {
          set_global("user_satisfied", answer, state)
        })
      )
    )
  )
}


get_instrument_range_pages <- function(type) {
  # a short multi-page protocol to get the user's frequency range

  if (type == "record_audio_page") {
    join(
      get_note_until_satisfied_loop(prompt_text = i18n("get_range_low_note"), var_name = "bottom_range", page_type = "record_audio_page"),
      get_note_until_satisfied_loop(prompt_text = i18n("get_range_high_note"), var_name = "top_range", page_type = "record_audio_page"),
      reactive_page(function(state, ...) {
        lowest_user_note <- get_global("bottom_range", state)
        highest_user_note <- get_global("top_range", state)
        range <- c(lowest_user_note, highest_user_note)
        present_stimuli(stimuli = range, stimuli_type = "midi_notes",
                        display_modality = "both",
                        page_text = i18n("your_range_message"),
                        page_type = "one_button_page",
                        button_text = i18n("Next")
                        )
      })
    )
  }
  else {
    join(
      get_note_until_satisfied_loop(prompt_text = i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page"),
      get_note_until_satisfied_loop(prompt_text = i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page"),
      reactive_page(function(state, ...) {
        lowest_user_note <- get_global("bottom_range", state)
        highest_user_note <- get_global("top_range", state)
        range <- c(lowest_user_note, highest_user_note)
        present_stimuli(stimuli = range, stimuli_type = "midi_notes", page_type = "one_button_page",
                        display_modality = "both", page_text = i18n("your_range_message"), button_text = i18n("Next"))
      })
    )
  }

}


melody_checks <- function(melody, state) {
  if(is.null(melody)) {
    melody <- get_global("melody", state)
  }
  if(length(melody) == 1 & is.character(melody)) {
    melody <- str_mel_to_vector(melody, ",")
  }
  melody
}


return_correct_attempts_left_page <- function(attempts_left, var_name, number_attempts) {
  if(attempts_left == 0) {
    NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = tags$div(tags$p(i18n("attempts_remaining_0"))),
              choices = i18n("Continue"), save_answer = FALSE)
  }
  else if (attempts_left == 1) {
    NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = tags$div(tags$p(i18n("happy_with_response_message")), tags$p(i18n("attempts_remaining_1"))),
              choices = c(i18n("Continue"), i18n("Try_Again")), save_answer = FALSE)
  }
  else {
    NAFC_page(label = paste0(var_name,"_attempt_", number_attempts, "_choice"), prompt = tags$div(tags$p(i18n("happy_with_response_message")), tags$p(paste0(i18n("attempts_remaining_several.1"), " ", attempts_left, " ", i18n("attempts_remaining_several.2")))),
              choices = c(i18n("Continue"), i18n("Try_Again")), save_answer = FALSE)
  }
}

play_melody_until_satisfied_loop <- function(melody = NULL, melody_no = "x", var_name = "melody", max_goes = 3,
                                             page_type = "record_audio_page", page_title = " ", page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                             answer_meta_data = " ", get_answer = get_answer_store_async) {


  c(
    # set the user satisfied state to false

    code_block(function(state, ...) {
      print('play_melody_until_satisfied_loop')
      # repeat melody logic stuff
      set_global("user_satisfied", "Try Again", state)
      set_global("number_attempts", 1, state)

      # sample melody for this trial
      if(is.null(melody)) {
        print('sample melody because null')

        inst <- get_global("inst", state)
        bottom_range <- get_global("bottom_range", state)
        top_range <- get_global("top_range", state)
        trial_chars <- get_global("trials", state)
        trial_char <- get_trial_characteristics(trial_df = trial_chars, trial_no = melody_no)
        sample <- sample_melody_in_key(inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$difficulty, length = trial_char$melody_length)
        melody <- sample[[1]]
        answer_meta_data <- sample[[2]]
        answer_meta_data <- dplyr::select(answer_meta_data, -type)
        answer_meta_data <- cbind(answer_meta_data,
                                  data.frame(abs_melody = paste0(melody, collapse = ","))
                                  )

        set_global("melody", melody, state)
        set_global("answer_meta_data", toJSON(answer_meta_data), state)
      }

    }),

    # keep in loop until the participant confirms the note is correct
    while_loop(test = function(state, ...) {
      number_attempts <- get_global("number_attempts", state)
      user_answer <- get_global("user_satisfied", state)
      user_wants_to_play_again <- user_answer %in% dict_key_to_translations("Try_Again")
      user_wants_to_play_again
      },
      logic = list(
        reactive_page(function(state, ...) {

          number_attempts <- get_global("number_attempts", state)

          melody <- melody_checks(melody, state)

          answer_meta_data <- get_global("answer_meta_data", state)

          if(page_type == "record_midi_page") {

            midi_device <- get_global("midi_device", state)
            if(is.null(midi_device)) { shiny::showNotification(i18n("no_midi_device_selected")) }

            present_stimuli(stimuli = melody,
                            stimuli_type = "midi_notes",
                            display_modality = "auditory",
                            page_title = " ",
                            page_text = page_text,
                            page_type = page_type,
                            answer_meta_data = answer_meta_data,
                            get_answer = get_answer,
                            midi_device = midi_device,
                            page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                            play_button_text = i18n("Play")
                            )
          } else {

          # page 1, play melody
          present_stimuli(stimuli = melody,
                          stimuli_type = "midi_notes",
                          display_modality = "auditory",
                          page_title = " ",
                          page_text = page_text,
                          page_type = "record_audio_page",
                          record_audio_method = "aws_pyin",
                          answer_meta_data = answer_meta_data,
                          get_answer = get_answer,
                          page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                          button_text = i18n("Record"),
                          play_button_text = i18n("Play")
                          )
          }

        }),

        # logic page 2, was the user ok with this response?
        reactive_page(function(answer, state, ...) {
          number_attempts <- get_global("number_attempts", state)
          attempts_left <- max_goes - number_attempts
          return_correct_attempts_left_page(attempts_left, var_name, number_attempts)
        }),

        code_block(function(state, answer, opt, ...) {
          set_global("user_satisfied", answer, state)
          number_attempts <- get_global("number_attempts", state)
          number_attempts <- number_attempts + 1
          set_global("number_attempts", number_attempts, state)
          save_results_to_disk(complete = FALSE, state, opt)
        })
      )
    ) # end while_loop
  ) # end join
}


build_multi_page_play_melody_until_satisfied_loop <- function(n_items, var_name = "melody",
                                                              page_type, max_goes = 3,
                                                              page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                                              get_answer = get_answer_null
                                                              ) {
  # items should be a dataframe
  # this will return a sequence of test items
  unlist(lapply(1:n_items, function(melody_no) {
    play_melody_until_satisfied_loop(melody_no = melody_no,
                                     var_name = var_name,
                                     max_goes = max_goes,
                                     page_type = page_type,
                                     page_text = page_text,
                                     get_answer = get_answer
                                     )
  }))
}



musical_test <- function(test_name = NULL,
                         item_bank = list("rhythmic" = berkowitz.musicxml, "arrhythmic" = berkowitz.rds.abs),
                         no_items = 10,
                         display_modality = "visual",
                         response_type = "record_midi_page",
                         feedback = TRUE) {

  midi.response.pages <- create_midi_pages(no_items)
  audio.response.pages <- create_audio_pages(no_items)

  if (response_type == "user_selected") {

    tl <- psychTestR::join(

      midi_vs_audio_select_page(),

      # if they selected MIDI, show the pages as MIDI otherwise audio
      conditional(check.response.type.midi, midi.response.pages),

      conditional(check.response.type.audio, audio.response.pages)
    )

  }

  else if (response_type == "record_midi_page") {
    # i.e midi test
    tl <- midi.response.pages
  }

  else {
    # i.e audio test
    tl <- audio.response.pages
  }


  if (feedback) {
    tl <- insert.every.other.pos.in.list(tl, display_previous_answer_music_notation_pitch_class()) # or 2?
  }



  tl


}


