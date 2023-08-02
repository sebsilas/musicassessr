

#' A page builder for creating a specified number of play_melody_loops
#'
#' @param item_bank
#' @param presampled_items
#' @param num_items
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
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param start_hidden
#' @param sound_only_first_melody_note
#' @param show_sheet_music
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param melody_paradigm
#' @param singing_trials
#' @param phase
#'
#' @return
#' @export
#'
#' @examples
multi_page_play_melody_loop <- function(item_bank = NULL,
                                        presampled_items = NULL,
                                        num_items = NULL, # Can be null if presampled
                                        var_name = "melody",
                                        stimuli_type = "midi_notes",
                                        page_type = "record_audio_page",
                                        max_goes = 4L,
                                        page_title = psychTestR::i18n("copy_melody_title"),
                                        page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                        get_answer = get_answer_pyin_melodic_production,
                                        rel_to_abs_mel_function = NULL,
                                        start_from_trial_no = 1L,
                                        clip_stimuli_length = FALSE,
                                        arrhythmic = FALSE,
                                        example = FALSE,
                                        feedback = FALSE,
                                        sound = "piano",
                                        get_trial_characteristics_function = NULL,
                                        max_goes_forced = FALSE,
                                        display_modality = "auditory",
                                        show_progress = TRUE,
                                        start_hidden = FALSE,
                                        sound_only_first_melody_note = FALSE,
                                        show_sheet_music = FALSE,
                                        sheet_music_id = 'sheet_music',
                                        give_first_melody_note = FALSE,
                                        get_similarity_to_previous_melody = FALSE,
                                        volume_meter = FALSE,
                                        volume_meter_type = 'default',
                                        melody_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                                        singing_trials = TRUE,
                                        phase = c('test', 'learn', 'review', 'example')) {

  melody_paradigm <- match.arg(melody_paradigm)
  phase <- match.arg(phase)


  stopifnot(is.null.or(item_bank, tibble::is_tibble),
            is.null.or(presampled_items, is.data.frame),
            is.null.or(num_items, is.scalar.numeric),
            assertthat::is.string(var_name),
            assertthat::is.string(stimuli_type),
            assertthat::is.string(page_type),
            is.scalar.numeric(max_goes),
            is(page_title, "html") || is.character(page_title),
            assertthat::is.string(page_text),
            is.function(get_answer),
            is.null.or(rel_to_abs_mel_function, is.function),
            is.scalar.numeric(start_from_trial_no),
            is.scalar.logical(clip_stimuli_length),
            is.scalar.logical(arrhythmic),
            is.scalar.logical(example),
            is.function(feedback) | is.scalar.logical(feedback),
            assertthat::is.string(sound),
            is.null.or(get_trial_characteristics_function, is.function),
            is.scalar.logical(max_goes_forced),
            assertthat::is.string(display_modality),
            is.scalar.logical(show_progress),
            is.scalar.logical(start_hidden),
            is.scalar.logical(sound_only_first_melody_note),
            is.scalar.logical(show_sheet_music),
            assertthat::is.string(sheet_music_id),
            is.scalar.logical(give_first_melody_note),
            is.scalar.logical(get_similarity_to_previous_melody),
            is.scalar.logical(volume_meter),
            assertthat::is.string(volume_meter_type),
            melody_paradigm %in% c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
            is.scalar.logical(singing_trials),
            phase %in% c('learn', 'test', 'review', 'example'))


  if(is.null(presampled_items)) {
    # Items should be a data frame
    # This will return a sequence of test items
    items <- purrr::map(start_from_trial_no:num_items, function(melody_no) {

      page <- play_melody_loop(melody_no = melody_no,
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
                               get_trial_characteristics_function = get_trial_characteristics_function,
                               max_goes_forced = max_goes_forced,
                               item_bank = item_bank,
                               display_modality = display_modality,
                               total_no_melodies = num_items,
                               show_progress = show_progress,
                               start_hidden = start_hidden,
                               sound_only_first_melody_note = sound_only_first_melody_note,
                               show_sheet_music = show_sheet_music,
                               sheet_music_id = sheet_music_id,
                               give_first_melody_note = give_first_melody_note,
                               get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                               volume_meter = volume_meter,
                               volume_meter_type = volume_meter_type,
                               singing_trial = singing_trials,
                               phase = phase,
                               melody_paradigm = melody_paradigm)

      if (melody_paradigm == "sing_melody_first") {

        sing_page <- play_melody_loop(melody_no = melody_no,
                                       var_name = var_name,
                                       max_goes = max_goes,
                                       page_type = page_type,
                                       page_title = psychTestR::i18n("Sing_the_Melody"),
                                       page_text = psychTestR::i18n("sing_melody_page_text"),
                                       get_answer = get_answer,
                                       stimuli_type = stimuli_type,
                                       rel_to_abs_mel_function = rel_to_abs_mel_function,
                                       clip_stimuli_length = clip_stimuli_length,
                                       arrhythmic = arrhythmic,
                                       example = example,
                                       sound = sound,
                                       get_trial_characteristics_function = get_trial_characteristics_function,
                                       max_goes_forced = max_goes_forced,
                                       item_bank = item_bank,
                                       display_modality = display_modality,
                                       total_no_melodies = num_items,
                                       show_progress = show_progress,
                                       start_hidden = start_hidden,
                                       sound_only_first_melody_note = sound_only_first_melody_note,
                                       show_sheet_music = show_sheet_music,
                                       sheet_music_id = sheet_music_id,
                                       give_first_melody_note = give_first_melody_note,
                                       get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                                       volume_meter = volume_meter,
                                       volume_meter_type = volume_meter_type,
                                       singing_trial = TRUE,
                                       phase = phase,
                                       melody_paradigm = melody_paradigm)

        sing_then_play_pages <- psychTestR::join(sing_page, page)

        return(sing_then_play_pages)

      } else {
        return(page)
      }

    })

    items

  } else {

    items <- purrr::map(1:nrow(presampled_items), function(x) {
      melody <- presampled_items %>% dplyr::slice(x)

      if (melody_paradigm == "sing_melody_first") {

        sing_page <- play_melody_loop(melody_no = melody_no,
                                      var_name = var_name,
                                      max_goes = max_goes,
                                      page_type = page_type,
                                      page_title = psychTestR::i18n("Sing_the_Melody"),
                                      page_text = psychTestR::i18n("sing_melody_page_text"),
                                      get_answer = get_answer,
                                      stimuli_type = stimuli_type,
                                      rel_to_abs_mel_function = rel_to_abs_mel_function,
                                      clip_stimuli_length = clip_stimuli_length,
                                      arrhythmic = arrhythmic,
                                      example = example,
                                      sound = sound,
                                      get_trial_characteristics_function = get_trial_characteristics_function,
                                      max_goes_forced = max_goes_forced,
                                      item_bank = item_bank,
                                      display_modality = display_modality,
                                      total_no_melodies = num_items,
                                      show_progress = show_progress,
                                      start_hidden = start_hidden,
                                      sound_only_first_melody_note = sound_only_first_melody_note,
                                      show_sheet_music = show_sheet_music,
                                      sheet_music_id = sheet_music_id,
                                      give_first_melody_note = give_first_melody_note,
                                      get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                                      volume_meter = volume_meter,
                                      volume_meter_type = volume_meter_type,
                                      singing_trial = TRUE,
                                      phase = phase,
                                      melody_paradigm = melody_paradigm)

        sing_then_play_pages <- psychTestR::join(sing_page, page)

        return(sing_then_play_pages)

      } else {
        return(page)
      }
    })

  }

  # Add feedback
  items <- add_feedback(items, feedback, after = 2) # A play_melody_loop is 3 pages long

  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("presampled_items", presampled_items, state)
    }),
    items
  )

}





#' Create a psychTestR test loop for having several attempts at playing back a melody.
#'
#' @param item_bank
#' @param melody
#' @param melody_no
#' @param var_name
#' @param stimuli_type
#' @param max_goes
#' @param max_goes_forced
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
#' @param display_modality
#' @param total_no_melodies
#' @param show_progress
#' @param start_hidden
#' @param sound_only_first_melody_note
#' @param show_sheet_music
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param psychTestRCAT
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param singing_trial Boolean: if TRUE, the trial is defined as singing-based, otherwise instrument-based.
#' @param phase Can be one of 'learn', 'test', 'review' or 'example'
#' @param melody_paradigm
#'
#' @return
#' @export
#'
#' @examples
play_melody_loop <- function(item_bank = NULL,
                             melody = NULL,
                             melody_no = 0,
                             var_name = "melody",
                             stimuli_type = "midi_notes",
                             max_goes = 4L,
                             max_goes_forced = FALSE,
                             page_type = "record_audio_page",
                             page_title = "Copy The Melody",
                             page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                             answer_meta_data = data.frame(),
                             get_answer = get_answer_pyin_melodic_production,
                             rel_to_abs_mel_function = NULL,
                             clip_stimuli_length = FALSE,
                             start_note = 1L,
                             end_note = "end",
                             durations = 'null',
                             arrhythmic = FALSE,
                             note_length = 0.5,
                             play_button_text = psychTestR::i18n("Play"),
                             example = FALSE,
                             sound = "piano",
                             reactive_stimuli = NULL,
                             get_trial_characteristics_function = NULL,
                             display_modality = "auditory",
                             total_no_melodies = 0,
                             show_progress = TRUE,
                             start_hidden = FALSE,
                             sound_only_first_melody_note = FALSE,
                             show_sheet_music = FALSE,
                             sheet_music_id = 'sheet_music',
                             give_first_melody_note = FALSE,
                             psychTestRCAT = FALSE,
                             melody_row = NULL,
                             get_similarity_to_previous_melody = FALSE,
                             volume_meter = FALSE,
                             volume_meter_type = 'default',
                             singing_trial = TRUE,
                             phase = c('test', 'learn', 'review', 'example'),
                             melody_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality')) {


  save_answer <- example_save(example)
  phase <- match.arg(phase)

  psychTestR::join(
    # Set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      # Repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)
      psychTestR::set_global("max_goes", max_goes, state)
      psychTestR::set_global("attempts_left", max_goes, state)
      # Grab sampled melody for this trial (if one not specified)
      if(is.null(melody)) grab_sampled_melody(item_bank, melody_row, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function, note_length, get_trial_characteristics_function, psychTestRCAT, get_similarity_to_previous_melody, phase)
    }),

    # Keep in loop until the participant confirms they are happy with their entry
    psychTestR::while_loop(test = function(state, ...) {
      number_attempts <- psychTestR::get_global("number_attempts", state)
      user_answer <- psychTestR::get_global("user_satisfied", state)
      user_wants_to_play_again <- user_answer %in% dict_key_to_translations("Try_Again")
    },
    logic = list(

      # Present the melody
      present_melody(stimuli = melody,
                     stimuli_type = stimuli_type,
                     display_modality = display_modality,
                     page_title = page_title,
                     page_text = page_text,
                     page_type = page_type,
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
                     rel_to_abs_mel_function = rel_to_abs_mel_function,
                     max_goes_forced = max_goes_forced,
                     total_no_melodies = total_no_melodies,
                     show_progress = show_progress,
                     start_hidden = start_hidden,
                     sound_only_first_melody_note = sound_only_first_melody_note,
                     show_sheet_music = show_sheet_music,
                     sheet_music_id = sheet_music_id,
                     give_first_melody_note = give_first_melody_note,
                     arrhythmic = arrhythmic,
                     note_length = note_length,
                     psychTestRCAT = psychTestRCAT,
                     volume_meter = volume_meter,
                     volume_meter_type = volume_meter_type,
                     singing_trial = singing_trial,
                     phase = phase,
                     melody_paradigm = melody_paradigm),

      # Update and see how to proceed
      update_play_melody_loop_and_save(state, max_goes)
    )
    ) # End psychTestR::while_loop
  ) # End join
}

present_melody <- function(stimuli,
                           stimuli_type,
                           display_modality,
                           page_title,
                           page_text,
                           max_goes_forced = FALSE,
                           page_type,
                           answer_meta_data = data.frame(),
                           get_answer,
                           save_answer,
                           page_label,
                           button_text,
                           play_button_text,
                           start_note = 1L,
                           end_note,
                           durations,
                           state,
                           melody_no,
                           var_name,
                           sound = "piano",
                           reactive_stimuli = NULL,
                           rel_to_abs_mel_function = NULL,
                           hideOnPlay = FALSE,
                           show_progress = TRUE,
                           total_no_melodies = 0,
                           start_hidden = FALSE,
                           sound_only_first_melody_note = FALSE,
                           show_sheet_music = FALSE,
                           sheet_music_id = 'sheet_music',
                           give_first_melody_note = FALSE,
                           arrhythmic = FALSE,
                           note_length = 0.5,
                           psychTestRCAT = FALSE,
                           volume_meter = FALSE,
                           volume_meter_type = 'default',
                           singing_trial = TRUE,
                           melody_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                           user_rating = FALSE,
                           happy_with_response = FALSE, ...) {

  if(!is.null(rel_to_abs_mel_function) & stimuli_type != "audio_WJD") {
    # then this presumes that the melody was transposed at test time, and therefore, should be grabbed
    # via get_local/global
    stimuli <- NULL
  }

  psychTestR::reactive_page(function(state, ...) {

    # grab attempts left etc.
    number_attempts <- psychTestR::get_global("number_attempts", state)
    max_goes <- psychTestR::get_global("max_goes", state)
    attempts_left <- psychTestR::get_global("attempts_left", state) - 1L

    if(length(answer_meta_data) < 1) {
      answer_meta_data <- psychTestR::get_global("answer_meta_data", state)
    }

    transpose_first_melody_note <- psychTestR::get_global("transpose_first_melody_note", state)
    clef <- psychTestR::get_global("clef", state)

    # MIDI checks
    midi_device <- midi_device_check(page_type, state)

    if(stimuli_type == "audio_WJD") {
      stimuli <- present_stimuli_audio_WJD(stimuli)
      stimuli_type <- "audio"
      hideOnPlay <- TRUE
    }

    # Grab vars
    melody_checks <- melody_checks(stimuli, state, stimuli_type, arrhythmic, note_length)

    if(psychTestRCAT) {
      melody_no <- psychTestR::get_local("item", state) %>% psychTestRCAT::get_item_number()
    }

    psychTestR::set_global("trial_time_started", Sys.time(), state)
    psychTestR::set_global("singing_trial", singing_trial, state)

    # Use a display_modality created at test time, if appropriate
    display_modality <- if(is.null(melody_checks$display_modality)) display_modality else melody_checks$display_modality

    # Present the stimulus
    present_stimuli(stimuli = melody_checks$melody,
                    stimuli_type = stimuli_type,
                    display_modality = display_modality,
                    page_title = page_title,
                    page_text = page_text,
                    page_type = page_type,
                    answer_meta_data = answer_meta_data,
                    get_answer = get_answer,
                    save_answer = save_answer,
                    midi_device = midi_device,
                    page_label = paste0(var_name,"_", melody_no, "_attempt_", number_attempts),
                    play_button_text = play_button_text,
                    start_note = melody_checks$start_note,
                    end_note = melody_checks$end_note,
                    durations = melody_checks$durations,
                    user_rating = user_rating,
                    happy_with_response = happy_with_response,
                    attempts_left = attempts_left,
                    sound = sound,
                    hideOnPlay = hideOnPlay,
                    max_goes = max_goes,
                    max_goes_forced = max_goes_forced,
                    transpose_first_melody_note = transpose_first_melody_note,
                    clef = clef,
                    melody_no = melody_no,
                    show_progress = show_progress,
                    total_no_melodies = total_no_melodies,
                    start_hidden = if(melody_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual") TRUE else start_hidden,
                    sound_only_first_melody_note = sound_only_first_melody_note,
                    show_sheet_music = if(melody_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual") TRUE else show_sheet_music,
                    sheet_music_id = sheet_music_id,
                    give_first_melody_note = if(melody_paradigm == 'learn_phase_visual_display_modality') FALSE else give_first_melody_note,
                    volume_meter = volume_meter,
                    volume_meter_type = volume_meter_type,
                    show_sheet_music_after_record = melody_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual")

  })
}


example_save <- function(example) {
  ifelse(example, FALSE, TRUE)
}

grab_sampled_melody <- function(item_bank = NULL,
                                melody_row = NULL,
                                var_name,
                                stimuli_type,
                                state,
                                melody_no,
                                arrhythmic,
                                rel_to_abs_mel_function = NULL,
                                note_length = 0.5,
                                get_trial_characteristics_function = NULL,
                                psychTestRCAT = FALSE,
                                get_similarity_to_previous_melody = FALSE,
                                phase = c('test', 'learn', 'review', 'example'),
                                display_modality = c('auditory', 'visual', 'both'), ...) {


  logging::loginfo("Grab sampled melody")
  display_modality <- match.arg(display_modality)


  # Not all trial specifications need a range, instrument. etc but grab it here anyway
  bottom_range <- psychTestR::get_global("bottom_range", state)
  top_range <- psychTestR::get_global("top_range", state)
  range <- psychTestR::get_global("range", state)
  inst <- psychTestR::get_global("inst", state)

  if(stimuli_type == "midi_file") {
    sort_sampled_midi_file(trials, melody_no, clip_stimuli_length, state)
  } else {
    # Has melody been specified directly, or sampled at test time?
    if(is.null(melody_row)) {

      # assume melodies sampled at test time and stored in global object
      if(is.null(get_trial_characteristics_function)) {

        melody_from_state <- grab_melody_from_state(var_name, melody_no, state, psychTestRCAT, rel_to_abs_mel_function)
        rel_melody <- melody_from_state$rel_melody
        melody_row <- melody_from_state$melody_row
        abs_melody <- melody_from_state$abs_melody
        item_number <- melody_from_state$item_number

        if(!is.na(item_number)) {
          melody_no <- item_number
        }

      } else {

        trials <- psychTestR::get_global(var_name, state)
        trial_char <- get_trial_characteristics_function(trial_df = trials, trial_no = melody_no)
        melody_row <- sample_melody_in_key(item_bank, inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$key_difficulty, length = trial_char$melody_length)
        melody_row <- cbind(melody_row,
                            tibble::tibble(key_difficulty = trial_char$key_difficulty,
                                           display_modality = trial_char$display_modality)
                            )
        # Not fully abstracted from PBET setup, yet
        abs_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(abs_melody))
        rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
        rel_to_abs_mel_function <- NULL
        display_modality <- trial_char$display_modality
        psychTestR::set_global('display_modality', display_modality, state)
      }

    } else {
      transpose <- transposition_check(melody_row)
      rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
    }
  }

  # Sort durations if rhythmic.
  if(arrhythmic) {
    durations <- NA
  } else {
    durations <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(durations))
  }

  # Does the melody need to be rhythmic or arrhythmic?
  melody <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$melody
  durations <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$durations

  # Does the melody need putting into a certain pitch range...?
  if(is.null(rel_to_abs_mel_function)) {
    if(is.data.frame(abs_melody)) {
      abs_melody <- melody %>% dplyr::pull(abs_melody)
    }
  } else {
    # ...then assume that the melody is in relative format and fit it into a key, based on a rel_to_abs_mel_functionw
    abs_melody <- rel_to_abs_mel_function(rel_melody = rel_melody, bottom_range = bottom_range, top_range = top_range, range = range, transpose = transpose)
  }

  # Get similarity to previous melody
  similarity_to_previous_melody <- get_similarity_to_previous_melody(get_similarity_to_previous_melody, melody_no, state, abs_melody)

  psychTestR::set_global("previous_melody", abs_melody, state)
  psychTestR::set_global("previous_durations", durations, state)

  # Attach generated absolute melody to meta data
  answer_meta_data <- cbind(melody_row,
                            tibble::tibble(abs_melody = paste0(abs_melody, collapse = ","),
                                           similarity_to_previous_melody = similarity_to_previous_melody,
                                           phase = phase,
                                           display_modality = display_modality,
                                           rhythmic = !arrhythmic
                            ))

  # Set the melody to be used
  psychTestR::set_global("melody", list("melody" = abs_melody,
                                        "durations" = durations), state)
  psychTestR::set_global("answer_meta_data", rjson::toJSON(answer_meta_data), state)


}


transposition_check <- function(melody_row) {
  if("transpose" %in% names(melody_row)) {
    transpose <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(transpose))
  }
}

grab_melody_from_state <- function(var_name, melody_no, state, psychTestRCAT = FALSE, rel_to_abs_mel_function = NULL) {

  if(psychTestRCAT) {

    melody_row <- psychTestR::get_local("item", state)
    item_number <- psychTestRCATME::get_item_number(melody_row)

    rel_melody <- melody_row %>% dplyr::pull(answer) %>% itembankr::str_mel_to_vector()

    if(is.null(rel_to_abs_mel_function)) {
      abs_melody <- melody_row %>% dplyr::pull(answer) %>% itembankr::str_mel_to_vector()
    }

  } else {
    # assume melodies sampled at test time and stored in global object
    trials <- psychTestR::get_global(var_name, state)
    melody_row <- trials %>% dplyr::slice(melody_no)
    rel_melody <- melody_row %>% dplyr::pull(melody) %>% itembankr::str_mel_to_vector()
    item_number <- NA
    abs_melody <- NA
  }

  list("rel_melody" = rel_melody,
       "melody_row" = melody_row,
       "abs_melody" = abs_melody,
       "item_number" = item_number)
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
    logging::loginfo('Update play melody loop and save')
    psychTestR::set_global("user_satisfied", answer$user_satisfied, state)
    number_attempts <- psychTestR::get_global("number_attempts", state)
    attempts_left <- max_goes - number_attempts
    psychTestR::set_global("attempts_left", attempts_left, state)
    number_attempts <- number_attempts + 1L
    psychTestR::set_global("number_attempts", number_attempts, state)
    logging::loginfo('Save results to disk...')
    psychTestR::save_results_to_disk(complete = FALSE, state, opt)
    logging::loginfo('...save results to disk complete.')
  })
}


melody_checks <- function(melody, state, stimuli_type = "midi_notes", arrhythmic = FALSE, note_length = 0.5) {

  if(is.null(melody)) {
    melody <- psychTestR::get_global("melody", state)
  }

  if(is.data.frame(melody)) {
    durations <- melody %>% dplyr::pull(durations) # durations needs to be first
    melody <- melody %>% dplyr::pull(abs_melody)
  }

  if(assertthat::is.string(melody) &
     stimuli_type != "midi_file" & stimuli_type != "audio") {
    melody <- itembankr::str_mel_to_vector(melody, ",")
  }

  if(stimuli_type == "midi_file") {
    start_note <- melody$start_note
    end_note <- melody$end_note
    melody <- melody$midi_file
  } else {
    start_note <- 1L
    end_note <- "end"
  }

  if(is.list(melody)) {
    durations <- as.vector(unlist(melody$durations))
    melody <- as.vector(unlist(melody$melody))
  } else {
    durations <- NA
  }

  if(arrhythmic) {
    durations <- rep(note_length, length(melody))
  }

  list("melody" = melody,
       "start_note" = start_note,
       "end_note" = end_note,
       "durations" = durations,
       "display_modality" = psychTestR::get_global("display_modality", state) )
}

midi_device_check <- function(page_type, state, midi_device = " ") {

  if(page_type == "record_midi_page") {
    midi_device <- psychTestR::get_global("midi_device", state)
    if(midi_device == " ") {
      shiny::showNotification(psychTestR::i18n("no_midi_device_selected"))
    } else {
      return(midi_device)
    }
  }
  midi_device
}


get_similarity_to_previous_melody <- function(get_similarity_to_previous_melody, melody_no, state, abs_melody) {

  if(get_similarity_to_previous_melody) {
    logging::loginfo("Get similarity to previous melody.")
    if(melody_no > 1) {
      previous_melody <- psychTestR::get_global("previous_melody", state)
      if(arrhythmic) {
        similarity_to_previous_melody <- ngrukkon_safe(diff(previous_melody), diff(abs_melody))
      } else {

        previous_durations <- psychTestR::get_global("previous_durations", state)

        previous_df <- tibble::tibble(note = previous_melody,
                                      freq = hrep::midi_to_freq(previous_melody), # doesn't matter, we don't use it here, but req for produce_extra_melodic_features
                                      dur = previous_durations,
                                      onset = cumsum(previous_durations)) %>%
          itembankr::produce_extra_melodic_features()

        current_df <- tibble::tibble(note = abs_melody,
                                     freq = hrep::midi_to_freq(abs_melody), # doesn't matter, we don't use it here, but req for produce_extra_melodic_features
                                     dur = durations,
                                     onset = cumsum(durations)) %>%
          itembankr::produce_extra_melodic_features()



        similarity_to_previous_melody <- opti3_df(previous_df, current_df)$opti3
      }
    } else {
      similarity_to_previous_melody <- NA
    }
  } else {
    logging::loginfo("Don't get similarity to previous melody.")
    similarity_to_previous_melody <- NA
  }

  similarity_to_previous_melody
}



