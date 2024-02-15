

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
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param melody_block_paradigm
#' @param singing_trials
#' @param phase
#' @param melody_trial_paradigm
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param learn_test_paradigm
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
                                        sheet_music_start_hidden = FALSE,
                                        sound_only_first_melody_note = FALSE,
                                        sheet_music_id = 'sheet_music',
                                        give_first_melody_note = FALSE,
                                        get_similarity_to_previous_melody = FALSE,
                                        volume_meter = FALSE,
                                        volume_meter_type = 'default',
                                        melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                                        singing_trials = TRUE,
                                        phase = c('test', 'learn', 'review', 'example'),
                                        melody_trial_paradigm = c("call_and_response", "simultaneous_recall"),
                                        first_note_message = psychTestR::i18n("first_note_is"),
                                        transposed_message = psychTestR::i18n("transposed"),
                                        play_first_note_button_text = psychTestR::i18n("play_first_note"),
                                        learn_test_paradigm = FALSE) {

  melody_block_paradigm <- match.arg(melody_block_paradigm)
  melody_trial_paradigm <- match.arg(melody_trial_paradigm)
  phase <- match.arg(phase)

  stopifnot(is.null.or(item_bank, tibble::is_tibble),
            is.null.or(presampled_items, is.data.frame),
            is.null.or(num_items, is.scalar.numeric),
            is.scalar.character(var_name),
            is.scalar.character(stimuli_type),
            is.scalar.character(page_type),
            is.scalar.numeric(max_goes),
            is(page_title, "html") || is.character(page_title),
            is.scalar.character(page_text),
            is.function(get_answer),
            is.null.or(rel_to_abs_mel_function, is.function),
            is.scalar.numeric(start_from_trial_no),
            is.scalar.logical(clip_stimuli_length),
            is.scalar.logical(arrhythmic),
            is.scalar.logical(example),
            is.function(feedback) | is.scalar.logical(feedback),
            is.scalar.character(sound),
            is.null.or(get_trial_characteristics_function, is.function),
            is.scalar.logical(max_goes_forced),
            is.scalar.character(display_modality),
            is.scalar.logical(show_progress),
            is.scalar.logical(sheet_music_start_hidden),
            is.scalar.logical(sound_only_first_melody_note),
            is.scalar.character(sheet_music_id),
            is.scalar.logical(give_first_melody_note),
            is.scalar.logical(get_similarity_to_previous_melody),
            is.scalar.logical(volume_meter),
            is.scalar.character(volume_meter_type),
            melody_block_paradigm %in% c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
            is.scalar.logical(singing_trials),
            phase %in% c('learn', 'test', 'review', 'example'),
            melody_trial_paradigm %in% c("call_and_response", "simultaneous_recall"),
            is.scalar.character(first_note_message),
            is.scalar.character(transposed_message),
            is.scalar.character(play_first_note_button_text),
            is.scalar.logical(learn_test_paradigm)
            )

  if(is.null(presampled_items) && is.infinite(num_items)) {
    # Then the test stops when defined by the user
    items <- psychTestR::join(

      psychTestR::code_block(function(state, ...) {
        psychTestR::set_global("reactive_melody_no", 1L, state)
        psychTestR::set_global("user_determined_stop", FALSE, state)
      }),

      psychTestR::while_loop(test = function(state, ...) {
        ! psychTestR::get_global("user_determined_stop", state)
      }, logic = psychTestR::join(
                construct_play_melody_page(melody = NULL,
                                           melody_row = NULL,
                                           melody_no = NA,
                                           var_name,
                                           max_goes,
                                           page_type,
                                           page_title,
                                           page_text,
                                           get_answer,
                                           stimuli_type,
                                           rel_to_abs_mel_function,
                                           clip_stimuli_length,
                                           arrhythmic,
                                           example,
                                           sound,
                                           get_trial_characteristics_function,
                                           max_goes_forced,
                                           item_bank,
                                           display_modality,
                                           num_items,
                                           show_progress,
                                           sheet_music_start_hidden,
                                           sound_only_first_melody_note,
                                           sheet_music_id,
                                           give_first_melody_note,
                                           get_similarity_to_previous_melody,
                                           volume_meter,
                                           volume_meter_type,
                                           singing_trials,
                                           phase,
                                           melody_block_paradigm,
                                           melody_trial_paradigm,
                                           first_note_message,
                                           transposed_message,
                                           play_first_note_button_text,
                                           reactive_melody_no = TRUE,
                                           learn_test_paradigm),

      psychTestR::NAFC_page(label = "finished_user_determined_loop",
                            choices = c("Yes", "No"),
                            prompt = "Would you like to continue learning?",
                            on_complete = function(state, answer, ...) {

                              if(answer == "No") {
                                psychTestR::set_global("user_determined_stop", TRUE, state)
                                reactive_melody_no <- psychTestR::get_global("reactive_melody_no", state)
                                psychTestR::set_global("reactive_melody_no", reactive_melody_no + 1L, state)
                              }

                            }))
      )
    )


  } else if(is.null(presampled_items) && ! is.infinite(num_items)) {

    # This will return a sequence of test items
    items <- purrr::map(start_from_trial_no:num_items, function(melody_no) {

      construct_play_melody_page(melody = NULL,
                                 melody_row = NULL,
                                 melody_no,
                                 var_name,
                                 max_goes,
                                 page_type,
                                 page_title,
                                 page_text,
                                 get_answer,
                                 stimuli_type,
                                 rel_to_abs_mel_function ,
                                 clip_stimuli_length,
                                 arrhythmic,
                                 example,
                                 sound,
                                 get_trial_characteristics_function,
                                 max_goes_forced,
                                 item_bank,
                                 display_modality,
                                 num_items,
                                 show_progress,
                                 sheet_music_start_hidden,
                                 sound_only_first_melody_note,
                                 sheet_music_id,
                                 give_first_melody_note,
                                 get_similarity_to_previous_melody,
                                 volume_meter,
                                 volume_meter_type,
                                 singing_trials,
                                 phase,
                                 melody_block_paradigm,
                                 melody_trial_paradigm,
                                 first_note_message,
                                 transposed_message,
                                 play_first_note_button_text,
                                 reactive_melody_no = FALSE,
                                 learn_test_paradigm)
    })


  } else {

    items <- purrr::map(1:nrow(presampled_items), function(x) {

      melody <- presampled_items %>% dplyr::slice(!! x)

      construct_play_melody_page(melody = NULL,
                                 melody_row = melody, # We actually want to give the whole row to melody row
                                 melody_no = x,
                                 var_name,
                                 max_goes,
                                 page_type,
                                 page_title,
                                 page_text,
                                 get_answer,
                                 stimuli_type,
                                 rel_to_abs_mel_function ,
                                 clip_stimuli_length,
                                 arrhythmic,
                                 example,
                                 sound,
                                 get_trial_characteristics_function,
                                 max_goes_forced,
                                 item_bank,
                                 display_modality,
                                 num_items,
                                 show_progress,
                                 sheet_music_start_hidden,
                                 sound_only_first_melody_note,
                                 sheet_music_id,
                                 give_first_melody_note,
                                 get_similarity_to_previous_melody,
                                 volume_meter,
                                 volume_meter_type,
                                 singing_trials,
                                 phase,
                                 melody_block_paradigm,
                                 melody_trial_paradigm,
                                 first_note_message,
                                 transposed_message,
                                 play_first_note_button_text,
                                 reactive_melody_no = FALSE,
                                 learn_test_paradigm)

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


construct_play_melody_page <- function(melody = NULL,
                                       melody_row = NULL,
                                       melody_no,
                                       var_name,
                                       max_goes,
                                       page_type,
                                       page_title,
                                       page_text,
                                       get_answer,
                                       stimuli_type,
                                       rel_to_abs_mel_function,
                                       clip_stimuli_length,
                                       arrhythmic,
                                       example,
                                       sound,
                                       get_trial_characteristics_function,
                                       max_goes_forced,
                                       item_bank,
                                       display_modality,
                                       num_items,
                                       show_progress,
                                       sheet_music_start_hidden,
                                       sound_only_first_melody_note,
                                       sheet_music_id,
                                       give_first_melody_note,
                                       get_similarity_to_previous_melody,
                                       volume_meter,
                                       volume_meter_type,
                                       singing_trials,
                                       phase,
                                       melody_block_paradigm,
                                       melody_trial_paradigm,
                                       first_note_message,
                                       transposed_message,
                                       play_first_note_button_text,
                                       reactive_melody_no = FALSE,
                                       learn_test_paradigm = FALSE) {


  page <- play_melody_loop(melody_no = melody_no,
                           melody_row = melody_row,
                           melody = melody,
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
                           sheet_music_start_hidden = sheet_music_start_hidden,
                           sound_only_first_melody_note = sound_only_first_melody_note,
                           sheet_music_id = sheet_music_id,
                           give_first_melody_note = give_first_melody_note,
                           get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                           volume_meter = volume_meter,
                           volume_meter_type = volume_meter_type,
                           singing_trial = singing_trials,
                           phase = phase,
                           melody_block_paradigm = melody_block_paradigm,
                           melody_trial_paradigm = melody_trial_paradigm,
                           first_note_message = first_note_message,
                           transposed_message = transposed_message,
                           play_first_note_button_text = play_first_note_button_text,
                           skip_sampling_and_take_from_last_melody = melody_block_paradigm == "sing_melody_first",
                           reactive_melody_no = reactive_melody_no,
                           learn_test_paradigm = learn_test_paradigm)
                           # In the case of putting a sing melody page first, we do the sampling on the sing page (in code below, but which chronogically comes first); then we skip sampling on the "real" (instrument) version, and just use the sampled melody from the sing page

  if (melody_block_paradigm == "sing_melody_first") {

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
                                  display_modality = "auditory",
                                  total_no_melodies = num_items,
                                  show_progress = show_progress,
                                  sheet_music_start_hidden = sheet_music_start_hidden,
                                  sound_only_first_melody_note = FALSE,
                                  sheet_music_id = sheet_music_id,
                                  give_first_melody_note = FALSE,
                                  get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                                  volume_meter = volume_meter,
                                  volume_meter_type = volume_meter_type,
                                  singing_trial = TRUE,
                                  phase = phase,
                                  melody_block_paradigm = melody_block_paradigm,
                                  melody_trial_paradigm = melody_trial_paradigm,
                                  first_note_message = first_note_message,
                                  transposed_message = transposed_message,
                                  play_first_note_button_text = play_first_note_button_text,
                                  reactive_melody_no = reactive_melody_no,
                                  learn_test_paradigm = learn_test_paradigm)

    sing_then_play_pages <- psychTestR::join(sing_page, page)

    return(sing_then_play_pages)

  } else {
    return(page)
  }
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
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param psychTestRCAT
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param singing_trial Boolean: if TRUE, the trial is defined as singing-based, otherwise instrument-based.
#' @param phase Can be one of 'learn', 'test', 'review' or 'example'
#' @param melody_block_paradigm
#' @param melody_trial_paradigm
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param skip_sampling_and_take_from_last_melody
#' @param reactive_melody_no
#' @param learn_test_paradigm
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
                             sheet_music_start_hidden = FALSE,
                             sound_only_first_melody_note = FALSE,
                             sheet_music_id = 'sheet_music',
                             give_first_melody_note = FALSE,
                             psychTestRCAT = FALSE,
                             melody_row = NULL,
                             get_similarity_to_previous_melody = FALSE,
                             volume_meter = FALSE,
                             volume_meter_type = 'default',
                             singing_trial = TRUE,
                             phase = c('test', 'learn', 'review', 'example'),
                             melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                             melody_trial_paradigm = c("call_and_response", "simultaneous_recall"),
                             first_note_message = psychTestR::i18n("first_note_is"),
                             transposed_message = psychTestR::i18n("transposed"),
                             play_first_note_button_text = psychTestR::i18n("play_first_note"),
                             skip_sampling_and_take_from_last_melody = FALSE,
                             reactive_melody_no = FALSE,
                             learn_test_paradigm = FALSE) {


  save_answer <- ! example

  phase <- match.arg(phase)
  melody_block_paradigm <- match.arg(melody_block_paradigm)
  melody_trial_paradigm <- match.arg(melody_trial_paradigm)


  psychTestR::join(
    # Set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      # Repeat melody logic stuff
      psychTestR::set_global("user_satisfied", "Try Again", state)
      psychTestR::set_global("number_attempts", 1, state)
      psychTestR::set_global("max_goes", max_goes, state)
      psychTestR::set_global("attempts_left", max_goes, state)
      psychTestR::set_global("display_modality", display_modality, state)
      psychTestR::set_global("phase", phase, state)
      psychTestR::set_global("rhythmic", ! arrhythmic, state)
      # Grab sampled melody for this trial (if one not specified)
      if(is.null(melody) && ! skip_sampling_and_take_from_last_melody && phase != "review") grab_sampled_melody(item_bank, melody_row, var_name, stimuli_type, state, melody_no, arrhythmic, rel_to_abs_mel_function, note_length, get_trial_characteristics_function, psychTestRCAT, get_similarity_to_previous_melody, phase, display_modality, reactive_melody_no, learn_test_paradigm)
      if(phase == "review") grab_sampled_melody_review(var_name, state, melody_no, arrhythmic, rel_to_abs_mel_function, note_length)
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
                     sheet_music_start_hidden = sheet_music_start_hidden,
                     sound_only_first_melody_note = sound_only_first_melody_note,
                     sheet_music_id = sheet_music_id,
                     give_first_melody_note = give_first_melody_note,
                     arrhythmic = arrhythmic,
                     note_length = note_length,
                     psychTestRCAT = psychTestRCAT,
                     volume_meter = volume_meter,
                     volume_meter_type = volume_meter_type,
                     singing_trial = singing_trial,
                     phase = phase,
                     melody_trial_paradigm = melody_trial_paradigm,
                     melody_block_paradigm = melody_block_paradigm,
                     first_note_message = first_note_message,
                     transposed_message = transposed_message,
                     play_first_note_button_text = play_first_note_button_text,
                     reactive_melody_no = reactive_melody_no),

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
                           sheet_music_start_hidden = FALSE,
                           sound_only_first_melody_note = FALSE,
                           sheet_music_id = 'sheet_music',
                           give_first_melody_note = FALSE,
                           arrhythmic = FALSE,
                           note_length = 0.5,
                           psychTestRCAT = FALSE,
                           volume_meter = FALSE,
                           volume_meter_type = 'default',
                           singing_trial = TRUE,
                           melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                           user_rating = FALSE,
                           happy_with_response = FALSE,
                           melody_trial_paradigm = c("call_and_response", "simultaneous_recall"),
                           call_and_response_end = c("manual", "auto"),
                           first_note_message = psychTestR::i18n("first_note_is"),
                           transposed_message = psychTestR::i18n("transposed"),
                           play_first_note_button_text = psychTestR::i18n("play_first_note"),
                           reactive_melody_no = FALSE, ...) {

  melody_block_paradigm <- match.arg(melody_block_paradigm)
  melody_trial_paradigm <- match.arg(melody_trial_paradigm)
  call_and_response_end <- match.arg(call_and_response_end)

  psychTestR::reactive_page(function(state, ...) {

    # Grab various variables
    number_attempts <- psychTestR::get_global("number_attempts", state)
    max_goes <- psychTestR::get_global("max_goes", state)
    attempts_left <- psychTestR::get_global("attempts_left", state) - 1L
    transpose_visual_notation <- psychTestR::get_global("transpose_visual_notation", state)
    transpose_visual_notation <- if(is.null(transpose_visual_notation)) 0L else transpose_visual_notation
    clef <- psychTestR::get_global("clef", state)
    clef <- if(is.null(clef)) "auto" else clef

    if(length(answer_meta_data) < 1L) {
      answer_meta_data <- psychTestR::get_global("answer_meta_data", state)
      if(is.null(answer_meta_data)) {
        answer_meta_data <- data.frame()
      }
    }

    logging::loginfo("Transpose visual notation play melody loop: %s", transpose_visual_notation)
    logging::loginfo("Getting clef: %s", clef)

    # MIDI checks
    midi_device <- midi_device_check(page_type, state)

    # Grab vars
    melody_checks <- melody_checks(stimuli, state, stimuli_type, arrhythmic, note_length)

    if(psychTestRCAT) {
      melody_no <- psychTestR::get_local("item", state) %>%
        psychTestRCAT::get_item_number()
    }

    # Set some vars for storing in DB
    trial_time_started <- Sys.time()
    psychTestR::set_global("trial_time_started", trial_time_started, state)
    psychTestR::set_global("singing_trial", singing_trial, state)

    # Use a display_modality created at test time, if appropriate
    display_modality <- if(is.null(melody_checks$display_modality)) display_modality else melody_checks$display_modality

    # Get trial paradigm info
    trial_paradigm <- paradigm(paradigm_type = melody_trial_paradigm, page_type = page_type, call_and_response_end = call_and_response_end, attempts_left = attempts_left)

    db_vars <- if(psychTestR::get_global("musicassessr_db", state)) {

      list(
        midi_vs_audio = stringr::str_remove(stringr::str_remove(page_type, "record_"), "_page"),
        stimuli = paste0(melody_checks$melody, collapse = ","), # Note the duplication
        stimuli_durations = paste0(melody_checks$durations, collapse = ","),
        trial_time_started = trial_time_started,
        instrument = psychTestR::get_global("inst", state),
        attempt = number_attempts,
        item_id = if(is.scalar.character(answer_meta_data)) rjson::fromJSON(answer_meta_data)$item_id else answer_meta_data$item_id,
        display_modality = display_modality,
        phase = psychTestR::get_global("phase", state),
        rhythmic = !arrhythmic,
        item_bank_id = psychTestR::get_global("item_bank_id", state),
        session_id = get_promise_value(psychTestR::get_global("session_id", state)),
        test_id = psychTestR::get_global("test_id", state)
      )
    } else NULL

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
                    trigger_start_of_stimulus_fun = trial_paradigm$trigger_start_of_stimulus_fun,
                    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
                    hideOnPlay = hideOnPlay,
                    max_goes = max_goes,
                    max_goes_forced = max_goes_forced,
                    transpose_visual_notation = transpose_visual_notation,
                    clef = clef,
                    melody_no = if(reactive_melody_no) psychTestR::get_global("reactive_melody_no", state) else melody_no,
                    show_progress = show_progress,
                    total_no_melodies = total_no_melodies,
                    sheet_music_start_hidden = if(melody_block_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual") TRUE else sheet_music_start_hidden,
                    sound_only_first_melody_note = sound_only_first_melody_note,
                    sheet_music_id = sheet_music_id,
                    give_first_melody_note = if(melody_block_paradigm == 'learn_phase_visual_display_modality') FALSE else give_first_melody_note,
                    volume_meter = volume_meter,
                    volume_meter_type = volume_meter_type,
                    show_sheet_music_after_record = melody_block_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual",
                    show_record_button = melody_block_paradigm == 'learn_phase_visual_display_modality' && display_modality == "visual",
                    first_note_message = first_note_message,
                    transposed_message = transposed_message,
                    play_first_note_button_text = play_first_note_button_text,
                    reactive_melody_no = reactive_melody_no,
                    db_vars = db_vars,
                    use_musicassessr_db = psychTestR::get_global("musicassessr_db", state))

  })
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
                                display_modality = c('auditory', 'visual', 'both'),
                                reactive_melody_no = FALSE,
                                learn_test_paradigm = FALSE, ...) {

  logging::loginfo("Grab sampled melody")
  display_modality <- match.arg(display_modality)
  logging::loginfo("Display modality: %s", display_modality)
  # Not all trial specifications need a range, instrument. etc but grab it here anyway
  bottom_range <- psychTestR::get_global("bottom_range", state)
  top_range <- psychTestR::get_global("top_range", state)
  range <- psychTestR::get_global("range", state)
  inst <- psychTestR::get_global("inst", state)


  # Has melody been specified directly, or sampled at test time?
  if(is.null(melody_row)) {
    logging::loginfo("Sample at test time")
    # Assume melodies sampled at test time and stored in global object
    if(is.null(get_trial_characteristics_function) || learn_test_paradigm && phase == "test") {

      melody_from_state <- grab_melody_from_state(var_name, melody_no, state, psychTestRCAT, rel_to_abs_mel_function, learn_test_paradigm, phase)
      rel_melody <- melody_from_state$rel_melody
      melody_row <- melody_from_state$melody_row
      abs_melody <- melody_from_state$abs_melody
      item_number <- melody_from_state$item_number
      melody_no <- if(!is.na(item_number)) item_number else melody_no

    } else {

      trials <- psychTestR::get_global(var_name, state)
      melody_no <- if(reactive_melody_no) psychTestR::get_global("reactive_melody_no", state) else melody_no
      trial_char <- get_trial_characteristics_function(trial_df = trials, trial_no = melody_no)
      melody_row <- sample_melody_in_key(item_bank, inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = trial_char$key_difficulty, length = trial_char$melody_length)
      melody_row <- cbind(melody_row, tibble::tibble(key_difficulty = trial_char$key_difficulty, display_modality = trial_char$display_modality) )
      # Not fully abstracted from PBET setup, yet
      abs_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(abs_melody))
      rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
      rel_to_abs_mel_function <- NULL
      display_modality <- trial_char$display_modality
    }

  } else {
    logging::loginfo("Melody has been specified.")
    transpose <- transposition_check(melody_row)
    rel_melody <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(melody))
    logging::loginfo("Rel melody: %s", melody_row %>% dplyr::pull(melody))
  }

  # Does the melody need to be rhythmic or arrhythmic?
  durations <- if(arrhythmic) NA else itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(durations))
  logging::loginfo("Durations: %s", melody_row %>% dplyr::pull(durations))
  melody <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$melody
  durations <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$durations

  # Does the melody need putting into a certain pitch range...?
  abs_melody <- transpose_melody(rel_to_abs_mel_function, rel_melody, abs_melody, melody, bottom_range, top_range, range, transpose)

  # Get similarity to previous melody
  similarity_to_previous_melody <- get_similarity_to_previous_melody(get_similarity_to_previous_melody, melody_no, state, abs_melody)


  # Attach generated absolute melody to meta data

  answer_meta_data <- melody_row %>%
    dplyr::mutate(similarity_to_previous_melody = similarity_to_previous_melody,
                  phase = phase,
                  rhythmic = !arrhythmic,
                  abs_melody = paste0(abs_melody, collapse = ","), # Thus, we overwrite what was in melody_row (abs_melody may have been transposed)
                  display_modality = display_modality)

  previous_melodies_var_name <- paste0("previous_melodies_", var_name)

  if(is.null(psychTestR::get_global(previous_melodies_var_name, state))) {
    psychTestR::set_global(previous_melodies_var_name, answer_meta_data, state)
  } else {
    prev <- psychTestR::get_global(previous_melodies_var_name, state)
    logging::logdebug("prev %s", prev)
    logging::logdebug("answer_meta_data %s", answer_meta_data)
    psychTestR::set_global(previous_melodies_var_name, rbind(prev, answer_meta_data), state)
  }

  # Set the melody to be used
  psychTestR::set_global("item_id", melody_row$item_id, state)
  psychTestR::set_global("melody", list("melody" = abs_melody, "durations" = durations), state)
  psychTestR::set_global("answer_meta_data", rjson::toJSON(answer_meta_data), state)
  # And other variables
  psychTestR::set_global('display_modality', display_modality, state)
  psychTestR::set_global("previous_melody", abs_melody, state)
  psychTestR::set_global("previous_durations", durations, state)


}


grab_sampled_melody_review <- function(var_name, state, melody_no, arrhythmic, rel_to_abs_mel_function, note_length) {

  melody_from_state <- grab_melody_from_state(var_name, melody_no, state, psychTestRCAT = FALSE, rel_to_abs_mel_function)

  arrhythmic <- ! melody_from_state$rhythmic

  psychTestR::set_global("answer_meta_data", melody_from_state$melody_row, state)

  rel_melody <- melody_from_state$rel_melody
  melody_row <- melody_from_state$melody_row
  item_id <- melody_from_state$item_id

  bottom_range <- psychTestR::get_global("bottom_range", state)
  top_range <- psychTestR::get_global("top_range", state)
  range <- psychTestR::get_global("range", state)
  inst <- psychTestR::get_global("inst", state)

  transpose <- transposition_check(melody_row)

  durations <- if(arrhythmic) NA else itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(durations))
  melody <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$melody
  durations <- sort_arrhythmic(arrhythmic, rel_melody, durations, note_length)$durations

  abs_melody <- transpose_melody(rel_to_abs_mel_function, rel_melody, abs_melody, melody, bottom_range, top_range, range, transpose)
  psychTestR::set_global("melody", list("melody" = abs_melody, "durations" = durations), state)
  psychTestR::set_global("item_id", item_id, state)
}

transpose_melody <- function(rel_to_abs_mel_function, rel_melody, abs_melody, melody, bottom_range, top_range, range, transpose) {

  logging::loginfo('Transpose melody...')
  logging::loginfo("Rel melody: %s", paste0(rel_melody, collapse = ", "))

  if(is.null(rel_to_abs_mel_function)) {
    if(is.data.frame(abs_melody)) {
      abs_melody <- melody %>%
        dplyr::pull(abs_melody)
    }
  } else {
    # ...then assume that the melody is in relative format and fit it into a key, based on a rel_to_abs_mel_functionw
    abs_melody <- rel_to_abs_mel_function(rel_melody = rel_melody, bottom_range = bottom_range, top_range = top_range, range = range, transpose = transpose)
  }
  return(abs_melody)
}

transposition_check <- function(melody_row) {
  if("transpose" %in% names(melody_row)) {
    transpose <- itembankr::str_mel_to_vector(melody_row %>% dplyr::pull(transpose))
  }
}

grab_melody_from_state <- function(var_name, melody_no, state, psychTestRCAT = FALSE, rel_to_abs_mel_function = NULL, learn_test_paradigm = FALSE, phase = "test") {

  if(psychTestRCAT) {

    melody_row <- psychTestR::get_local("item", state)
    item_number <- psychTestRCATME::get_item_number(melody_row)

    rel_melody <- melody_row %>% dplyr::pull(answer) %>% itembankr::str_mel_to_vector()

    if(is.null(rel_to_abs_mel_function)) {
      abs_melody <- melody_row %>% dplyr::pull(answer) %>% itembankr::str_mel_to_vector()
    }

  } else {

    # Assume melodies sampled at test time and stored in global object
    trials <- psychTestR::get_global(var_name, state)
    melody_row <- trials %>% dplyr::slice(!! melody_no)
    rel_melody <- melody_row %>% dplyr::pull(melody) %>% itembankr::str_mel_to_vector()
    item_id <- melody_row %>% dplyr::pull(item_id)
    item_number <- NA
    abs_melody <- NA
  }

  list(rel_melody = rel_melody,
       melody_row = melody_row,
       abs_melody = abs_melody,
       item_number = item_number,
       rhythmic = melody_row %>% dplyr::pull(rhythmic),
       item_id = item_id)
}

sort_arrhythmic <- function(arrhythmic, rel_melody, durations, note_length) {

  if(arrhythmic) {
    durations <- rep(note_length, length(rel_melody) + 1)
  }
  list(melody = rel_melody, durations = durations)
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

  if(is.scalar.character(melody)) {
    durations <- itembankr::str_mel_to_vector(durations)
    melody <- itembankr::str_mel_to_vector(melody)
  }

  start_note <- 1L
  end_note <- "end"

  if(is.list(melody)) {
    durations <- as.vector(unlist(melody$durations))
    melody <- as.vector(unlist(melody$melody))
  } else {
    durations <- NA
  }

  if(arrhythmic) {
    durations <- rep(note_length, length(melody))
  }

  list(melody = melody,
       start_note = start_note,
       end_note = end_note,
       durations = durations,
       display_modality = psychTestR::get_global("display_modality", state) )
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
                                      freq = hrep::midi_to_freq(previous_melody), # Doesn't matter, we don't use it here, but req for produce_extra_melodic_features
                                      dur = previous_durations,
                                      onset = cumsum(previous_durations)) %>%
          itembankr::produce_extra_melodic_features()

        current_df <- tibble::tibble(note = abs_melody,
                                     freq = hrep::midi_to_freq(abs_melody), # Doesn't matter, we don't use it here, but req for produce_extra_melodic_features
                                     dur = durations,
                                     onset = cumsum(durations)) %>%
          itembankr::produce_extra_melodic_features()


        similarity_to_previous_melody <- opti3_df(previous_df, current_df)$opti3
      }
    } else {
      similarity_to_previous_melody <- NA
    }
  } else {
    similarity_to_previous_melody <- NA
  }

  similarity_to_previous_melody
}



