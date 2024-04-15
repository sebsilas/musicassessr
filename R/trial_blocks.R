

#' A block for playing melodies as audio then recording the participant response
#'
#' @param files_list
#' @param page_text
#' @param get_answer
#'
#' @return
#' @export
#'
#' @examples
record_and_present_audio_block <- function(files_list,
                                           page_text = "Press play to hear a melody, then sing it back.",
                                           get_answer = musicassessr::get_answer_pyin) {

  block <- purrr::map(files_list, function(file) {

    musicassessr::present_stimuli(
      stimuli = file,
      stimuli_type = "audio",
      display_modality = "auditory",
      page_type = "record_audio_page",
      get_answer = get_answer,
      page_text = page_text,
      hideOnPlay = TRUE,
      page_label = basename(file), # Check this also includes participant etc.
      volume = 0.60)

  })

  block <- musicassessr::insert_item_into_every_other_n_position_in_list(block, psychTestR::elt_save_results_to_disk(complete = FALSE))

  return(block)

}


# TODO: Factor the record...block pages

#' A block of record key press pages
#'
#' @param no_pages
#' @param label
#' @param feedback
#' @param get_answer
#' @param page_text
#' @param page_title
#'
#' @return
#' @export
#'
#' @examples
record_key_presses_block <- function(no_pages,
                                     label = "record_key_presses",
                                     feedback = NULL,
                                     get_answer = get_answer_pyin,
                                     page_title = psychTestR::i18n("Record_audio"),
                                     page_text = psychTestR::i18n("click_to_record_audio")) {
  pages <- psychTestR::join(

    rep(list(record_key_presses_page(get_answer = get_answer,
                                     label = label,
                                     page_title = page_title,
                                     page_text = page_text)), no_pages)
  )
  if(!is.null(feedback)) {
    pages <- add_feedback(pages, feedback)
  }
  pages
}

#' A block of record audio pages
#'
#' @param no_pages
#' @param label
#' @param feedback
#' @param get_answer
#' @param page_text
#' @param page_title
#'
#' @return
#' @export
#'
#' @examples
record_audio_block <- function(no_pages,
                               label = "record_audio",
                               feedback = NULL,
                               get_answer = get_answer_pyin,
                               page_title = psychTestR::i18n("Record_audio"),
                               page_text = psychTestR::i18n("click_to_record_audio")) {
  pages <- psychTestR::join(

    rep(list(record_audio_page(get_answer = get_answer,
                               label = label,
                               page_title = page_title,
                               page_text = page_text)), no_pages)
  )
  if(!is.null(feedback)) {
    pages <- add_feedback(pages, feedback)
  }
  pages
}


#' A block of record MIDI pages
#'
#' @param no_pages
#' @param label
#' @param feedback
#' @param get_answer
#' @param page_title
#' @param page_text
#' @param autoInstantiate
#' @param mute_midi_playback
#'
#' @return
#' @export
#'
#' @examples
record_midi_block <- function(no_pages,
                              label = "record_midi_page",
                              feedback = NULL,
                              get_answer = get_answer_midi,
                              page_title = psychTestR::i18n("Record_audio"),
                              page_text = psychTestR::i18n("click_to_record_audio"),
                              autoInstantiate = TRUE,
                              mute_midi_playback = FALSE) {

  page <- psychTestR::reactive_page(function(state, ...) {

    midi_device <- psychTestR::get_global("midi_device", state)

    if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(get_answer = get_answer,
                       label = label,
                       page_title = page_title,
                       page_text = page_text,
                       midi_device = midi_device,
                       autoInstantiate = autoInstantiate,
                       mute_midi_playback = mute_midi_playback)

    })

  pages <- psychTestR::join(rep(list(page), no_pages))

  if(!is.null(feedback)) {
    pages <- add_feedback(pages, feedback)
  }
  pages
}




#' Sing arrhythmic melody trial block
#'
#' @param var_name
#' @param module_name
#' @param page_text
#' @param page_title
#' @param instruction_text
#' @param sampler_function
#' @param item_bank,
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_text
#' @param page_title
#' @param page_type
#' @param instruction_text
#' @param get_trial_characteristics_function
#' @param item_characteristics_sampler_function
#' @param item_characteristics_pars
#' @param rel_to_abs_mel_function
#' @param max_goes
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param module_name
#' @param get_similarity_to_previous_melody
#'
#'
#' @return
#' @export
#'
#' @examples
sing_arrhythmic_melody_trials <- function(var_name = "arrhythmic_melody",
                                          module_name = "sing_arrhythmic_melodies",
                                          page_text = psychTestR::i18n("sing_melody_page_text"),
                                          page_title = psychTestR::i18n("Sing_the_Melody"),
                                          instruction_text = psychTestR::i18n("sing_melody_instruction_text"),
                                          sampler_function = sample_arrhythmic,
                                          item_bank,
                                          num_items = integer(),
                                          num_examples = 0L,
                                          feedback = FALSE,
                                          get_answer = get_answer_pyin_melodic_production,
                                          sound = "piano",
                                          page_type = "record_audio_page",
                                          get_trial_characteristics_function = NULL,
                                          item_characteristics_sampler_function = NULL,
                                          item_characteristics_pars = NULL,
                                          rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
                                          max_goes = 3L,
                                          max_goes_forced = FALSE,
                                          display_modality = "auditory",
                                          show_progress = TRUE,
                                          get_similarity_to_previous_melody = FALSE) {

  arrhythmic_melody_trials(var_name,
                           module_name,
                           page_text,
                           page_title,
                           instruction_text,
                           sampler_function,
                           item_bank,
                           num_items,
                           num_examples,
                           feedback,
                           get_answer,
                           sound,
                           page_type,
                           get_trial_characteristics_function,
                           item_characteristics_sampler_function,
                           item_characteristics_pars,
                           rel_to_abs_mel_function,
                           max_goes,
                           max_goes_forced,
                           display_modality,
                           show_progress,
                           sheet_music_start_hidden,
                           sound_only_first_melody_note,
                           sheet_music_id,
                           give_first_melody_note,
                           presampled,
                           get_similarity_to_previous_melody,
                           asynchronous_api_mode)

}




#' Sing rhythmic melody trial block
#'
#' @param var_name
#' @param module_name
#' @param page_text
#' @param page_title
#' @param instruction_text
#' @param sampler_function
#' @param item_bank,
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_text
#' @param page_title
#' @param page_type
#' @param instruction_text
#' @param get_trial_characteristics_function
#' @param item_characteristics_sampler_function
#' @param item_characteristics_pars
#' @param rel_to_abs_mel_function
#' @param max_goes
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param module_name
#' @param get_similarity_to_previous_melody
#'
#' @return
#' @export
#'
#' @examples
sing_rhythmic_melody_trials <- function(var_name = "rhythmic_melody",
                                        module_name = "sing_rhythmic_melodies",
                                        page_text = psychTestR::i18n("sing_rhythmic_melodies_page_text"),
                                        page_title = psychTestR::i18n("sing_rhythmic_melodies_page_title"),
                                        instruction_text = psychTestR::i18n("sing_rhythmic_melodies_instruction_text"),
                                        sampler_function = sample_arrhythmic,
                                        item_bank,
                                        num_items = integer(),
                                        num_examples = 0L,
                                        feedback = FALSE,
                                        get_answer = get_answer_pyin_melodic_production,
                                        sound = "piano",
                                        page_type = "record_audio_page",
                                        get_trial_characteristics_function = NULL,
                                        item_characteristics_sampler_function = NULL,
                                        item_characteristics_pars = NULL,
                                        rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
                                        max_goes = 3L,
                                        max_goes_forced = FALSE,
                                        display_modality = "auditory",
                                        show_progress = TRUE,
                                        get_similarity_to_previous_melody = FALSE) {

  rhythmic_melody_trials(var_name,
                         module_name,
                         page_text,
                         page_title,
                         instruction_text,
                         sampler_function,
                         item_bank,
                         num_items,
                         num_examples,
                         feedback,
                         get_answer,
                         sound,
                         page_type,
                         get_trial_characteristics_function,
                         item_characteristics_sampler_function,
                         item_characteristics_pars,
                         rel_to_abs_mel_function,
                         max_goes,
                         max_goes_forced,
                         display_modality,
                         show_progress,
                         sheet_music_start_hidden,
                         sound_only_first_melody_note,
                         sheet_music_id,
                         give_first_melody_note,
                         presampled,
                         get_similarity_to_previous_melody)

}





#' Arrhythmic melody trials block
#'
#' @param var_name
#' @param module_name
#' @param page_text
#' @param page_title
#' @param instruction_text
#' @param sampler_function
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_type
#' @param get_trial_characteristics_function
#' @param item_characteristics_sampler_function
#' @param item_characteristics_pars
#' @param rel_to_abs_mel_function
#' @param max_goes
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param presampled
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param melody_block_paradigm
#' @param singing_trials
#' @param review
#' @param phase
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param learn_test_paradigm
#' @param sample_item_bank_via_api
#' @param start_from_sampled_trial_no
#' @param pass_items_through_url_parameter
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
arrhythmic_melody_trials <- function(var_name = "arrhythmic_melody",
                                     module_name = "arrhythmic_melodies",
                                     page_text = psychTestR::i18n("arrhythmic_melody_trial_page_text"),
                                     page_title = psychTestR::i18n("arrhythmic_melody_trial_page_title"),
                                     instruction_text = psychTestR::i18n("arrhythmic_melody_trial_instruction_text"),
                                     sampler_function = sample_arrhythmic,
                                     item_bank,
                                     num_items = integer(),
                                     num_examples = 0L,
                                     feedback = FALSE,
                                     get_answer = get_answer_pyin_melodic_production,
                                     sound = "piano",
                                     page_type = "record_audio_page",
                                     get_trial_characteristics_function = NULL,
                                     item_characteristics_sampler_function = NULL,
                                     item_characteristics_pars = NULL,
                                     rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
                                     max_goes = 3L,
                                     max_goes_forced = FALSE,
                                     display_modality = "auditory",
                                     show_progress = TRUE,
                                     sheet_music_start_hidden = FALSE,
                                     sound_only_first_melody_note = FALSE,
                                     sheet_music_id = 'sheet_music',
                                     give_first_melody_note = FALSE,
                                     presampled = FALSE,
                                     get_similarity_to_previous_melody = FALSE,
                                     volume_meter = FALSE,
                                     volume_meter_type = 'default',
                                     melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                                     singing_trials = TRUE,
                                     review = FALSE,
                                     phase = c('test', 'learn', 'review'),
                                     first_note_message = "The first note is: ",
                                     transposed_message = "Please note, this is transposed for your instrument.",
                                     play_first_note_button_text = "Play First Note",
                                     learn_test_paradigm = FALSE,
                                     sample_item_bank_via_api = FALSE,
                                     start_from_sampled_trial_no = 1L,
                                     pass_items_through_url_parameter = FALSE,
                                     asynchronous_api_mode = FALSE) {


  melody_trials(var_name,
                module_name,
                page_text,
                page_title,
                instruction_text,
                sampler_function,
                item_bank,
                num_items,
                num_examples,
                feedback,
                get_answer,
                sound,
                page_type,
                get_trial_characteristics_function,
                item_characteristics_sampler_function,
                item_characteristics_pars,
                rel_to_abs_mel_function,
                max_goes,
                max_goes_forced,
                display_modality,
                show_progress,
                sheet_music_start_hidden,
                sound_only_first_melody_note,
                sheet_music_id,
                give_first_melody_note,
                presampled,
                arrhythmic = TRUE,
                get_similarity_to_previous_melody,
                volume_meter,
                volume_meter_type,
                melody_block_paradigm,
                singing_trials,
                review,
                phase,
                first_note_message,
                transposed_message,
                play_first_note_button_text,
                learn_test_paradigm,
                sample_item_bank_via_api,
                start_from_sampled_trial_no,
                pass_items_through_url_parameter,
                asynchronous_api_mode)

}




#' Rhythmic melody trials block
#'
#' @param var_name
#' @param module_name
#' @param page_text
#' @param page_title
#' @param instruction_text
#' @param sampler_function
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_type
#' @param get_trial_characteristics_function
#' @param item_characteristics_sampler_function
#' @param item_characteristics_pars
#' @param rel_to_abs_mel_function
#' @param max_goes
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param presampled
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param melody_block_paradigm
#' @param singing_trials
#' @param review
#' @param phase
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param learn_test_paradigm
#' @param sample_item_bank_via_api
#' @param start_from_sampled_trial_no
#' @param pass_items_through_url_parameter
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
rhythmic_melody_trials <- function(var_name = "rhythmic_melody",
                                   module_name = "rhythmic_melodies",
                                   page_text = psychTestR::i18n("rhythmic_melody_trial_page_text"),
                                   page_title = psychTestR::i18n("rhythmic_melody_trial_page_title"),
                                   instruction_text = psychTestR::i18n("rhythmic_melody_trial_instruction_text"),
                                   sampler_function = sample_rhythmic,
                                   item_bank,
                                   num_items = integer(),
                                   num_examples = 0L,
                                   feedback = FALSE,
                                   get_answer = get_answer_pyin_melodic_production,
                                   sound = "piano",
                                   page_type = "record_audio_page",
                                   get_trial_characteristics_function = NULL,
                                   item_characteristics_sampler_function = NULL,
                                   item_characteristics_pars = NULL,
                                   rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
                                   max_goes = 3L,
                                   max_goes_forced = FALSE,
                                   display_modality = "auditory",
                                   show_progress = TRUE,
                                   sheet_music_start_hidden = FALSE,
                                   sound_only_first_melody_note = FALSE,
                                   sheet_music_id = 'sheet_music',
                                   give_first_melody_note = FALSE,
                                   presampled = FALSE,
                                   get_similarity_to_previous_melody = FALSE,
                                   volume_meter = FALSE,
                                   volume_meter_type = 'default',
                                   melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                                   singing_trials = TRUE,
                                   review = FALSE,
                                   phase = c('test', 'learn', 'review'),
                                   first_note_message = "The first note is: ",
                                   transposed_message = "Please note, this is transposed for your instrument.",
                                   play_first_note_button_text = "Play First Note",
                                   learn_test_paradigm = FALSE,
                                   sample_item_bank_via_api = FALSE,
                                   start_from_sampled_trial_no = 1L,
                                   pass_items_through_url_parameter = FALSE,
                                   asynchronous_api_mode = FALSE) {


  melody_trials(var_name,
                module_name,
                page_text,
                page_title,
                instruction_text,
                sampler_function,
               item_bank,
               num_items,
               num_examples,
               feedback,
               get_answer,
               sound,
               page_type,
               get_trial_characteristics_function,
               item_characteristics_sampler_function,
               item_characteristics_pars,
               rel_to_abs_mel_function,
               max_goes,
               max_goes_forced,
               display_modality,
               show_progress,
               sheet_music_start_hidden,
               sound_only_first_melody_note,
               sheet_music_id,
               give_first_melody_note,
               presampled,
               arrhythmic = FALSE,
               get_similarity_to_previous_melody,
               volume_meter,
               volume_meter_type,
               melody_block_paradigm,
               singing_trials,
               review,
               phase,
               first_note_message,
               transposed_message,
               play_first_note_button_text,
               learn_test_paradigm,
               sample_item_bank_via_api,
               start_from_sampled_trial_no,
               pass_items_through_url_parameter,
               asynchronous_api_mode)

}





#' Melody trials constructor
#'
#' @param var_name
#' @param module_name
#' @param page_text
#' @param page_title
#' @param instruction_text
#' @param sampler_function
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_type
#' @param get_trial_characteristics_function
#' @param item_characteristics_sampler_function
#' @param item_characteristics_pars
#' @param rel_to_abs_mel_function
#' @param max_goes
#' @param max_goes_forced
#' @param display_modality
#' @param show_progress
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param give_first_melody_note
#' @param presampled
#' @param arrhythmic
#' @param get_similarity_to_previous_melody
#' @param volume_meter
#' @param volume_meter_type
#' @param melody_block_paradigm
#' @param singing_trials
#' @param review
#' @param phase
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#' @param learn_test_paradigm
#' @param sample_item_bank_via_api
#' @param start_from_sampled_trial_no
#' @param pass_items_through_url_parameter
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
melody_trials <- function(var_name,
                          module_name,
                          page_text,
                          page_title,
                          instruction_text,
                          sampler_function,
                          item_bank,
                          num_items = integer(),
                          num_examples = 0L,
                          feedback = FALSE,
                          get_answer = get_answer_pyin_melodic_production,
                          sound = "piano",
                          page_type = "record_audio_page",
                          get_trial_characteristics_function = NULL,
                          item_characteristics_sampler_function = NULL,
                          item_characteristics_pars = NULL,
                          rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
                          max_goes = 3L,
                          max_goes_forced = FALSE,
                          display_modality = "auditory",
                          show_progress = TRUE,
                          sheet_music_start_hidden = FALSE,
                          sound_only_first_melody_note = FALSE,
                          sheet_music_id = 'sheet_music',
                          give_first_melody_note = FALSE,
                          presampled = FALSE,
                          arrhythmic = FALSE,
                          get_similarity_to_previous_melody = FALSE,
                          volume_meter = FALSE,
                          volume_meter_type = 'default',
                          melody_block_paradigm = c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
                          singing_trials = TRUE,
                          review = FALSE,
                          phase = c('test', 'learn', 'review'),
                          first_note_message = psychTestR::i18n("first_note_is"),
                          transposed_message = psychTestR::i18n("transposed"),
                          play_first_note_button_text = psychTestR::i18n("play_first_note"),
                          learn_test_paradigm = FALSE,
                          sample_item_bank_via_api = FALSE,
                          start_from_sampled_trial_no = 1L,
                          pass_items_through_url_parameter = FALSE,
                          asynchronous_api_mode = FALSE) {

  phase <- match.arg(phase)


  stopifnot(
    is(item_bank, "item_bank"),
    is.scalar.numeric(num_items) || is.list(num_items) || presampled,
    is.scalar.numeric(num_examples) || is.list(num_examples),
    is.function(feedback) || is.scalar.logical(feedback),
    is.function(get_answer),
    is.scalar.character(sound),
    is.scalar.character(page_text),
    is.scalar.character(page_title),
    is.scalar.character(page_type),
    is.scalar.character(instruction_text),
    is.null.or(get_trial_characteristics_function, is.function),
    is.null.or(item_characteristics_sampler_function, is.function),
    is.null.or(item_characteristics_pars, is.list),
    is.null.or(rel_to_abs_mel_function, is.function),
    is.scalar.numeric(max_goes),
    is.scalar.logical(max_goes_forced),
    display_modality %in% c("auditory", "visual"),
    is.scalar.logical(show_progress),
    is.scalar.character(module_name),
    is.scalar.logical(sheet_music_start_hidden),
    is.scalar.logical(sound_only_first_melody_note),
    is.scalar.character(sheet_music_id),
    is.scalar.logical(give_first_melody_note),
    is.function(sampler_function),
    is.scalar.logical(presampled),
    is.scalar.logical(arrhythmic),
    is.scalar.logical(get_similarity_to_previous_melody),
    is.scalar.logical(volume_meter),
    is.scalar.character(volume_meter_type),
    melody_block_paradigm %in% c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
    is.scalar.logical(singing_trials),
    is.scalar.logical(review),
    phase %in% c('test', 'learn', 'review'),
    is.scalar.character(first_note_message),
    is.scalar.character(transposed_message),
    is.scalar.character(play_first_note_button_text),
    is.scalar.logical(learn_test_paradigm),
    is.scalar.logical(sample_item_bank_via_api),
    is.scalar.numeric(start_from_sampled_trial_no),
    is.scalar.logical(pass_items_through_url_parameter),
    is.scalar.logical(asynchronous_api_mode)
  )

  if(review && phase != 'review') {
    stop("If review is TRUE phase should be 'review")
  }

  if(presampled & ! sample_item_bank_via_api) {
    num_items <- nrow(item_bank)
  }

  if(presampled && num_items > 50) {
    logging::logwarn("presampled is TRUE and num_items is %s. Are you sure that's correct?", num_items)
  }


  if(review) {
    num_examples <- 0L
  }

  # Flatten the number of items for use in some places
  num_examples_flat <- flatten_no_item_list(num_examples)
  num_items_flat <- flatten_no_item_list(num_items)

  # Unclass item bank, so it can work with dplyr
  item_bank <- item_bank %>% tibble::as_tibble()

  if(num_items_flat == 0) {
    return(empty_code_block())
  } else {


    if(!is.function(feedback)) {
      if(feedback) {
        feedback <- feedback_melodic_production
      }
    }

    main_trials <-  multi_page_play_melody_loop(
      presampled_items = if(presampled & ! sample_item_bank_via_api) item_bank else if(pass_items_through_url_parameter) NULL else NULL,
      stimuli_type = "midi_notes",
      var_name = var_name,
      num_items = num_items_flat,
      page_title = page_title,
      page_text = page_text,
      page_type = page_type,
      get_answer = get_answer,
      rel_to_abs_mel_function = rel_to_abs_mel_function,
      feedback = feedback,
      sound = sound,
      get_trial_characteristics_function = get_trial_characteristics_function,
      max_goes_forced = max_goes_forced,
      max_goes = max_goes,
      item_bank = item_bank,
      display_modality = display_modality,
      show_progress = show_progress,
      sheet_music_start_hidden = sheet_music_start_hidden,
      sound_only_first_melody_note = sound_only_first_melody_note,
      sheet_music_id = sheet_music_id,
      give_first_melody_note = give_first_melody_note,
      arrhythmic = arrhythmic,
      singing_trials = singing_trials,
      get_similarity_to_previous_melody = get_similarity_to_previous_melody,
      volume_meter = volume_meter,
      volume_meter_type = volume_meter_type,
      melody_block_paradigm = melody_block_paradigm,
      phase = phase,
      first_note_message = first_note_message,
      transposed_message = transposed_message,
      play_first_note_button_text = play_first_note_button_text,
      learn_test_paradigm = learn_test_paradigm,
      sample_item_bank_via_api = sample_item_bank_via_api,
      start_from_trial_no = if(sample_item_bank_via_api) start_from_sampled_trial_no + num_examples_flat else 1L,
      pass_items_through_url_parameter = pass_items_through_url_parameter)

    if(review) {

      var_name <- paste0(var_name, "_review")

      # If a review block, we wrap these conditional to check there are enough review items for a given user.

      main_trials <- psychTestR::join(

        # Success (IF)
        psychTestR::conditional(function(state, ...) {

        res <- nrow(psychTestR::get_global(var_name, state)) >= num_examples_flat

        if(res) {
          logging::loginfo("There are enough review items.")
        }

        return(res)

      }, main_trials),

      # Failure (ELSE)

      psychTestR::conditional(function(state, ...) {

        res <- nrow(psychTestR::get_global(var_name, state)) < num_examples_flat

        if(res) {
          logging::loginfo("There are not enough review items.")
        }

        return(res)

      }, psychTestR::one_button_page("Sorry, there are not enough items for you to review!"))


      )

    }

    psychTestR::module(module_name,
                    psychTestR::join(

                      if(asynchronous_api_mode) wait_for_api_page(),

                        # Instructions depending on review
                        if(review) psychTestR::one_button_page("Now you will review some melodies you have encountered previously."),
                         # Examples
                         if(is.numeric(num_examples_flat) && num_examples_flat > 0L && !review) {
                           psychTestR::join(

                             # Instructions
                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(paste0(psychTestR::i18n("First_try"), " ", num_examples_flat, " ", psychTestR::i18n("example_trials"), "."))
                             ), button_text = psychTestR::i18n("Next")),

                             ## Sample example items
                             if(!presampled && !pass_items_through_url_parameter) handle_item_sampling(item_bank, num_examples_flat, item_characteristics_sampler_function, item_characteristics_pars, sampler_function, review, var_name, phase, learn_test_paradigm, !arrhythmic),

                             ## Run examples
                             multi_page_play_melody_loop(
                               presampled_items = if(presampled & ! sample_item_bank_via_api) item_bank else if(pass_items_through_url_parameter) NULL else NULL,
                               stimuli_type = "midi_notes",
                               var_name = var_name,
                               num_items = num_examples_flat,
                               page_title = page_title,
                               page_text = page_text,
                               page_type = page_type,
                               get_answer = get_answer,
                               rel_to_abs_mel_function = rel_to_abs_mel_function,
                               example = TRUE,
                               feedback = feedback,
                               sound = sound,
                               get_trial_characteristics_function = get_trial_characteristics_function,
                               max_goes_forced = max_goes_forced,
                               max_goes = max_goes,
                               item_bank = item_bank,
                               display_modality = display_modality,
                               show_progress = show_progress,
                               sheet_music_start_hidden = sheet_music_start_hidden,
                               sound_only_first_melody_note = sound_only_first_melody_note,
                               sheet_music_id = sheet_music_id,
                               give_first_melody_note = give_first_melody_note,
                               arrhythmic = arrhythmic,
                               get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                               volume_meter = volume_meter,
                               volume_meter_type = volume_meter_type,
                               melody_block_paradigm = melody_block_paradigm,
                               singing_trials = singing_trials,
                               phase = "example",
                               first_note_message = first_note_message,
                               transposed_message = transposed_message,
                               play_first_note_button_text = play_first_note_button_text,
                               learn_test_paradigm = learn_test_paradigm,
                               sample_item_bank_via_api = sample_item_bank_via_api,
                               start_from_trial_no = start_from_sampled_trial_no,
                               pass_items_through_url_parameter = pass_items_through_url_parameter),

                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("ready_for_real_thing"))), button_text = psychTestR::i18n("Next"))
                           )
                         },
                         ## Sample items
                        if(!presampled && ! pass_items_through_url_parameter) handle_item_sampling(item_bank, num_items_flat, item_characteristics_sampler_function, item_characteristics_pars, sampler_function, review, var_name, phase, learn_test_paradigm, !arrhythmic),

                        ## Trials
                        main_trials,

                        # At end of block, clear this var, otherwise can lead to issues between trial blocks.
                        # Another option would be to revert everything to set_local.
                        # Should probably do this in future release.
                        psychTestR::code_block(function(state, ...) {
                          psychTestR::set_global(paste0("previous_melodies_", var_name), NULL, state)
                        })
                       )
    )
  }
}


handle_item_sampling <- function(item_bank, num_items_flat, item_characteristics_sampler_function,
                                 item_characteristics_pars, sampler_function, review = FALSE, var_name, phase = "test",
                                 learn_test_paradigm = FALSE, rhythmic = FALSE) {

  if(review) {
    sample_review(num_items_flat, id = var_name, rhythmic = rhythmic)
  } else if(learn_test_paradigm && phase == "test") {

    psychTestR::code_block(function(state, ...) {
      logging::loginfo("Test phase of learn-test paradigm: sampling from learn phase melodies")
      res <- psychTestR::get_global(paste0("previous_melodies_", var_name), state) %>%
        dplyr::slice_sample(n = num_items_flat)
      psychTestR::set_global(var_name, res, state)
    })

  } else {
    if(is.null(item_characteristics_sampler_function)) {
      if(!is.null(sampler_function)) {
        sampler_function(item_bank, num_items_flat)
      }
    } else {
      sample_item_characteristics(var_name = var_name, item_characteristics_sampler_function, item_characteristics_pars)
    }
  }
}



#' Present a block of long tone trials
#'
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param show_instructions
#' @param page_text
#' @param page_title
#' @param get_answer
#' @param page_type
#' @param long_tone_trials_as_screening
#' @param long_tone_trials_as_screening_failure_page
#' @param instruction_text
#' @param module_name
#' @param show_progress
#' @param paradigm
#' @param long_tone_length
#'
#' @return
#' @export
#'
#' @examples
long_tone_trials <- function(num_items,
                             num_examples = 0L,
                             feedback = FALSE,
                             show_instructions = TRUE,
                             page_text = psychTestR::i18n("long_tone_text"),
                             page_title = psychTestR::i18n("long_tone_title"),
                             get_answer = get_answer_pyin_long_note,
                             page_type = "record_audio_page",
                             long_tone_trials_as_screening = FALSE,
                             long_tone_trials_as_screening_failure_page = "http://www.google.com",
                             instruction_text = shiny::div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction")),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction_2")),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction_3"))),
                             module_name = "long_tone_trials",
                             show_progress = TRUE,
                             paradigm = c("simultaneous_recall", "call_and_response"),
                             long_tone_length = 5) {

  if(match.arg(paradigm) == "simultaneous_recall") {
    instruction_text <- shiny::div(
      shiny::tags$h2(page_title),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction")),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction_2")),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction_3")))
  } else if(match.arg(paradigm) == "call_and_response") {
    page_title <- psychTestR::i18n("long_tone_title_call_and_response")
    instruction_text <- shiny::div(
      shiny::tags$h2(page_title),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction_call_and_response")),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction_call_and_response_2")),
      shiny::tags$p(psychTestR::i18n("long_tone_instruction_call_and_response_3")))

  } else {
    stop("Unknown long tone paradigm")
  }



  if(num_items == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {

    if(!is.function(feedback)) {
      if(feedback) {
        feedback <- feedback_long_tone
      }
    }
    # sample melodies based on range
    psychTestR::module(module_name,
                       psychTestR::join(
                         # instructions
                         if(show_instructions) {
                           psychTestR::one_button_page(instruction_text, button_text = psychTestR::i18n("Next"))
                         },
                         # examples
                         if(is.numeric(num_examples) & num_examples > 0L) {
                           psychTestR::join(psychTestR::one_button_page(shiny::div(
                             shiny::tags$h2(page_title),
                             shiny::tags$p(paste0(psychTestR::i18n("First_try"), " ", num_examples, " ", psychTestR::i18n("example_trials"), "."))), button_text = psychTestR::i18n("Next")),
                             sample_from_user_range(num_examples),
                             if(page_type == "reactive") {
                               psychTestR::conditional(function(state, ...) {
                                 psychTestR::get_global("response_type", state) == "MIDI"
                               }, logic = multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_midi_page",
                                                                                  example = TRUE, feedback = feedback,
                                                                                  get_answer = get_answer, trial_paradigm = paradigm,
                                                                                  page_text = page_text, page_title = page_title,
                                                                                  long_tone_length = long_tone_length))

                               psychTestR::conditional(function(state, ...){
                                 psychTestR::get_global("response_type", state) == "Microphone"
                               }, logic = multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_audio_page",
                                                                                  example = TRUE, feedback = feedback, get_answer = get_answer, trial_paradigm = paradigm, long_tone_length = long_tone_length))

                             } else {
                               multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type,
                                                                       example = TRUE, feedback = feedback, get_answer = get_answer,
                                                                       page_text = page_text, page_title = page_title,
                                                                       trial_paradigm = paradigm, long_tone_length = long_tone_length)
                             },
                             psychTestR::one_button_page(shiny::div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("ready_for_real_thing"))), button_text = psychTestR::i18n("Next"))
                           )},
                         # sample
                         sample_from_user_range(num_items),
                         # build pages
                         multi_play_long_tone_record_audio_pages(no_items = num_items,
                                                                 page_type = page_type,
                                                                 feedback = feedback,
                                                                 get_answer = get_answer,
                                                                 page_text = page_text,
                                                                 page_title = page_title,
                                                                 show_progress = show_progress,
                                                                 trial_paradigm = paradigm,
                                                                 long_tone_length = long_tone_length),

                         psychTestR::elt_save_results_to_disk(complete = FALSE),

                         if(long_tone_trials_as_screening) end_of_long_note_trial_screening(long_tone_trials_as_screening_failure_page)

                       )
    )
  }
}




#' Present "Find This Note" trials
#'
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param page_title
#' @param get_answer
#' @param page_type
#' @param page_text
#'
#' @return
#' @export
#'
#' @examples
find_this_note_trials <- function(num_items,
                                  num_examples = 0L,
                                  feedback = FALSE,
                                  page_title = "Find This Note",
                                  get_answer = get_answer_pyin_melodic_production,
                                  page_type = "record_audio_page",
                                  page_text = "Press play to hear the note. Try and play it on your instrument when you can.",
                                  trial_paradigm = c("simultaneous_recall", "call_and_response"),
                                  call_and_response_end = c("manual", "auto"),
                                  singing_trials = FALSE) {

  # Get trial paradigm info
  trial_paradigm <- match.arg(trial_paradigm)
  call_and_response_end <- match.arg(call_and_response_end)
  paradigm <- paradigm(paradigm_type = trial_paradigm, page_type = page_type, call_and_response_end = call_and_response_end)


  if(num_items == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {

    if(!is.function(feedback)) {
      if(feedback) {
        feedback <- feedback_melodic_production
      }
    }

    # Sample melodies based on range
    psychTestR::module("find_this_note_trials",
                       psychTestR::join(
                         # Instructions
                         psychTestR::one_button_page(shiny::div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p("In the next section, you will hear a long note. Try and find it as quickly as you can.")
                         )),
                         # Examples
                         if(is.numeric(num_examples) & num_examples > 0L) {
                           psychTestR::join(psychTestR::one_button_page(shiny::div(
                             shiny::tags$h2(page_title),
                             shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
                             sample_from_user_range(num_examples),
                             multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type,
                                                                     page_text = page_text, page_title = page_title,
                                                                     example = TRUE, feedback = feedback, get_answer = get_answer,
                                                                     trial_paradigm = trial_paradigm,
                                                                     call_and_response_end = call_and_response_end,
                                                                     singing_trial = FALSE),
                             psychTestR::one_button_page(shiny::div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("ready_for_real_thing"))))
                           )},
                         # Sample
                         sample_from_user_range(num_items),
                         # Build pages
                         multi_play_long_tone_record_audio_pages(no_items = num_items,
                                                                 page_text = page_text,
                                                                 page_title = page_title,
                                                                 page_type = page_type,
                                                                 feedback = feedback,
                                                                 get_answer = get_answer,
                                                                 trial_paradigm = trial_paradigm,
                                                                 call_and_response_end = call_and_response_end,
                                                                 singing_trial = FALSE)
                       )
    )
  }
}



#' A trial block which plays back real audio from the Weimar Jazz Database
#'
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param item_length
#' @param get_answer
#' @param sound
#' @param page_text
#' @param page_title
#' @param instruction_text
#'
#' @return
#' @export
#'
#' @examples
wjd_audio_melody_trials <- function(item_bank,
                                    num_items,
                                    num_examples = 0L,
                                    feedback = FALSE,
                                    item_length = c(3,15),
                                    get_answer = get_answer_pyin_melodic_production, sound = "piano",
                                    page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                    page_title = "Play the Melody And Rhythm from Audio",
                                    instruction_text = "Now you will hear some melodies as audio. Please try and play back the melodies and rhythms as best as you can.") {

  stopifnot(is(item_bank, "item_bank"))

  # Declass item bank to work with tidyverse
  item_bank <- tibble::as_tibble(item_bank)

  num_items_flat <- flatten_no_item_list(num_items)
  num_examples_flat <- flatten_no_item_list(num_examples)

  if(num_items_flat == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {

    if(!is.function(feedback)) {
      if(feedback) {
        feedback <- feedback_melodic_production
      }
    }

    intro <- c(
      # instructions
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h2(page_title),
        shiny::tags$p(instruction_text)
      ), button_text = psychTestR::i18n("Next")))

    # Examples
    if(is.numeric(num_examples_flat) & num_examples_flat > 0L) {

      examples <- item_sampler(item_bank, num_examples_flat)

      ## Sample
      example_tl <- psychTestR::join(
        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$h2(page_title),
          shiny::tags$p(paste0("First try ", num_examples_flat, " example trials.")))),

        # Trials
        multi_page_play_melody_loop(
          num_items = num_items,
          presampled_items = if(presampled & ! sample_item_bank_via_api) examples else if(pass_items_through_url_parameter) NULL else NULL,
          var_name = "wjd_audio_melody",
          page_type = "record_audio_page",
          stimuli_type = "audio_WJD",
          page_title = page_title,
          page_text = page_text,
          get_answer = get_answer,
          rel_to_abs_mel_function = leave_relative,
          arrhythmic = TRUE,
          example = TRUE,
          feedback = feedback,
          sound = sound),
        ## sample
        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$h2(page_title),
          shiny::tags$p(psychTestR::i18n("ready_for_real_thing"))), button_text = i18n("Next")))
    } else {
      example_tl <- c()
    }


    ## trials
    trials <- item_sampler(item_bank, num_items_flat)

    trials_tl <- multi_page_play_melody_loop(
      presampled_items = if(presampled & ! sample_item_bank_via_api) trials else if(pass_items_through_url_parameter) NULL else NULL,
      var_name = "wjd_audio_melody",
      stimuli_type = "audio_WJD",
      page_type = "record_audio_page",
      page_title = page_title,
      page_text = page_text,
      get_answer = get_answer,
      rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
      arrhythmic = TRUE,
      feedback = feedback,
      sound = sound)

    psychTestR::module("wjd_audio_melodies",
                       psychTestR::join(intro, example_tl, trials_tl))
  }
}


#' A block which test a participant's perception of intervals
#'
#' @param num_items
#' @param sound
#' @param page_title
#' @param instruction_text
#'
#' @return
#' @export
#'
#' @examples
interval_perception_trials <- function(num_items = 26L,
                                       sound = "piano",
                                       page_title = "What's that interval?",
                                       instruction_text = "In the next set of trials, you will hear a musical interval. You must try and say what the interval is. There are no practice rounds, you will begin immediately.") {

  if(is.numeric(num_items) & num_items > 0L) {
    psychTestR::module("interval_perception",
                       # no examples (too self explanatory/easy)
                       psychTestR::join(
                         psychTestR::one_button_page(shiny::tags$div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p(instruction_text)
                         )),
                         sample_intervals(num_items = num_items),
                         multi_interval_page(num_items)))
  } else {
    c()
  }
}







#'  Present a trial block of melodies from audio files
#'
#' @param audio_directory
#' @param no_to_sample
#' @param module_prefix
#' @param page_title
#' @param page_text
#' @param grab_meta_data
#' @param meta_data_df
#' @param meta_data_lookup_column
#' @param get_answer
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
audio_melodic_production_trials <- function(audio_directory,
                                            no_to_sample = NULL,
                                            module_prefix = "audio_trial",
                                            page_title = psychTestR::i18n("Sing_the_Melody"),
                                            page_text = psychTestR::i18n("sing_melody_page_text"),
                                            grab_meta_data,
                                            meta_data_df,
                                            meta_data_lookup_column,
                                            get_answer = get_answer_pyin_melodic_production,
                                            feedback = FALSE) {

  shiny_prefix <- paste0("audio_", paste(sample(1:9, 20, replace = TRUE), sep="", collapse="")) # NB: for cases where multiple blocks with different directories are used

  shiny::addResourcePath(
    prefix = shiny_prefix, # custom prefix that will be used to reference your directory
    directoryPath = audio_directory
  )

  files_list <- list.files(audio_directory, pattern = "\\.mp3$")

  shiny_files_list <- paste0(shiny_prefix,'/', files_list)

  if(!is.null(no_to_sample)) {
    smp <- sample(x = 1:length(files_list), size = no_to_sample)
    shiny_files_list <- shiny_files_list[smp]
    files_list <- files_list[smp]
  }


  trials <- purrr::map(1:length(shiny_files_list), function(i) {

    shiny_file <- shiny_files_list[i]
    file <- files_list[i]

    if(grab_meta_data) {
      base_file <- basename(file)
      md <- grab_meta_data(meta_data_df, meta_data_lookup_column, base_file)
      md_note <- md %>% dplyr::pull(note) %>% itembankr::str_mel_to_vector()
      md_durations <- md %>% dplyr::pull(durations) %>% itembankr::str_mel_to_vector()

    } else {
      md <- " "
      md_note <- " "
      md_durations <- " "
    }

    page_lab <- paste0(module_prefix, "_", tools::file_path_sans_ext(file))

    psychTestR::reactive_page(function(state, ...) {
      psychTestR::set_global("answer_meta_data", rjson::toJSON(md), state)
      psychTestR::set_global("stimuli", rjson::toJSON(md_note), state)
      psychTestR::set_global("stimuli_durations", rjson::toJSON(md_durations), state)

      present_stimuli(
        stimuli = shiny_file,
        stimuli_type = "audio",
        display_modality = "auditory",
        page_title = page_title,
        page_type = "record_audio_page",
        get_answer = get_answer,
        page_text = shiny::tags$div(set_melodic_stimuli(md_note, md_durations), page_text),
        hideOnPlay = TRUE,
        page_label = page_lab,
        answer_meta_data = rjson::toJSON(md),
        audio_playback_as_single_play_button = TRUE)
    })

  })

  if(feedback) {
    trials <- add_feedback(trials, feedback_melodic_production)
  }

  psychTestR::module(label = module_prefix, trials)

}


#' Grab meta data from a df
#'
#' @param meta_data_df
#' @param lookup_column
#' @param value
#'
#' @return
#' @export
#'
#' @examples
grab_meta_data <- function(meta_data_df, lookup_column, value) {
  lookup_column <- as.name(lookup_column)
  meta_data_df %>% dplyr::filter(!!lookup_column == !!value)
}
