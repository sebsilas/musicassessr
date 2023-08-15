
#' A block of record audio pages
#'
#' @param no_pages
#' @param feedback
#' @param get_answer
#' @param page_text
#' @param page_title
#'
#' @return
#' @export
#'
#' @examples
record_audio_block <- function(no_pages, feedback = NULL,
                               get_answer = get_answer_pyin,
                               page_title = psychTestR::i18n("Record_audio"),
                               page_text = psychTestR::i18n("click_to_record_audio")) {
  pages <- psychTestR::join(

    rep(list(record_audio_page(get_answer = get_answer,
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
#' @param feedback
#' @param get_answer
#' @param page_title
#' @param page_text
#' @param autoInstantiate
#'
#' @return
#' @export
#'
#' @examples
record_midi_block <- function(no_pages,
                              feedback = NULL,
                              get_answer = get_answer_midi,
                              page_title = psychTestR::i18n("Record_audio"),
                              page_text = psychTestR::i18n("click_to_record_audio"),
                              autoInstantiate = TRUE) {

  page <- psychTestR::reactive_page(function(state, ...) {

    midi_device <- psychTestR::get_global("midi_device", state)

    if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(get_answer = get_answer,
                       page_title = page_title,
                       page_text = page_text,
                       midi_device = midi_device,
                       autoInstantiate = autoInstantiate)

    })

  pages <- psychTestR::join(rep(list(page), no_pages))

  if(!is.null(feedback)) {
    pages <- add_feedback(pages, feedback)
  }
  pages
}



#' Sing arrhythmic melody trial block
#'
#' @param item_bank
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
                                          get_answer = get_answer_pyin_melodic_production_additional_measures,
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
                           get_similarity_to_previous_melody)

}




#' Sing rhythmic melody trial block
#'
#' @param item_bank
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
                                        get_answer = get_answer_pyin_melodic_production_additional_measures,
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
#'
#' @return
#' @export
#'
#' @examples
arrhythmic_melody_trials <- function(var_name = "arrhythmic_melody",
                                    module_name = "arrhythmic_melodies",
                                     page_text = "Press play to hear the melody. Play back the melody.",
                                     page_title = "Play this melody.",
                                     instruction_text = "Now you will hear some melodies. Please try and play back the melodies.",
                                     sampler_function = sample_arrhythmic,
                                     item_bank,
                                     num_items = integer(),
                                     num_examples = 0L,
                                     feedback = FALSE,
                                     get_answer = get_answer_pyin_melodic_production_additional_measures,
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
                                     phase = c('test', 'learn', 'review')) {


  # Register trial block

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
                phase)

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
#'
#' @return
#' @export
#'
#' @examples
rhythmic_melody_trials <- function(var_name = "rhythmic_melody",
                                   module_name = "rhythmic_melodies",
                                   page_text = "Press play to hear the melody. Play back the melody plus rhythm.",
                                   page_title = "Play this melody plus rhythm",
                                   instruction_text = "Now you will hear melodies with rhythms. Please try and play the melodies with the correct rhythm.",
                                   sampler_function = sample_rhythmic,
                                   item_bank,
                                   num_items = integer(),
                                   num_examples = 0L,
                                   feedback = FALSE,
                                   get_answer = get_answer_pyin_melodic_production_additional_measures,
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
                                   phase = c('test', 'learn', 'review')) {


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
               phase)

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
                          get_answer = get_answer_pyin_melodic_production_additional_measures,
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
                          phase = c('test', 'learn', 'review')) {


  if(presampled) {
    num_items <- nrow(item_bank)
  }

  if(review) {
    num_examples <- 0L
  }

  phase <- match.arg(phase)


  stopifnot(
    is(item_bank, "item_bank"),
    is.scalar.numeric(num_items) | is.list(num_items) | presampled,
    is.scalar.numeric(num_examples) | is.list(num_examples),
    is.function(feedback) | is.scalar.logical(feedback),
    is.function(get_answer),
    assertthat::is.string(sound),
    assertthat::is.string(page_text),
    assertthat::is.string(page_title),
    assertthat::is.string(page_type),
    assertthat::is.string(instruction_text),
    is.null.or(get_trial_characteristics_function, is.function),
    is.null.or(item_characteristics_sampler_function, is.function),
    is.null.or(item_characteristics_pars, is.list),
    is.null.or(rel_to_abs_mel_function, is.function),
    is.scalar.numeric(max_goes),
    is.scalar.logical(max_goes_forced),
    display_modality %in% c("auditory", "visual"),
    is.scalar.logical(show_progress),
    assertthat::is.string(module_name),
    is.scalar.logical(sheet_music_start_hidden),
    is.scalar.logical(sound_only_first_melody_note),
    assertthat::is.string(sheet_music_id),
    is.scalar.logical(give_first_melody_note),
    is.function(sampler_function),
    is.scalar.logical(presampled),
    is.scalar.logical(arrhythmic),
    is.scalar.logical(get_similarity_to_previous_melody),
    is.scalar.logical(volume_meter),
    assertthat::is.string(volume_meter_type),
    melody_block_paradigm %in% c('standard', 'sing_melody_first', 'learn_phase_visual_display_modality'),
    is.scalar.logical(singing_trials),
    is.scalar.logical(review),
    phase %in% c('test', 'learn', 'review')
  )

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

    psychTestR::module(module_name,
                    psychTestR::join(

                        # Set item bank ID in code block
                        set_item_bank_id(item_bank),
                         # Instructions
                         psychTestR::one_button_page(shiny::tags$div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p(instruction_text)
                         ), button_text = psychTestR::i18n("Next")),
                         # Examples
                         if(is.numeric(num_examples_flat) & num_examples_flat > 0L) {
                           psychTestR::join(
                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(paste0(psychTestR::i18n("First_try"), " ", num_examples_flat, " ", psychTestR::i18n("example_trials"), "."))
                             ), button_text = psychTestR::i18n("Next")),
                             ## Sample example items
                             handle_item_sampling(item_bank, num_items_flat, item_characteristics_sampler_function, item_characteristics_pars, sampler_function, review, id, var_name),
                             ## Run examples
                             multi_page_play_melody_loop(
                               presampled_items = if(presampled) item_bank else NULL,
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
                               phase = "example"),

                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("ready_for_real_thing"))), button_text = psychTestR::i18n("Next"))
                           )
                         },
                         ## Sample items
                         handle_item_sampling(item_bank, num_items_flat, item_characteristics_sampler_function, item_characteristics_pars, sampler_function, review, id, var_name),
                         ## Trials
                         multi_page_play_melody_loop(
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
                           get_similarity_to_previous_melody = get_similarity_to_previous_melody,
                           volume_meter = volume_meter,
                           volume_meter_type = volume_meter_type,
                           melody_block_paradigm = melody_block_paradigm,
                           phase = phase
                           )
                       )
    )
  }
}



handle_item_sampling <- function(item_bank, num_items_flat, item_characteristics_sampler_function, item_characteristics_pars, sampler_function, review = FALSE, id, var_name) {
  if(review) {
    sample_review(num_review_items, id = id)
  } else {
    if(is.null(item_characteristics_sampler_function)) {
      if(!is.null(sampler_function)) sampler_function(item_bank, num_items_flat)
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
                             paradigm = c("sing_along", "call_and_response"),
                             long_tone_length = 5) {

  if(match.arg(paradigm) == "sing_along") {
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
                                                                                  example = TRUE, feedback = feedback, get_answer = get_answer, paradigm = paradigm, long_tone_length = long_tone_length))

                             } else {
                               multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type,
                                                                       example = TRUE, feedback = feedback, get_answer = get_answer,
                                                                       page_text = page_text, page_title = page_title,
                                                                       paradigm = paradigm, long_tone_length = long_tone_length)
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
                                                                 paradigm = paradigm,
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
                                  call_and_response_end = c("manual", "auto")) {

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

    # sample melodies based on range
    psychTestR::module("find_this_note_trials",
                       psychTestR::join(
                         # Set item bank ID in code block
                         set_item_bank_id(NA),
                         # Instructions
                         psychTestR::one_button_page(shiny::div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p("In the next section, you will hear a long note. Try and find it as quickly as you can.")
                         )),
                         # Examples
                         if(is.numeric(num_examples) & num_examples > 0L) {
                           c(psychTestR::one_button_page(shiny::div(
                             shiny::tags$h2(page_title),
                             shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
                             sample_from_user_range(num_examples),
                             multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type,
                                                                      page_text = page_text, page_title = page_title,
                                                                      example = TRUE, feedback = feedback, get_answer = get_answer,
                                                                     trial_paradigm = trial_paradigm,
                                                                     call_and_response_end = call_and_response_end,
                                                                     singing_trial = FALSE
                                                                     ),
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
                                    get_answer = get_answer_pyin_melodic_production_additional_measures, sound = "piano",
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

    # examples
    if(is.numeric(num_examples_flat) & num_examples_flat > 0L) {

      examples <- item_sampler(item_bank, num_examples_flat)

      ## sample
      example_tl <- psychTestR::join(
        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$h2(page_title),
          shiny::tags$p(paste0("First try ", num_examples_flat, " example trials.")))),

        # trials
        multi_page_play_melody_loop(
          num_items = num_items,
          presampled_items = examples,
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
      presampled_items = trials,
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
                                            get_answer = get_answer_pyin_melodic_production_additional_measures,
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
        auto_next_page = TRUE,
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


set_item_bank_id <- function(item_bank) {
  psychTestR::code_block(function(state, ...) {

    if(!is.null(attributes(item_bank)$item_bank_name)) {

      if (attributes(item_bank)$item_bank_name == "Berkowitz" && attributes(item_bank)$item_bank_type == "phrase") {
        psychTestR::set_global('item_bank_id', 1L, state)
      }

      if (attributes(item_bank)$item_bank_name == "Berkowitz" && attributes(item_bank)$item_bank_type == "ngram") {
        psychTestR::set_global('item_bank_id', 2L, state)
      }

      if (attributes(item_bank)$item_bank_name == "WJD" && attributes(item_bank)$item_bank_type == "phrase") {
        psychTestR::set_global('item_bank_id', 3L, state)
      }

      if (attributes(item_bank)$item_bank_name == "WJD" && attributes(item_bank)$item_bank_type == "ngram") {
        psychTestR::set_global('item_bank_id', 4L, state)
      }
    }


  })
}
