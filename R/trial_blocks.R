

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
#' @param give_first_melody_note
#'
#' @return
#' @export
#'
#' @examples
sing_arrhythmic_melody_trials <- function(item_bank, num_items, num_examples = 0L, feedback = FALSE,
                                     get_answer = get_answer_pyin_melodic_production, sound = "piano",
                                     page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                     page_title = "Sing the Melody",
                                     page_type = "record_audio_page",
                                     instruction_text = "Now you will hear some melodies. Please try and sing the melodies.",
                                     get_trial_characteristics_function = NULL,
                                     item_characteristics_sampler_function = NULL,
                                     item_characteristics_pars = NULL,
                                     rel_to_abs_mel_function = musicassessr:: rel_to_abs_mel_mean_centred,
                                     max_goes = 3L,
                                     max_goes_forced = FALSE,
                                     give_first_melody_note = FALSE) {

  arrhythmic_melody_trials(item_bank,
                           num_items,
                           num_examples,
                           feedback,
                           get_answer, sound,
                           page_text,
                           page_title,
                           page_type,
                           instruction_text,
                           get_trial_characteristics_function,
                           item_characteristics_sampler_function ,
                           item_characteristics_pars,
                           rel_to_abs_mel_function,
                           max_goes,
                           max_goes_forced,
                           give_first_melody_note)

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
#' @param give_first_melody_note
#'
#' @return
#' @export
#'
#' @examples
sing_rhythmic_melody_trials <- function(item_bank,
                                        num_items,
                                        num_examples = 0L,
                                        feedback = FALSE,
                                        get_answer = get_answer_pyin_melodic_production,
                                        sound = "piano",
                                        page_text = psychTestR::i18n("sing_melody_trial"),
                                        page_title = "Sing This Melody Plus Rhythm",
                                        page_type = "record_audio_page",
                                        instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.",
                                        get_trial_characteristics_function = NULL,
                                        item_characteristics_sampler_function = NULL,
                                        item_characteristics_pars = NULL,
                                        rel_to_abs_mel_function = musicassessr:: rel_to_abs_mel_mean_centred,
                                        max_goes = 3L,
                                        max_goes_forced = FALSE,
                                        give_first_melody_note = FALSE) {

  rhythmic_melody_trials(item_bank,
                         num_items,
                         num_examples,
                         feedback,
                         get_answer,
                         sound,
                         page_text,
                         page_title,
                         page_type,
                         instruction_text,
                         get_trial_characteristics_function,
                         item_characteristics_sampler_function,
                         item_characteristics_pars,
                         rel_to_abs_mel_function,
                         max_goes,
                         max_goes_forced,
                         give_first_melody_note)

}

#' Arrhythmic Melody Trials Block
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
#' @param give_first_melody_note
#'
#' @return
#' @export
#'
#' @examples
arrhythmic_melody_trials <- function(item_bank, num_items, num_examples = 0L, feedback = FALSE,
                                     get_answer = get_answer_pyin_melodic_production, sound = "piano",
                                     page_text = "Click below to hear the melody. Play back the melody. Click Stop when finished.",
                                     page_title = "Play the Melody",
                                     page_type = "record_audio_page",
                                     instruction_text = "Now you will hear some melodies. Please try and play the melodies.",
                                     get_trial_characteristics_function = NULL,
                                     item_characteristics_sampler_function = NULL,
                                     item_characteristics_pars = NULL,
                                     rel_to_abs_mel_function = musicassessr:: rel_to_abs_mel_mean_centred,
                                     max_goes = 3L,
                                     max_goes_forced = FALSE,
                                     give_first_melody_note = FALSE) {


  if(num_items == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {

    if(!is.function(feedback)) {
      if(feedback) {
        feedback <- feedback_melodic_production
      }
    }

    psychTestR::module("arrhythmic_melodies",
                       c(
                         # instructions
                         psychTestR::one_button_page(shiny::tags$div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p(instruction_text)
                         )),
                         # examples
                         if(is.numeric(num_examples) & num_examples > 0L) {
                           ## sample
                           c(
                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
                             if(is.null(item_characteristics_sampler_function)) {
                               sample_arrhythmic(item_bank, num_examples)
                             } else {
                               sample_item_characteristics(var_name = "arrhythmic_melody",
                                                           item_characteristics_sampler_function,
                                                           item_characteristics_pars)
                             },
                             # trials
                             musicassessr::multi_page_play_melody_loop(
                               n_items = ifelse(is.list(num_examples), sum(unlist(num_examples)), num_examples),
                               var_name = "arrhythmic_melody",
                               page_type = page_type,
                               page_title = page_title,
                               page_text = page_text,
                               get_answer = get_answer,
                               rel_to_abs_mel_function = rel_to_abs_mel_function,
                               arrhythmic = TRUE,
                               example = TRUE,
                               feedback = feedback,
                               sound = sound,
                               get_trial_characteristics_function = get_trial_characteristics_function,
                               max_goes_forced = max_goes_forced,
                               max_goes = max_goes,
                               give_first_melody_note = give_first_melody_note,
                               item_bank = item_bank),
                             ## sample
                             psychTestR::one_button_page(shiny::tags$div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p("Now you're ready for the real thing!")))
                           )},
                         if(is.null(item_characteristics_sampler_function)) {
                           sample_arrhythmic(item_bank, num_items)
                         } else {
                           sample_item_characteristics(var_name = "arrhythmic_melody",
                                                       item_characteristics_sampler_function,
                                                       item_characteristics_pars)
                         },
                         ## trials
                         musicassessr::multi_page_play_melody_loop(
                           n_items = ifelse(is.list(num_items), sum(unlist(num_items)), num_items),
                           var_name = "arrhythmic_melody",
                           page_type = page_type,
                           page_title = page_title,
                           page_text = page_text,
                           get_answer = get_answer,
                           rel_to_abs_mel_function = rel_to_abs_mel_function,
                           arrhythmic = TRUE,
                           feedback = feedback,
                           sound = sound,
                           get_trial_characteristics_function = get_trial_characteristics_function,
                           max_goes_forced = max_goes_forced,
                           max_goes = max_goes,
                           give_first_melody_note = give_first_melody_note,
                           item_bank = item_bank)
                       )
    )
  }
}







#' Rhythmic melody trials block
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
#' @param give_first_melody_note
#'
#' @return
#' @export
#'
#' @examples
rhythmic_melody_trials <- function(item_bank,
                                   num_items,
                                   num_examples = 0L,
                                   feedback = FALSE,
                                   get_answer = get_answer_pyin_melodic_production,
                                   sound = "piano",
                                   page_text = "Press play to hear the melody. Play back the melody plus rhythm.",
                                   page_title = "Play This Melody Plus Rhythm",
                                   page_type = "record_audio_page",
                                   instruction_text = "Now you will hear melodies with rhythms. Please try and play the melodies with the correct rhythm.",
                                   get_trial_characteristics_function = NULL,
                                   item_characteristics_sampler_function = NULL,
                                   item_characteristics_pars = NULL,
                                   rel_to_abs_mel_function = musicassessr:: rel_to_abs_mel_mean_centred,
                                   max_goes = 3L,
                                   max_goes_forced = FALSE,
                                   give_first_melody_note = FALSE) {

  if(num_items == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {


  if(!is.function(feedback)) {
    if(feedback) {
      feedback <- feedback_melodic_production
    }
  }

    psychTestR::module("rhythmic_melodies",
        c(
        # instructions
        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$h2(page_title),
          shiny::tags$p(instruction_text)
          )),
        # examples
        if(is.numeric(num_examples) & num_examples > 0L) {
          c(
            psychTestR::one_button_page(shiny::tags$div(
              shiny::tags$h2(page_title),
              shiny::tags$p(paste0("First try ", num_examples, " example trials."))
              )),
            if(is.null(item_characteristics_sampler_function)) {
              sample_rhythmic(item_bank, num_examples)
            } else {
              sample_item_characteristics(var_name = "rhythmic_melody",
                                          item_characteristics_sampler_function,
                                          item_characteristics_pars)
            },
            musicassessr::multi_page_play_melody_loop(
              stimuli_type = "midi_notes",
              var_name = "rhythmic_melody",
              n_items = ifelse(is.list(num_examples), sum(unlist(num_examples)), num_examples),
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
              give_first_melody_note = give_first_melody_note,
              item_bank = item_bank),
        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$h2(page_title),
          shiny::tags$p("Now you're ready for the real thing!")))
          )
        },
        # sample
        if(is.null(item_characteristics_sampler_function)) {
          sample_rhythmic(item_bank, num_items)
        } else {
          sample_item_characteristics(var_name = "rhythmic_melody",
                                      item_characteristics_sampler_function,
                                      item_characteristics_pars)
        },
        ## trials
        musicassessr::multi_page_play_melody_loop(
          stimuli_type = "midi_notes",
          var_name = "rhythmic_melody",
          n_items = ifelse(is.list(num_items), sum(unlist(num_items)), num_items),
          page_title = page_title,
          page_text = page_text,
          page_type = page_type,
          get_answer = get_answer,
          rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
          feedback = feedback,
          sound = sound,
          get_trial_characteristics_function = get_trial_characteristics_function,
          max_goes_forced = max_goes_forced,
          max_goes = max_goes,
          give_first_melody_note = give_first_melody_note,
          item_bank = item_bank)
      )
    )
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
                             page_title = "Sing Along With This Note",
                             get_answer = get_answer_pyin_long_note,
                             page_type = "record_audio_page",
                             long_tone_trials_as_screening = FALSE,
                             long_tone_trials_as_screening_failure_page = "http://www.google.com",
                             instruction_text = shiny::div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction")),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction_2")),
                               shiny::tags$p(psychTestR::i18n("long_tone_instruction_3"))),
                             module_name = "long_tone_trials") {

  print('long_tone_trials_as_screening?')
  print(long_tone_trials_as_screening)

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
          psychTestR::one_button_page(instruction_text)
          },
        # examples
        if(is.numeric(num_examples) & num_examples > 0L) {
          psychTestR::join(psychTestR::one_button_page(shiny::div(
            shiny::tags$h2(page_title),
            shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
            musicassessr::sample_from_user_range(num_examples),
            if(page_type == "reactive") {
              psychTestR::conditional(function(state, ...) {
                psychTestR::get_global("response_type", state) == "MIDI"
              }, logic = musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_midi_page",
                                                                               example = TRUE, feedback = feedback,
                                                                               get_answer = get_answer_midi_melodic_production,
                                                                               page_text = page_text, page_title = page_title))

              psychTestR::conditional(function(state, ...){
                psychTestR::get_global("response_type", state) == "Microphone"
              }, logic = musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_audio_page", example = TRUE, feedback = feedback, get_answer = get_answer_pyin_melodic_production))

            } else {
              musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type,
                                                                    example = TRUE, feedback = feedback, get_answer = get_answer,
                                                                    page_text = page_text, page_title = page_title)
            },
        psychTestR::one_button_page(shiny::div(
          shiny::tags$h2(page_title),
          shiny::tags$p("Now you're ready for the real thing!")))
        )},
        # sample
        musicassessr::sample_from_user_range(num_items),
        # build pages
        musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_items,
                                                              page_type = page_type,
                                                              feedback = feedback,
                                                              get_answer = get_answer,
                                                              page_text = page_text,
                                                              page_title = page_title),

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
find_this_note_trials <- function(num_items, num_examples = 0L,
                                  feedback = FALSE,
                                 page_title = "Find This Note",
                                 get_answer = get_answer_pyin_melodic_production,
                                 page_type = "record_audio_page",
                                 page_text = "Press play to hear the note. Try and play it on your instrument when you can.") {

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
                       c(
                         # instructions
                         psychTestR::one_button_page(shiny::div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p("In the next section, you will hear a long note. Try and find it as quickly as you can.")
                         )),
                         # examples
                         if(is.numeric(num_examples) & num_examples > 0L) {
                           c(psychTestR::one_button_page(shiny::div(
                             shiny::tags$h2(page_title),
                             shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
                             musicassessr::sample_from_user_range(num_examples),
                             musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_audio_page",
                                                                                   page_text = page_text, page_title = page_title,
                                                                                   example = TRUE, feedback = feedback, get_answer = get_answer),
                             psychTestR::one_button_page(shiny::div(
                               shiny::tags$h2(page_title),
                               shiny::tags$p("Now you're ready for the real thing!")))
                           )},
                         # sample
                         musicassessr::sample_from_user_range(num_items),
                         # build pages
                         musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_items,
                                                                               page_text = page_text,
                                                                               page_title = page_title,
                                                                               page_type = "record_audio_page",
                                                                               feedback = feedback,
                                                                               get_answer = get_answer)
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
wjd_audio_melody_trials <- function(item_bank, num_items, num_examples = 0L, feedback = FALSE,
                                    item_length = c(3,15), get_answer = get_answer_pyin_melodic_production, sound = "piano",
                                    page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                    page_title = "Play the Melody And Rhythm from Audio",
                                    instruction_text = "Now you will hear some melodies as audio. Please try and play back the melodies and rhythms as best as you can.") {


  if(num_items == 0) {
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
   )))

   # examples
   if(is.numeric(num_examples) & num_examples > 0L) {

     examples <- item_sampler(itembankr::subset_item_bank(item_bank, item_length = item_length), num_examples)

     ## sample
     example_tl <- psychTestR::join(
       psychTestR::one_button_page(shiny::tags$div(
         shiny::tags$h2(page_title),
         shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),

       # trials
       musicassessr::multi_page_play_melody_loop(
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
         shiny::tags$p("Now you're ready for the real thing!"))))
     } else {
       example_tl <- c()
     }


   ## trials
  trials <- item_sampler(itembankr::subset_item_bank(item_bank, item_length = item_length), sum(unlist(num_items)))

   trials_tl <- musicassessr::multi_page_play_melody_loop(
     presampled_items = trials,
     var_name = "wjd_audio_melody",
     stimuli_type = "audio_WJD",
     page_type = "record_audio_page",
     page_title = page_title,
     page_text = page_text,
     get_answer = get_answer,
     rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
     arrhythmic = TRUE,
     feedback = feedback,
     sound = sound)

   psychTestR::module("wjd_audio_melodies",
                      psychTestR::join(intro, example_tl, trials_tl))
  }
}


#' A block which test a participant's perception of intervals
#'
#' @param n_items
#' @param sound
#' @param page_title
#' @param instruction_text
#'
#' @return
#' @export
#'
#' @examples
interval_perception_trials <- function(n_items = 26L, sound = "piano",
                                       page_title = "What's that interval?",
                                       instruction_text = "In the next set of trials, you will hear a musical interval. You must try and say what the interval is. There are no practice rounds, you will begin immediately.") {

  if(is.numeric(n_items) & n_items > 0L) {
      psychTestR::module("interval_perception",
                         # no examples (too self explanatory/easy)
                         psychTestR::join(
                           psychTestR::one_button_page(shiny::tags$div(
                           shiny::tags$h2(page_title),
                           shiny::tags$p(instruction_text)
                         )),
                          sample_intervals(num_items = n_items),
                          multi_interval_page(n_items)))
  } else {
    c()
  }
}








#' Present a trial block of melodies from audio files
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
#'
#' @return
#' @export
#'
#' @examples
audio_melodic_production_trials <- function(audio_directory,
                                           no_to_sample = NULL,
                                           module_prefix = "audio_trial",
                                           page_title = "Sing back the melody",
                                           page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                                           grab_meta_data,
                                           meta_data_df,
                                           meta_data_lookup_column,
                                           get_answer = musicassessr::get_answer_pyin_melodic_production) {

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
      print('grab_md')
      base_file <- basename(file)
      print(base_file)
      md <- grab_meta_data(meta_data_df, meta_data_lookup_column, base_file)
      print(md)
      md_note <- md %>% dplyr::pull(note) %>% itembankr::str_mel_to_vector()
      print(md_note)
      md_durations <- md %>% dplyr::pull(durations) %>% itembankr::str_mel_to_vector()
      print(md_durations)

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

        musicassessr::present_stimuli(
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

  trials_with_feedback <- add_feedback(trials, feedback_melodic_production)

  psychTestR::module(label = module_prefix, trials_with_feedback)

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


# abstracted way of defining trials:: WIP

# arrhythmic_melody_trials <- function(item_bank,
#                                      num_items,
#                                      num_examples = 2,
#                                      feedback) {
#   music_module(module_name = "arrhythmic_melody_trials",
#                intro_text = "Now you will hear some melodies. Please try and sing the melodies.",
#                build_page_fun = multi_page_play_melody_until_satisfied_loop,
#                sample_fun = sample_arrhythmic,
#                feedback_fun = feedback_melodic_production, ...)
# }

# music_module <- function(module_name, intro_text, build_page_fun, sample_fun, feedback_fun, ...) {
#
#     if(feedback & !is.function(feedback)) {
#       feedback <- feedback_fun
#     }
#
#     psychTestR::module(module_name,
#                        c(
#                          # instructions
#                          psychTestR::one_button_page(intro_text),
#                          # examples
#                          if(!is.null(num_examples)) {
#                            build_page_fun(...)
#                          },
#                          ## sample
#                          sample_fun(...),
#                          ## trials
#                          build_page_fun(...)
#                        )
#     )
# }




