

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
#' @param no_attempts
#' @param max_goes_forced
#'
#' @return
#' @export
#'
#' @examples
arrhythmic_melody_trials <- function(item_bank, num_items, num_examples = 0L, feedback = FALSE,
                                     get_answer = get_answer_pyin_melodic_production, sound = "piano",
                                     page_text = psychTestR::i18n("sing_melody_trial"),
                                     page_title = "Sing the Melody",
                                     page_type = "record_audio_page",
                                     instruction_text = "Now you will hear some melodies. Please try and sing the melodies.",
                                     get_trial_characteristics_function = NULL,
                                     item_characteristics_sampler_function = NULL,
                                     item_characteristics_pars = NULL,
                                     rel_to_abs_mel_function = musicassessr:: rel_to_abs_mel_mean_centred,
                                     no_attempts = 3L,
                                     max_goes_forced = FALSE) {

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
                               get_trial_characteristics_function = get_trial_characteristics_function),
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
                           get_trial_characteristics_function = get_trial_characteristics_function)
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
#' @param no_attempts
#' @param max_goes_forced
#'
#' @return
#' @export
#'
#' @examples
rhythmic_melody_trials <- function(item_bank, num_items, num_examples = 0L, feedback = FALSE,
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
                                   no_attempts = 3L,
                                   max_goes_forced = FALSE) {

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
              get_trial_characteristics_function = get_trial_characteristics_function),
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
          get_trial_characteristics_function = get_trial_characteristics_function)
      )
    )
  }
}


#' Present a block of long tone trials
#'
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param page_title
#' @param get_answer
#'
#' @return
#' @export
#'
#' @examples
long_tone_trials <- function(num_items, num_examples = 0L, feedback = FALSE,
                             page_title = "Sing Along With This Note",
                             get_answer = get_answer_pyin_long_note,
                             page_type = "record_audio_page") {

  if(num_items == 0) {
    return(psychTestR::code_block(function(state, ...) { }))
  } else {

  if(!is.function(feedback)) {
    if(feedback) {
      feedback <- feedback_long_tone
    }
  }
    # sample melodies based on range
    psychTestR::module("long_note_trials",
      c(
        # instructions
        psychTestR::one_button_page(shiny::div(
          shiny::tags$h2(page_title),
          shiny::tags$p("Now you will hear some long tones. Please try and sing the exact same note along with the tone as soon as you can. It will last 5 seconds.")
          )),
        # examples
        if(is.numeric(num_examples) & num_examples > 0L) {
          c(psychTestR::one_button_page(shiny::div(
            shiny::tags$h2(page_title),
            shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
            musicassessr::sample_from_user_range(num_examples),
            if(page_type == "reactive") {
              psychTestR::conditional(function(state, ...) {
                psychTestR::get_global("response_type", state) == "MIDI"
              }, logic = musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_midi_page", example = TRUE, feedback = feedback, get_answer = get_answer_midi_melodic_production))

              psychTestR::conditional(function(state, ...){
                psychTestR::get_global("response_type", state) == "Microphone"
              }, logic = musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_audio_page", example = TRUE, feedback = feedback, get_answer = get_answer_pyin_melodic_production))

            } else {
              musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = page_type, example = TRUE, feedback = feedback, get_answer = get_answer)
            },
        psychTestR::one_button_page(shiny::div(
          shiny::tags$h2(page_title),
          shiny::tags$p("Now you're ready for the real thing!")))
        )},
        # sample
        musicassessr::sample_from_user_range(num_items),
        # build pages
        musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_items, page_type = page_type, feedback = feedback, get_answer = get_answer)
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




