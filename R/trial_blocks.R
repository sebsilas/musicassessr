


#' Arrhythmic Melody Trials Block
#'
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#'
#' @return
#' @export
#'
#' @examples
arrhythmic_melody_trials <- function(item_bank, num_items, num_examples = NULL, feedback = FALSE, get_answer = musicassessr::get_answer_pyin) {

  if(feedback & !is.function(feedback)) {
    print('add feedback_melodic_production')
    feedback <- feedback_melodic_production
  }

  psychTestR::module("arrhythmic_melodies",
    c(
      # instructions
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h2("Sing the Melody"),
        shiny::tags$p("Now you will hear some melodies. Please try and sing the melodies.")
        )),
      # examples
      if(!is.null(num_examples)) {
        ## sample
          c(
            psychTestR::one_button_page(shiny::tags$div(
              shiny::tags$h2("Sing the Melody"),
              shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
            sample_arrhythmic(item_bank, num_examples),
            # trials
            musicassessr::multi_page_play_melody_loop(
              n_items = num_examples,
              var_name = "arrhythmic_melody",
              page_type = "record_audio_page",
              page_title = "Sing the Melody",
              page_text = psychTestR::i18n("sing_melody_trial"),
              get_answer = get_answer,
              rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
              arrhythmic = TRUE,
              example = TRUE,
              feedback = feedback)
          )
      },
      ## sample
    c(
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h2("Sing the Melody"),
        shiny::tags$p("Now you're ready for the real thing!"))),
      sample_arrhythmic(item_bank, num_items),
        ## trials
          musicassessr::multi_page_play_melody_loop(
            n_items = num_items,
            var_name = "arrhythmic_melody",
            page_type = "record_audio_page",
            page_title = "Sing the Melody",
            page_text = psychTestR::i18n("sing_melody_trial"),
            get_answer = get_answer,
            rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
            arrhythmic = TRUE,
            feedback = feedback)
    )
    )
  )
}



#' Rhythmic melody trials block
#'
#' @param item_bank
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
rhythmic_melody_trials <- function(item_bank, num_items, num_examples = NULL, feedback = FALSE,
                                   page_title = "Sing This Melody Plus Rhythm", get_answer = musicassessr::get_answer_pyin) {

  if(feedback & !is.function(feedback)) {
    print('add feedback_melodic_production')
    feedback <- feedback_melodic_production
  }

  psychTestR::module("rhythmic_melodies",
      c(
      # instructions
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h2(page_title),
        shiny::tags$p("Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.")
        )),
      # examples
      if(!is.null(num_examples)) {
        c(
          psychTestR::one_button_page(shiny::tags$div(
            shiny::tags$h2(page_title),
            shiny::tags$p(paste0("First try ", num_examples, " example trials."))
            )),
          sample_rhythmic(item_bank, num_examples),
          musicassessr::multi_page_play_melody_loop(
            stimuli_type = "midi_notes",
            var_name = "rhythmic_melody",
            n_items = num_examples,
            page_title = page_title,
            page_text = psychTestR::i18n("sing_melody_trial"),
            get_answer = get_answer,
            rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
            example = TRUE,
            feedback = feedback)
            )
      },
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h2(page_title),
        shiny::tags$p("Now you're ready for the real thing!"))),
      # sample
      sample_rhythmic(item_bank, num_items),
      ## trials
      musicassessr::multi_page_play_melody_loop(
        stimuli_type = "midi_notes",
        var_name = "rhythmic_melody",
        n_items = num_items,
        page_title = page_title,
        page_text = psychTestR::i18n("sing_melody_trial"),
        get_answer = musicassessr::get_answer_save_aws_key,
        rel_to_abs_mel_function = musicassessr::rel_to_abs_mel_mean_centred,
        feedback = feedback
        )
    )
  )
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
long_tone_trials <- function(num_items, num_examples = NULL, feedback = FALSE,
                             page_title = "Sing Along With This Note", get_answer = musicassessr::get_answer_save_aws_key) {

  if(feedback & !is.function(feedback)) {
    feedback <- feedback_long_tone
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
      if(!is.null(num_examples)) {
        c(psychTestR::one_button_page(shiny::div(
          shiny::tags$h2(page_title),
          shiny::tags$p(paste0("First try ", num_examples, " example trials.")))),
          musicassessr::sample_from_user_range(num_examples),
          musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_examples, page_type = "record_audio_page", example = TRUE, feedback = feedback, get_answer = get_answer)
        )
      },
      psychTestR::one_button_page(shiny::div(
        shiny::tags$h2(page_title),
        shiny::tags$p("Now you're ready for the real thing!"))),
      # sample
      musicassessr::sample_from_user_range(num_items),
      # build pages
      musicassessr::multi_play_long_tone_record_audio_pages(no_items = num_items, page_type = "record_audio_page", feedback = feedback, get_answer = get_answer)
    )
  )
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




