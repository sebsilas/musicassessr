


#' Adaptive arrhythmic melody trials block
#'
#' @param label
#' @param num_items
#' @param item_bank
#' @param model
#' @param fixed_effects
#' @param demo
#' @param page_type
#' @param page_title
#' @param page_text
#' @param get_answer
#' @param give_first_melody_note
#' @param play_melody_loop
#' @param answer_column
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
adaptive_arrhythmic_melody_trials <- function(label,
                                              num_items,
                                              item_bank,
                                              model,
                                              fixed_effects,
                                              demo = FALSE,
                                              page_type = "record_audio_page",
                                              page_title = "Sing back the melody",
                                              page_text = "Sing back the melody",
                                              get_answer = musicassessr::get_answer_pyin_melodic_production,
                                              give_first_melody_note = FALSE,
                                              play_melody_loop = FALSE,
                                              answer_column = "melody",
                                              feedback = FALSE) {

  stopifnot(
    assertthat::is.string(label),
    is.scalar.numeric(num_items),
    is(item_bank, "item_bank"),
    is(model, "lmerModLmerTest"),
    is.character(fixed_effects) & length(fixed_effects) > 0,
    is.logical(demo),
    assertthat::is.string(page_type),
    assertthat::is.string(page_title),
    assertthat::is.string(page_text),
    is.function(get_answer),
    is.logical(give_first_melody_note),
    is.logical(play_melody_loop),
    assertthat::is.string(melody),
    is.logical(feedback)
  )

  item_bank <- item_bank %>%
    dplyr::rename(answer = answer_column) %>%
    dplyr::mutate(discrimination = 1, guessing = 1, inattention = 1)

  psychTestRCATME::adapt_test(label = label,
                              item_bank = item_bank,
                              show_item = show_item_arrhythmic(num_items = num_items,
                                                               page_type = page_type,
                                                               page_title = page_title,
                                                               page_text = page_text,
                                                               get_answer = get_answer,
                                                               give_first_melody_note = give_first_melody_note,
                                                               play_melody_loop = play_melody_loop,
                                                               feedback = feedback),
                              stopping_rule = psychTestRCATME::stopping_rule.num_items(n = num_items),
                              opt = psychTestRCATME::adapt_test_options(
                                next_item.criterion = "bOpt",
                                next_item.estimator = "BM",
                                final_ability.estimator = "WL",
                                mixed_effects_model = model,
                                eligible_first_items = which(dplyr::between(item_bank$difficulty,
                                                                            min(item_bank$difficulty),
                                                                            min(item_bank$difficulty) + sd(item_bank$difficulty))
                                                             & dplyr::between(item_bank$N,
                                                                              min(item_bank$N),
                                                                              min(item_bank$N) + sd(item_bank$N))),
                                continuous_response = TRUE,
                                dv_name = "opti3",
                                fixed_effects = fixed_effects,
                                demo = demo,
                                predict_based_on_mixed_effects_model_function = psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model))

}


show_item_arrhythmic <- function(num_items,
                                 page_type = "record_audio_page",
                                 page_title = "Copy The Melody",
                                 page_text = "Press play to hear the melody, then play it back as best as you can when it finishes.",
                                 get_answer = musicassessr::get_answer_pyin_melodic_production,
                                 give_first_melody_note = FALSE,
                                 play_melody_loop = FALSE,
                                 feedback = FALSE) {

  if(play_melody_loop) {

    p <- psychTestR::new_timeline(

      play_melody_loop(page_title = page_title,
                        page_text = page_text,
                        page_type = page_type,
                        get_answer = get_answer,
                        show_progress = TRUE,
                        total_no_melodies = num_items,
                        give_first_melody_note = give_first_melody_note,
                        psychTestRCAT = TRUE,
                        arrhythmic = TRUE,
                       play_button_text = psychTestR::i18n("Play") ),
      dict = musicassessr_dict)

    if(feedback) {
      p <- add_feedback(p, feedback_melodic_production)
    }
    p


  } else {


    function(item, state, ...) {

      item_number <- psychTestRCATME::get_item_number(item)

      bottom_range <- psychTestR::get_global("bottom_range", state)
      top_range <- psychTestR::get_global("top_range", state)

      melody <- item %>%
        dplyr::pull(answer) %>%
        itembankr::str_mel_to_vector() %>%
        musicassessr:: rel_to_abs_mel_mean_centred(bottom_range, top_range)


        present_stimuli(stimuli = melody,
                        stimuli_type = "midi_notes",
                        display_modality = "auditory",
                        page_title = page_title,
                        page_text = page_text,
                        page_type = page_type,
                        get_answer = get_answer,
                        show_progress = TRUE,
                        melody_no = item_number,
                        total_no_melodies = num_items,
                        give_first_melody_note = give_first_melody_note)
    }

  }

}
