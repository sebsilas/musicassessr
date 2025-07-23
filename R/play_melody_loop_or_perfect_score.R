

create_play_melody_loop_or_perfect_score_block <- function(num_items,
                                                           page_type = "record_audio_page",
                                                           max_attempts = 4L) {

  purrr::map(1:num_items, function(trial_no) {

    purrr::map(1:max_attempts, function(attempt_no) {

        play_melody_loop_or_perfect_score(attempt = attempt_no,
                                          page_type = page_type,
                                          trial_no = trial_no,
                                          max_attempts = max_attempts,
                                          no_trials = num_items)

    })
  }) %>% unlist()

}

play_melody_loop_or_perfect_score <- function(stimuli = c(60, 61, 62, 63),
                                              stimuli_durations = rep(0.5, 4),
                                              attempt = 1L,
                                              item_id = "item_id",
                                              rhythmic = TRUE,
                                              page_type = "record_audio_page",
                                              max_attempts = 3L,
                                              trial_no = 1L,
                                              no_trials = 1L) {

  trial_paradigm <- paradigm(page_type = page_type)


  protocol <- psychTestR::module("play_melody_loop_or_perfect_score_protocol",
                                 psychTestR::join(

                                   # Reset score
                                   psychTestR::code_block(function(state, ...) {
                                     psychTestR::set_global("latest_score", NULL, state)
                                   }),

                                   # Trial page

                                   psychTestR::reactive_page(function(state, ...) {

                                     psychTestR::set_global("trial_time_started", Sys.time(), state)
                                     page_label <- paste0("pbet_longitudinal_trial_no_", trial_no, "_attempt_", attempt)

                                     trial_dat <- psychTestR::get_global("longitudinal_items", state) %>%
                                       dplyr::slice(trial_no)

                                     stimuli <- itembankr::str_mel_to_vector( trial_dat$abs_melody )
                                     stimuli_durations = itembankr::str_mel_to_vector( trial_dat$durations )
                                     item_id <- trial_dat$item_id

                                     rhythmic <- is_rhythmic(stimuli_durations)

                                     db_vars <- create_db_vars_template(attempt = attempt,
                                                                        item_id = item_id,
                                                                        rhythmic = rhythmic,
                                                                        additional = list(
                                                                              longitudinal_participant_no = psychTestR::get_global("longitudinal_participant_no", state),
                                                                              longitudinal_test_session_no = psychTestR::get_global("longitudinal_test_session_no", state)
                                                                        ),
                                                                        feedback = TRUE,
                                                                        feedback_type = "opti3",
                                                                        page_label = page_label,
                                                                        instrument = psychTestR::get_global("inst", state),
                                                                        session_id = get_promise_value( psychTestR::get_global("session_id", state) ),
                                                                        user_id = psychTestR::get_global("user_id", state))

                                     present_stimuli(
                                       page_label = page_label,
                                       stimuli = stimuli,
                                       give_first_melody_note = TRUE,
                                       stimuli_durations = stimuli_durations,
                                       display_modality = "auditory",
                                       stimuli_type = "midi_notes",
                                       page_title = psychTestR::i18n("Play_the_melody_by_ear"),
                                       page_text = shiny::tags$div(
                                         shiny::tags$h3(paste0(
                                           psychTestR::i18n("Section_Progress"),
                                           ': ',
                                           trial_no,
                                           "/",
                                           no_trials
                                         )),
                                         shiny::tags$h4(paste0(psychTestR::i18n("Attempt"), " ", attempt, "/", max_attempts)),
                                         set_melodic_stimuli(stimuli, stimuli_durations)
                                       ),
                                       get_answer = get_answer_async_midi_vs_audio,
                                       trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
                                       db_vars = db_vars,
                                       page_type = page_type)

                                   }),

                                   # Feedback handler page
                                   midi_or_audio_reactive(feedback_handler_page_midi(state, attempt, label = paste0("feedback_", trial_no, "_attempt_", attempt)),
                                                          feedback_handler_page_audio(state, attempt, label = paste0("feedback_", trial_no, "_attempt_", attempt), stimuli, stimuli_durations))



                                 )
  )

  if(attempt > 1L) {
    protocol <- psychTestR::conditional(function(state, ...) {

      latest_score <- psychTestR::get_global("latest_score", state)

      logging::loginfo("latest_score: %s", latest_score)

      if(length(latest_score) == 0) {
        return(TRUE)
      }

      if(dplyr::near(latest_score, 10)) {
        return(FALSE)
      } else {
        return(TRUE)
      }

    }, protocol)
  }

  return(protocol)

}


feedback_handler_page_midi <- function(state, attempt, label) {
  psychTestR::reactive_page(function(state, ...) {

    res_prod <- get_latest_production_data(state)
    participant_recall <- res_prod$pyin_style_res$note
    participant_recall_durations <- res_prod$pyin_style_res$dur
    stimuli <- res_prod$stimuli
    stimuli_durations <- res_prod$stimuli_durations

    rate_similarity(label = label,
                    labels = c("Not good", "Quite good", "Very good", "Perfect"),
                    prompt = shiny::tags$div(

                      shiny::tags$h3("Assessment"),

                      shiny::tags$p("Now listen to the melody again, and your performance."),

                      present_stimuli(
                        stimuli = stimuli,
                        stimuli_durations = stimuli_durations,
                        display_modality = "auditory",
                        stimuli_type = "midi_notes",
                        play_button_text = "Hear Melody Again",
                        trigger_end_of_stimulus_fun = wrap_js_fun_body('document.getElementById("audio_feedback_wrapper").style.display = "block";')
                      ),



                      # Recall

                      shiny::tags$div(id = "audio_feedback_wrapper",
                                      style = "display: none;",
                                      shiny::tags$p(id = "response_again", "And now here is what you played:"),
                                      present_stimuli(
                                        stimuli = participant_recall,
                                        stimuli_durations = participant_recall_durations,
                                        display_modality = "auditory",
                                        stimuli_type = "midi_notes",
                                        play_button_text = "Hear My Response Again",
                                        play_button_id = "hear_again",
                                        trigger_end_of_stimulus_fun = wrap_js_fun_body("document.getElementById('next').style.display = 'block';
                                          document.getElementById('hear_again').style.display = 'none';
                                          document.getElementById('response_again').style.display = 'none';
                                          document.querySelectorAll('.slider-container').forEach(el => {
                                              el.style.display = 'block';
                                            });")
                                      )
                      )
                    ),
                    trigger_button_style = "display: none",
                    get_answer = function(input, state, ...) {

                      res_prod <- get_latest_production_data(state)


                      user_input_as_pyin <- res_prod$pyin_style_res

                      stimuli_df <- create_stimuli_df(stimuli, stimuli_durations)

                      feedback <- opti3_df(melody1 = stimuli_df, melody2 = user_input_as_pyin)

                      logging::loginfo("names(feedback): %s", names(feedback) )
                      logging::loginfo("feedback: %s", feedback)

                      psychTestR::set_global("latest_score", feedback, state)

                      list(
                        feedback = feedback$opti3,
                        slider_rating = input$slider
                      )
                    },
                    slider_start_hidden = TRUE,
                    after_table_ui = shiny::tags$p("How would you rate your performance having listened back?")
    )

  })
}

get_latest_production_data <- function(state) {
  answer <- psychTestR::answer(state)
  res <- psychTestR::results(state)$production_reflection_perception_paradigm_protocol

  res_prod <- res[grepl("production", names(res))]

  # Take the latest trial
  res_prod <- res_prod[length(res_prod)][[1]]
}

benevolent_score2 <- function(score) {
  benevolentScore <- (score * 10) + score
  benevolentScore <- round(benevolentScore)
  if (benevolentScore > 10) {
    benevolentScore <- 10
  }
  return(benevolentScore)
}

feedback_handler_page_audio <- function(state, attempt, label, stimuli, stimuli_durations) {
  psychTestR::reactive_page(function(state, ...) {

    ui <- get_async_data_ui("handleFeedbackReflectionParadigm",
                              val_to_grab = '"ngrukkon"',
                              show_loader = TRUE,
                              show_next_button = FALSE)

  psychTestR::page(ui = ui,
                   label = label,
                   get_answer = function(input, ...) {

                     feedback <-  input$API_DATA_RESPONSE

                     feedback <- benevolent_score2(feedback)


                     psychTestR::set_global("latest_score", feedback, state)

                     list(
                       feedback = feedback
                     )
                   },
                   save_answer = TRUE,
                   final = FALSE)

  })
}

