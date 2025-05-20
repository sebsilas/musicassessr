
#' Production reflection perception paradigm test
#'
#' @param num_items_arrhythmic
#' @param num_items_rhythmic
#' @param max_attempts
#'
#' @returns
#' @export
#'
#' @examples
production_reflection_perception_paradigm_standalone <- function(num_items_arrhythmic = 4L,
                                                                 num_items_rhythmic = 4L,
                                                                 max_attempts = 3L) {

  pbet_hmtm_2024_item_bank <- pbet_hmtm_2024_item_bank %>% tibble::as_tibble()

  smp_arrhythmic <- sample_in_ntiles_wrapper(pbet_hmtm_2024_item_bank,
                                            num_items = num_items_arrhythmic,
                                            n = 4, # i.e., quartiles
                                            col_name = "arrhythmic_difficulty_percentile") %>%
    dplyr::mutate(rhythmic = FALSE)

  smp_rhythmic <- sample_in_ntiles_wrapper(pbet_hmtm_2024_item_bank %>%
                                               dplyr::filter(!item_id %in% !! smp_arrhythmic$item_id),
                                           num_items = num_items_rhythmic,
                                           n = 4, # i.e., quartiles
                                           col_name = "rhythmic_difficulty_percentile") %>%
    dplyr::mutate(rhythmic = TRUE)


  smp <- rbind(smp_arrhythmic, smp_rhythmic) %>%
    dplyr::slice_sample(n = nrow(.)) %>%  # Randomise order
    dplyr::mutate(trial_no = dplyr::row_number() )


  production_reflection_perception_paradigm_block_audio <- create_production_reflection_perception_paradigm_block(smp, page_type = "record_audio_page")
  production_reflection_perception_paradigm_block_midi <- create_production_reflection_perception_paradigm_block(smp, page_type = "record_midi_page")

  production_reflection_perception_paradigm_block <- midi_or_audio_reactive(production_reflection_perception_paradigm_block_midi, production_reflection_perception_paradigm_block_audio)

  make_musicassessr_test(
    welcome_page = psychTestR::one_button_page(
      "Trialing the new production-reflection-perception paradigm.",
      button_text = psychTestR::i18n("Next")
    ),
    elts = function() {

      production_reflection_perception_paradigm_block

    },
    opt = musicassessr_opt(
      visual_notation = TRUE,
      get_p_id = TRUE,
      async_success_msg = psychTestR::i18n("lets_proceed"),
      use_presigned_url = FALSE,
      asynchronous_api_mode = TRUE,
      user_id = 1L,
      username = "dev",
      midi_input = TRUE,
      setup_options = setup_pages_options(
        input_type = "midi_keyboard_or_microphone",
        SNR_test = FALSE,
        get_instrument_range = FALSE,
      ),
      app_name = "production_reflection_perception_paradigm"
    ),
    title = "Testing Production-Reflection-Perception Paradigm",
    admin_password = Sys.getenv("USER_PW"),
    dict = musicassessr_dict
  )
}



create_production_reflection_perception_paradigm_block <- function(trials,
                                                                   page_type = "record_audio_page",
                                                                   max_attempts = 4L) {

  purrr::pmap(trials, function(...) {

    trial_dat <- list(...)

    purrr::map(1:max_attempts, function(attempt_no) {
      production_reflection_perception_paradigm_protocol(stimuli = itembankr::str_mel_to_vector( trial_dat$abs_melody ),
                                                         stimuli_durations = itembankr::str_mel_to_vector( trial_dat$durations ),
                                                         attempt = attempt_no,
                                                         item_id = trial_dat$item_id,
                                                         rhythmic = trial_dat$rhythmic,
                                                         page_type = page_type,
                                                         max_attempts = max_attempts,
                                                         trial_no = trial_dat$trial_no,
                                                         no_trials = nrow(trials))
    })
  }) %>% unlist()

}

production_reflection_perception_paradigm_protocol <- function(stimuli = c(60, 61, 62, 63),
                                                               stimuli_durations = rep(0.5, 4),
                                                               attempt = 1L,
                                                               item_id = "item_id",
                                                               rhythmic = TRUE,
                                                               page_type = "record_audio_page",
                                                               max_attempts = 3L,
                                                               trial_no = 1L,
                                                               no_trials = 1L) {

  trial_paradigm <- paradigm(page_type = page_type)


  protocol <- psychTestR::module("production_reflection_perception_paradigm_protocol",
    psychTestR::join(

      # Reset score
      psychTestR::code_block(function(state, ...) {
        psychTestR::set_global("latest_score", NULL, state)
      }),

      # Production

      psychTestR::reactive_page(function(state, ...) {

        psychTestR::set_global("trial_time_started", Sys.time(), state)

        present_stimuli(
          page_label = paste0("production_reflection_perception.production_", trial_no, "_attempt_", attempt),
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
          db_vars = create_db_vars_template(attempt = attempt,
                                            item_id = item_id,
                                            rhythmic = rhythmic,
                                            feedback = TRUE,
                                            feedback_type = "opti3",
                                            page_label = paste0("production.attempt_", attempt),
                                            instrument = psychTestR::get_global("inst", state),
                                            session_id = get_promise_value( psychTestR::get_global("session_id", state) ),
                                            user_id = psychTestR::get_global("user_id", state),
                                            melody_block_paradigm = "production_reflection_perception"),
          page_type = page_type)

      }),

      # Reflection

      rate_similarity(label = paste0("reflection_", trial_no, "_attempt_", attempt),
                      prompt = shiny::tags$p("How would you rate your performance on the last trial?"),
                      labels = c("Not good", "Quite good", "Very good", "Perfect") ),

      # Perception
      midi_or_audio_reactive(perception_page_midi(state, attempt, label = paste0("perception_", trial_no, "_attempt_", attempt)),
                             perception_page_audio(state, attempt, label = paste0("perception_", trial_no, "_attempt_", attempt), stimuli, stimuli_durations))



    )
  )

  if(attempt > 1L) {
    protocol <- psychTestR::conditional(function(state, ...) {

      latest_score <- psychTestR::get_global("latest_score", state)

      logging::loginfo("latest_score: %s", latest_score)

      if(length(latest_score) == 0) {
        return(TRUE)
      }

      if(dplyr::near(latest_score, 1)) {
        return(FALSE)
      } else {
        return(TRUE)
      }

    }, protocol)
  }

  return(protocol)

}


perception_page_midi <- function(state, attempt, label) {
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

perception_page_audio <- function(state, attempt, label, stimuli, stimuli_durations) {
  psychTestR::reactive_page(function(state, ...) {

    wav_file <- psychTestR::get_global("wav_file", state)

    base_url <- if ( Sys.getenv("R_CONFIG_ACTIVE") %in% c("default", "prod") ) {
      "https://musicassessr-media-source.s3.eu-central-1.amazonaws.com/"
    } else {
      "https://shinny-app-source-41630.s3.us-east-1.amazonaws.com/"
    }

    audio_file <- paste0(base_url, wav_file, ".wav")

    logging::loginfo("audio_file: %s", audio_file)


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

                                      # Instructional message
                                      shiny::tags$p(id = "audio_message", "And now here is what you played:"),

                                      # Audio element - hidden initially
                                      shiny::tags$audio(id = "audio_feedback",
                                                        controls = TRUE,
                                                        style = "display: none;",
                                                        shiny::tags$source(src = audio_file, type = "audio/wav")),

                                      # Manual retry/check button
                                      shiny::tags$button(id = "check_audio_button", "Check if Audio is Ready", class = "btn"),

                                      # JS logic for manual check and end-of-playback trigger
                                      shiny::tags$script(shiny::HTML("
                                          document.getElementById('check_audio_button').addEventListener('click', function() {
                                            const audio = document.getElementById('audio_feedback');
                                            const source = audio.querySelector('source');

                                            audio.load();  // force reload

                                            audio.oncanplaythrough = function() {
                                              audio.style.display = 'block';
                                              document.getElementById('check_audio_button').style.display = 'none';
                                            };

                                            audio.onerror = function() {
                                              alert('Audio file not ready yet. Please try again in a moment.');
                                            };

                                            audio.onended = function() {
                                              console.log('Audio finished playing.');
                                              document.getElementById('next').style.display = 'block';
                                              document.querySelectorAll('.slider-container').forEach(el => {
                                                el.style.display = 'block';
                                              });
                                              audio.style.display = 'none';
                                              document.getElementById('audio_message').style.display = 'none';
                                            };
                                          });
                                        "))
                                                            ),


                      get_async_data_ui("handleFeedbackReflectionParadigm",
                                        val_to_grab = '"opti3"',
                                        show_loader = FALSE,
                                        show_next_button = FALSE), # We do this at the slider page level for layour reasons
                    ),
                    trigger_button_style = "display: none",
                    get_answer = function(input, ...) {

                      feedback <-  input$API_DATA_RESPONSE

                      psychTestR::set_global("latest_score", feedback, state)

                      list(
                        feedback = feedback,
                        slider_rating = input$slider
                      )
                    },
                    slider_start_hidden = TRUE,
                    after_table_ui = shiny::tags$p("How would you rate your performance having listened back?")
    )

  })
}

rate_similarity <- function(label = 'similarity_rating',
                            prompt = shiny::tags$p("How similar did the two melodies sound?"),
                            get_answer = function(input, ...) input$slider,
                            trigger_button_style = "",
                            labels = c("Not at all similar",
                                       "Somewhat similar",
                                       "Very similar",
                                       "The same"),
                            slider_start_hidden = FALSE,
                            after_table_ui = NULL) {

  slider_page_extended(
    value = 5,
    min = 0,
    max = 10,
    label = label,
    prompt = shiny::tags$div(
      # This part is always visible
      shiny::tags$div(class = "prompt-always-visible", prompt),

      # This part is hidden/shown dynamically
      shiny::tags$div(class = "slider-container",
                      style = if (slider_start_hidden) "display: none;" else "",
                      similar_table(labels = labels),
                      after_table_ui
      )
    ),
    width = "50%",
    get_answer = get_answer,
    trigger_button_style = trigger_button_style,
    slider_start_hidden = slider_start_hidden
  )
}



similar_table <- function(labels = c("Not at all similar",
                                     "Somewhat similar",
                                     "Very similar",
                                     "The same")) {
  htmltools::HTML(
    '
    <style>
  td {
    text-align: center;
    vertical-align: middle;
  }
</style>

<table style="width: 65%; margin: auto; font-weight: bold; text-align: center;">
  <thead>
    <tr>',

    paste0(paste0("<td>", labels, "</td>"), collapse = ""),

    '</tr>
  </thead>
</table>

    '
  )
}



slider_page_extended <- function(label,
                                 prompt,
                        min, max, value,
                        save_answer = TRUE,
                        button_text = "Next",
                        on_complete = NULL,
                        admin_ui = NULL,
                        step = NULL, round = FALSE,
                        ticks = TRUE, animate = FALSE,
                        width = NULL, sep = ",",
                        pre = NULL, post = NULL,
                        timeFormat = NULL,
                        timezone = NULL,
                        dragRange = TRUE,
                        get_answer = function(input, ...) input$slider,
                        trigger_button_style = "",
                        slider_start_hidden = FALSE) {

  stopifnot(is.scalar.character(label))

  slider <- shiny::sliderInput(
        "slider",
        label = NULL,
        min = min,
        max = max,
        value = value,
        step = step,
        round = round,
        ticks = ticks,
        animate = animate,
        width = width,
        sep = sep,
        pre = pre,
        post = post,
        timeFormat = timeFormat,
        timezone = timezone,
        dragRange = dragRange
      )

  body = shiny::div(
    tagify(prompt),
    shiny::div(
      class = "slider-container",
      style = if (slider_start_hidden) "display: none;" else "",
      slider
    )
  )

  ui <- shiny::div(body,
                   psychTestR::trigger_button("next", button_text, style = trigger_button_style)
                   )

  psychTestR::page(ui = ui, label = label, get_answer = get_answer, save_answer = save_answer,
       on_complete = on_complete, final = FALSE,
       admin_ui = admin_ui)
}
