


pbet_trial <- function(trial_dat, trial_paradigm, attempt) {
  present_stimuli(
    stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
    durations = itembankr::str_mel_to_vector(trial_dat$durations) / 2,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = "record_audio_page",
    page_title = "Play the melody by ear",
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_add_trial_and_compute_trial_scores_s3,
    give_first_melody_note = TRUE,
    attempt = attempt
  )
}
# Learn block


learn_trial <- function(user_sample, trial_no, attempt, trial_paradigm) {

  trial_dat <- user_sample[trial_no, ]

  if(trial_dat$display_modality == "visual") {
    trial <- present_stimuli(
      stimuli = paste0("assets/", trial_dat$file_name),
      stimuli_type = "musicxml_file",
      display_modality = "visual",
      page_type = "record_audio_page",
      sheet_music_start_hidden = TRUE,
      show_sheet_music_after_record = TRUE,
      page_title = "Play the melody from sight",
      page_text = shiny::tags$div(
        shiny::tags$p("Use metronome to get the correct BPM: "),
        js_metronome()
      ),
      get_answer = get_answer_add_trial_and_compute_trial_scores_s3,
      show_record_button = TRUE,
      attempt = attempt
    )
  } else {
    trial <- pbet_trial(trial_dat, trial_paradigm, attempt)
  }
}

learn_practice_trial <- function(user_sample, trial_no, attempt) {

  trial_dat <- user_sample[trial_no, ]

  trial_paradigm <- paradigm()

  trial <- present_stimuli(
    stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
    durations = itembankr::str_mel_to_vector(trial_dat$durations) / 2,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = "record_audio_page",
    page_title = "Play from memory",
    page_text = shiny::tags$div(
      shiny::tags$p("Now try and play the melody you just encountered from memory."),
      shiny::tags$script("document.getElementById('playButton').style.display = 'none';")
    ),
    show_record_button = TRUE,
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_add_trial_and_compute_trial_scores_s3,
    attempt = attempt
  )
  return(trial)
}


learn_phase <- function(user_sample, trial_paradigm) {
  no_trials <- nrow(user_sample)
  1:no_trials %>%
    purrr::map(function(trial_no) {
      psychTestR::join(

        psychTestR::one_button_page("Here is your first attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 1, trial_paradigm),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 1),

        psychTestR::one_button_page("Now is your second attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 2, trial_paradigm),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 2),

        psychTestR::one_button_page("Now is your final attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 3, trial_paradigm),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 3),

        if(trial_no != no_trials) psychTestR::one_button_page("Time for the next melody!")
      )
    })
}

test_phase <- function(user_sample, trial_paradigm) {

  # Randomise
  no_trials <- nrow(user_sample)

  1:no_trials %>%
    purrr::map(function(trial_no) {
      trial_dat <- user_sample[trial_no, ]
      pbet_trial(trial_dat, trial_paradigm, attempt = 1)
    })
}

get_tl <- function() {

  trial_paradigm <- paradigm()

  # Get sample

  user_sample <- Berkowitz_selected_musicxml_item_bank %>%
    dplyr::filter(dplyr::between(N, 6, 24)) %>%
    dplyr::group_by(SAA_rhythmic_difficulty_quartile) %>%
    dplyr::slice_sample(n = 1) %>%
    # Randomise order
    dplyr::slice_sample(n = 4) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(display_modality = sample(c("visual", "auditory", "visual", "auditory")) )


  psychTestR::join(
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("In the next section, there will be two phases: a ", shiny::tags$strong("learning"), " phase and a ", shiny::tags$strong("test"), " phase."),
                                                shiny::tags$p("In the first ", shiny::tags$em("learning"), "phase, you will be asked to learn four melodies, two by ear and two from sheet music."),
                                                shiny::tags$p("The order will be random."))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("You will have three attempts to learn each melody."), shiny::tags$p("After each attempt, you will be asked to try and play the melody from memory."))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("Afterwards, in the test phase, we will ask you to play all melodies you learned again from memory, but you will hear them all by ear."), shiny::tags$p("Try your best to recognise and play the melody from ear. "))),
    psychTestR::one_button_page(shiny::tags$p("Now we will begin the ", shiny::tags$strong("learning"), " phase, where you will learn the melodies either by sight or by ear.")),
    learn_phase(user_sample, trial_paradigm),
    psychTestR::one_button_page(shiny::tags$p("Now we will begin the", shiny::tags$strong("test"), " phase, where you will be asked to play all the melodies you heard by ear.")),
    test_phase(user_sample, trial_paradigm),
    psychTestR::text_input_page("extra_comments_sight_vs_ear",
                                one_line = FALSE,
                                prompt = "Please share some thoughts about your experience of learning to play melodies by ear or by sight, within this test, and/or in your broader musical life. You can leave this blank if you have nothing you would like to share.")
  )
}


#' PBET learn test block
#'
#' @return
#' @export
#'
#' @examples
pbet_learn_test <- function() {
  make_musicassessr_test(
    welcome_page = psychTestR::one_button_page("Trialing the new learn-test audio-visual paradigm."),
    elts = function() {
      get_tl()
    },
    opt = musicassessr_opt(setup_pages = TRUE,
                           visual_notation = TRUE,
                           asynchronous_api_mode = TRUE,
                           user_id = 1L,
                           username = "dev",
                           setup_options = setup_pages_options(skip_setup = "except_microphone"),
                           app_name = "learn_test_music_xml"),
    title = "Learn Test Visual vs. Audio",
    admin_password = Sys.getenv("USER_PW"),
    dict = musicassessr_dict
  )
}
