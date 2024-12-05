


#' PBET learn test block
#'
#' @return
#' @export
#'
#' @examples
pbet_learn_test_paradigm_standalone <- function(no_trials) {

  make_musicassessr_test(
    welcome_page = psychTestR::one_button_page("Trialing the new learn-test audio-visual paradigm."),
    elts = function() {
      pbet_learn_test_paradigm(no_trials)
    },
    opt = musicassessr_opt(visual_notation = TRUE,
                           asynchronous_api_mode = TRUE,
                           user_id = 1L,
                           username = "dev",
                           midi_input = TRUE,
                           setup_options = setup_pages_options(input_type = "midi_keyboard_or_microphone",
                                                               SNR_test = FALSE,
                                                               get_instrument_range = FALSE,),
                           app_name = "learn_test_music_xml"),
    title = "Learn Test Visual vs. Audio",
    admin_password = Sys.getenv("USER_PW"),
    dict = musicassessr_dict
  )
}

pbet_learn_test_paradigm <- function(no_trials = 4L,
                                     bpm = 100) {

  shiny::addResourcePath('assets', "/Users/sebsilas/Berkowitz_measures_divided")

  no_visual <- no_trials/2
  no_auditory <- no_trials/2

  trial_paradigm <- paradigm()

  # Get sample
  item_bank_filtered <- Berkowitz_selected_musicxml_item_bank %>%
    dplyr::filter(dplyr::between(N, 6, 24))

  user_sample <- item_bank_filtered %>%
    dplyr::group_by(SAA_rhythmic_difficulty_quartile) %>%
    dplyr::slice_sample(n = no_trials/4) %>%
    # Randomise order
    dplyr::slice_sample(n = no_trials) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(display_modality = sample(c(rep("visual", no_visual),
                                              rep("auditory", no_auditory))) )

  examples <- item_bank_filtered %>%
                dplyr::filter(!file_name %in% user_sample$file_name,
                              SAA_rhythmic_difficulty_quartile == 1) %>%
                dplyr::slice_sample(n = 2) %>%
                dplyr::mutate(display_modality = c("visual", "auditory"))


  midi_logic <- pbet_learn_test_trial_logic(user_sample, trial_paradigm, bpm, page_type = "record_midi_page", examples = examples) %>%
    unlist()

  audio_logic <- pbet_learn_test_trial_logic(user_sample, trial_paradigm, bpm, page_type = "record_audio_page", examples = examples) %>%
    unlist()

  midi_or_audio_reactive(
    midi_logic = midi_logic,
    audio_logic = audio_logic
  )

}

pbet_learn_test_trial_logic <- function(user_sample, trial_paradigm, bpm, page_type, examples) {

  psychTestR::join(

    set_test(test_name = "PBET",
             test_id = 2L),

    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("In the next section, there will be two phases: a ", shiny::tags$strong("learning"), " phase and a ", shiny::tags$strong("test"), " phase."),
                                                shiny::tags$p("In the first ", shiny::tags$em("learning"), "phase, you will be asked to learn four melodies, two by ear and two from sheet music."),
                                                shiny::tags$p("The order will be random."))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("You will have three attempts to learn each melody."), shiny::tags$p("After each attempt, you will be asked to try and play the melody from memory."))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("Afterwards, in the test phase, we will ask you to play all melodies you learned again from memory, but you will hear them all by ear."), shiny::tags$p("Try your best to recognise and play the melody from ear. "))),
    psychTestR::one_button_page(shiny::tags$p("Now we will begin the ", shiny::tags$strong("learning"), " phase, where you will learn the melodies either by sight or by ear.")),
    psychTestR::one_button_page("First, you will try two example practice trials: one by sight, the other by ear"),
    learn_phase(examples, trial_paradigm, bpm, page_type),
    psychTestR::one_button_page("Now you're ready for the real thing!"),
    learn_phase(user_sample, trial_paradigm, bpm, page_type),
    psychTestR::one_button_page(shiny::tags$p("Now we will begin the", shiny::tags$strong("test"), " phase, where you will be asked to play all the melodies you heard by ear.")),
    test_phase(user_sample, trial_paradigm, bpm, page_type),
    psychTestR::text_input_page("extra_comments_sight_vs_ear",
                                one_line = FALSE,
                                prompt = "Please share some thoughts about your experience of learning to play melodies by ear or by sight, within this test, and/or in your broader musical life. You can leave this blank if you have nothing you would like to share.")
  )
}





learn_phase <- function(user_sample, trial_paradigm, bpm, page_type) {
  no_trials <- nrow(user_sample)
  1:no_trials %>%
    purrr::map(function(trial_no) {
      psychTestR::join(

        psychTestR::one_button_page("Here is your first attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 1, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 1, page_type = page_type, bpm = bpm),

        psychTestR::one_button_page("Now is your second attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 2, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 2, page_type = page_type, bpm = bpm),

        psychTestR::one_button_page("Now is your final attempt."),
        learn_trial(user_sample, trial_no = trial_no, attempt = 3, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 3, page_type = page_type, bpm = bpm),

        if(trial_no != no_trials) psychTestR::one_button_page("Time for the next melody!")
      )
    })
}

test_phase <- function(user_sample, trial_paradigm, bpm, page_type) {

  # Randomise
  no_trials <- nrow(user_sample)

  1:no_trials %>%
    purrr::map(function(trial_no) {
      trial_dat <- user_sample[trial_no, ]
      pbet_trial(trial_dat, trial_paradigm, attempt = 1, bpm, page_type)
    })
}

pbet_trial <- function(trial_dat, trial_paradigm, attempt, bpm, page_type) {

  crotchet_seconds <- 60/bpm

  present_stimuli(
    stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
    durations = itembankr::str_mel_to_vector(trial_dat$durations) * crotchet_seconds,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = page_type,
    page_title = "Play the melody by ear",
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_async_midi_vs_audio,
    give_first_melody_note = TRUE,
    attempt = attempt
  ) %>% init_trial_time_started(attempt)



}

init_trial_time_started <- function(page, attempt = 1L) {

  stopifnot(psychTestR::is.test_element(page))

  psychTestR::reactive_page(function(state, ...) {

    psychTestR::set_global("trial_time_started", Sys.time(), state)
    psychTestR::set_global('melody_block_paradigm', "call_and_response", state)
    psychTestR::set_global('number_attempts', attempt, state)

    page

  })
}

learn_trial <- function(user_sample, trial_no, attempt, trial_paradigm, bpm, page_type) {

  stopifnot(
    is.numeric(bpm)
  )

  crotchet_seconds <- 60/bpm
  trial_dat <- user_sample[trial_no, ]
  no_trials <- nrow(user_sample)

  # For DB storage
  abs_melody <- itembankr::str_mel_to_vector(trial_dat$abs_melody)
  durations <- itembankr::str_mel_to_vector(trial_dat$durations) * crotchet_seconds

  if(trial_dat$display_modality == "visual") {
    trial <- present_stimuli(
      stimuli = paste0("assets/", trial_dat$file_name),
      stimuli_type = "musicxml_file",
      display_modality = "visual",
      page_type = page_type,
      sheet_music_start_hidden = TRUE,
      show_sheet_music_after_record = TRUE,
      page_title = "Play the melody from sight",
      page_text = shiny::tags$div(
        shiny::tags$h3(paste0(psychTestR::i18n("Section_Progress"), ': ', trial_no, "/", no_trials)),
        shiny::tags$h4(psychTestR::i18n("Attempt"), " ", attempt, "/3"),
        set_melodic_stimuli(abs_melody, durations),
        shiny::tags$p("Use metronome to get the correct BPM: "),
        js_metronome(bpm)
      ),
      get_answer = get_answer_async_midi_vs_audio,
      show_record_button = TRUE,
      attempt = attempt
    ) %>% init_trial_time_started(attempt)
  } else {
    trial <- pbet_trial(trial_dat, trial_paradigm, attempt, bpm, page_type)
  }
}

learn_practice_trial <- function(user_sample, trial_no, attempt, page_type, bpm) {

  trial_dat <- user_sample[trial_no, ]

  trial_paradigm <- paradigm()

  crotchet_seconds <- 60/bpm

  trial <- present_stimuli(
    stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
    durations = itembankr::str_mel_to_vector(trial_dat$durations) * crotchet_seconds,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = page_type,
    page_title = "Play from memory",
    page_text = shiny::tags$div(
      shiny::tags$p("Now try and play the melody you just encountered from memory."),
      shiny::tags$script("document.getElementById('playButton').style.display = 'none';")
    ),
    show_record_button = TRUE,
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_async_midi_vs_audio,
    attempt = attempt
  ) %>% init_trial_time_started(attempt)

  return(trial)
}






