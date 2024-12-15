


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

    psychTestR::code_block(function(state, ...) {
      # Init, if coming from a singing battery/trial
      # N.B, in the future, we'd remove this if it became a sight-singing test
      psychTestR::set_global("singing_trial", FALSE, state)
    }),

    set_test(test_name = "PBET", test_id = 2L),

    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p(psychTestR::i18n("pbet_learn_test_1"), " ", shiny::tags$strong(psychTestR::i18n("learning_phase")), " ", psychTestR::i18n("pbet_learn_test_2"), " ", shiny::tags$strong(psychTestR::i18n("test_phase")), "."),
                                                shiny::tags$p(psychTestR::i18n("pbet_learn_test_3"), " ", shiny::tags$em(psychTestR::i18n("learning_phase")), ", ", psychTestR::i18n("pbet_learn_test_4"), "."),
                                                shiny::tags$p(psychTestR::i18n("pbet_learn_test_5")))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p(psychTestR::i18n("pbet_learn_test_6")), shiny::tags$p(psychTestR::i18n("pbet_learn_test_7")))),
    psychTestR::one_button_page(shiny::tags$div(shiny::tags$p(psychTestR::i18n("pbet_learn_test_8")), shiny::tags$p(psychTestR::i18n("pbet_learn_test_9")))),
    psychTestR::one_button_page(shiny::tags$p(psychTestR::i18n("pbet_learn_test_10"), " ", shiny::tags$strong(psychTestR::i18n("learning_phase")), " ", psychTestR::i18n("pbet_learn_test_11"), ".")),
    psychTestR::one_button_page(psychTestR::i18n("pbet_learn_test_12")),
    learn_phase(examples, trial_paradigm, bpm, page_type),
    psychTestR::one_button_page(psychTestR::i18n("ready_for_real_thing")),
    learn_phase(user_sample, trial_paradigm, bpm, page_type),
    psychTestR::one_button_page(shiny::tags$p(psychTestR::i18n("pbet_learn_test_13"), " ", shiny::tags$strong(psychTestR::i18n("test_phase")), " ", psychTestR::i18n("pbet_learn_test_14"))),
    test_phase(user_sample, trial_paradigm, bpm, page_type),
    psychTestR::text_input_page("extra_comments_sight_vs_ear",
                                one_line = FALSE,
                                prompt = psychTestR::i18n("pbet_learn_test_share_thoughts"))
  )
}





learn_phase <- function(user_sample, trial_paradigm, bpm, page_type) {
  no_trials <- nrow(user_sample)
  1:no_trials %>%
    purrr::map(function(trial_no) {
      psychTestR::join(

        psychTestR::one_button_page(psychTestR::i18n("here_first_attempt")),
        learn_trial(user_sample, trial_no = trial_no, attempt = 1, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 1, page_type = page_type, bpm = bpm),

        psychTestR::one_button_page(psychTestR::i18n("here_second_attempt")),
        learn_trial(user_sample, trial_no = trial_no, attempt = 2, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 2, page_type = page_type, bpm = bpm),

        psychTestR::one_button_page(psychTestR::i18n("here_final_attempt")),
        learn_trial(user_sample, trial_no = trial_no, attempt = 3, trial_paradigm, bpm, page_type),
        learn_practice_trial(user_sample, trial_no = trial_no, attempt = 3, page_type = page_type, bpm = bpm),

        if(trial_no != no_trials) psychTestR::one_button_page(psychTestR::i18n("time_for_next_melody"))
      )
    })
}

test_phase <- function(user_sample, trial_paradigm, bpm, page_type) {

  # Randomise
  no_trials <- nrow(user_sample)

  1:no_trials %>%
    purrr::map(function(trial_no) {
      trial_dat <- user_sample[trial_no, ]
      page_label <- paste0("play_by_ear_test_trial_", trial_no, "_attempt_1")
      pbet_trial(trial_dat, trial_paradigm, attempt = 1, bpm, page_type, trial_no = trial_no, no_trials = no_trials, page_label = page_label)
    })
}

pbet_trial <- function(trial_dat, trial_paradigm, attempt, bpm, page_type, trial_no, no_trials, page_label = "play_by_ear") {

  logging::loginfo("trial_no: %s", trial_no)
  logging::loginfo("no_trials: %s", no_trials)

  crotchet_seconds <- 60/bpm
  abs_melody <- itembankr::str_mel_to_vector(trial_dat$abs_melody)
  durations <- itembankr::str_mel_to_vector(trial_dat$durations) * crotchet_seconds

  present_stimuli(
    stimuli = abs_melody,
    durations = durations,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = page_type,
    page_title = psychTestR::i18n("Play_the_melody_by_ear"),
    page_text = shiny::tags$div(
      shiny::tags$h3(paste0(psychTestR::i18n("Section_Progress"), ': ', trial_no, "/", no_trials)),
      shiny::tags$h4(psychTestR::i18n("Attempt"), " ", attempt, "/3"),
      set_melodic_stimuli(abs_melody, durations)
    ),
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_async_midi_vs_audio,
    give_first_melody_note = TRUE,
    attempt = attempt,
    page_label = page_label
  ) %>% init_trial_time_started(attempt, page_label = page_label)



}

init_trial_time_started <- function(page, attempt = 1L, additional = NULL, item_id = NULL, page_label = NULL) {

  stopifnot(psychTestR::is.test_element(page))

  psychTestR::reactive_page(function(state, ...) {

    psychTestR::set_global("trial_time_started", Sys.time(), state)
    psychTestR::set_global('melody_block_paradigm', "call_and_response", state)
    psychTestR::set_global('number_attempts', attempt, state)
    psychTestR::set_global('additional', additional, state)
    psychTestR::set_global('item_id', item_id, state)
    psychTestR::set_global('page_label', page_label, state)

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

  musicxml_file <- paste0("assets/", trial_dat$file_name)

  additional <- trial_dat %>%
    dplyr::select(flat_or_sharp, no_sharps_flats, no_bars, file_name) %>%
    dplyr::mutate(bpm = bpm)

  page_label <- paste0("play_by_ear_learn_trial_", trial_no, "_attempt_", attempt)

  item_id <- trial_dat$item_id

  if(trial_dat$display_modality == "visual") {

    page_label <- paste0("visual_", page_label)

    trial <- present_stimuli(
      stimuli = musicxml_file,
      stimuli_type = "musicxml_file",
      display_modality = "visual",
      page_type = page_type,
      sheet_music_start_hidden = TRUE,
      show_sheet_music_after_record = TRUE,
      page_title = psychTestR::i18n("Play_the_melody_from_sight"),
      page_text = shiny::tags$div(
        shiny::tags$h3(paste0(psychTestR::i18n("Section_Progress"), ': ', trial_no, "/", no_trials)),
        shiny::tags$h4(psychTestR::i18n("Attempt"), " ", attempt, "/3"),
        set_melodic_stimuli(abs_melody, durations, musicxml_file),
        shiny::tags$p(psychTestR::i18n("use_metronome_correct_bpm"), " "),
        js_metronome(bpm)
      ),
      page_label = page_label,
      get_answer = get_answer_async_midi_vs_audio,
      show_record_button = TRUE,
      attempt = attempt
    ) %>% init_trial_time_started(attempt, additional, item_id, page_label = page_label)
  } else {
    page_label <- paste0("auditory_", page_label)
    trial <- pbet_trial(trial_dat, trial_paradigm, attempt, bpm, page_type, trial_no = trial_no, no_trials = no_trials, page_label)
  }
}

learn_practice_trial <- function(user_sample, trial_no, attempt, page_type, bpm) {

  trial_dat <- user_sample[trial_no, ]

  trial_paradigm <- paradigm()

  crotchet_seconds <- 60/bpm

  page_label <- paste0("play_by_ear_learn_practice_from_memory_trial_", trial_no, "_attempt_", attempt)

  trial <- present_stimuli(
    stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
    durations = itembankr::str_mel_to_vector(trial_dat$durations) * crotchet_seconds,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = page_type,
    page_label = page_label,
    page_title = psychTestR::i18n("Play_from_memory"),
    page_text = shiny::tags$div(
      shiny::tags$p(psychTestR::i18n("play_from_memory_message")),
      shiny::tags$script("document.getElementById('playButton').style.display = 'none';")
    ),
    show_record_button = TRUE,
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_async_midi_vs_audio,
    attempt = attempt
  ) %>% init_trial_time_started(attempt, page_label = page_label)

  return(trial)
}



#     additional <- extract_no_sharps_or_flats_from_musicxml_file(musicxml_file)



