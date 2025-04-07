




# pbet_learn_test_paradigm_standalone(3)

#' PBET learn test block
#' @param no_trials
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
pbet_learn_test_paradigm_standalone <- function(no_trials, asynchronous_api_mode = TRUE) {
  make_musicassessr_test(
    welcome_page = psychTestR::one_button_page(
      "Trialing the new learn-test audio-visual paradigm.",
      button_text = psychTestR::i18n("Next")
    ),
    elts = function() {
      pbet_learn_test_paradigm(no_trials, asynchronous_api_mode = asynchronous_api_mode)
    },
    opt = musicassessr_opt(
      visual_notation = TRUE,
      asynchronous_api_mode = asynchronous_api_mode,
      user_id = 1L,
      username = "dev",
      midi_input = TRUE,
      setup_options = setup_pages_options(
        input_type = "midi_keyboard_or_microphone",
        SNR_test = FALSE,
        get_instrument_range = FALSE,
      ),
      app_name = "learn_test_music_xml"
    ),
    title = "Learn Test Visual vs. Audio",
    admin_password = Sys.getenv("USER_PW"),
    dict = musicassessr_dict
  )
}

#' PBET learn test paradigm
#'
#' @param no_trials
#' @param bpm
#' @param mute_midi_playback
#' @param asynchronous_api_mode
#' @param with_examples
#'
#' @returns
#' @export
#'
#' @examples
pbet_learn_test_paradigm <- function(no_trials = 4L,
                                     bpm = 100,
                                     mute_midi_playback = TRUE,
                                     asynchronous_api_mode = TRUE,
                                     with_examples = TRUE) {
  shiny::addResourcePath(
    'assets',
    system.file(
      "extdata/Berkowitz_measures_divided",
      package = "musicassessr",
      mustWork = TRUE
    )
  )


  midi_logic <- pbet_learn_test_trial_logic(
    no_trials,
    bpm,
    page_type = "record_midi_page",
    mute_midi_playback = mute_midi_playback,
    asynchronous_api_mode = asynchronous_api_mode,
    with_examples = with_examples
  ) %>%
    unlist()

  audio_logic <- pbet_learn_test_trial_logic(
    no_trials,
    bpm,
    page_type = "record_audio_page",
    mute_midi_playback = mute_midi_playback,
    asynchronous_api_mode = asynchronous_api_mode,
    with_examples = with_examples
  ) %>%
    unlist()

  psychTestR::module("pbet_learn_test_paradigm",
                     psychTestR::join(
                       midi_or_audio_reactive(midi_logic = midi_logic, audio_logic = audio_logic)
                     ))

}

pbet_learn_test_trial_logic <- function(no_trials,
                                        bpm,
                                        page_type,
                                        mute_midi_playback = TRUE,
                                        asynchronous_api_mode = TRUE,
                                        with_examples = TRUE) {

  trial_paradigm <- paradigm(page_type = page_type)


  psychTestR::join(
    learn_test_pbet_sampler(no_trials),

    psychTestR::code_block(function(state, ...) {
      # Init, if coming from a singing battery/trial
      # N.B, in the future, we'd remove this if it became a sight-singing test
      psychTestR::set_global("singing_trial", FALSE, state)
    }),

    set_test(test_name = "PBET", test_id = 2L),

    psychTestR::reactive_page(function(state, ...) {
      ui <- shiny::tags$div(
        if (psychTestR::get_global("asynchronous_api_mode", state))
          turn_on_upload_to_s3_mode(log = TRUE)
        else
          NULL,
        shiny::tags$p(
          psychTestR::i18n("pbet_learn_test_1"),
          " ",
          shiny::tags$strong(psychTestR::i18n("learning_phase")),
          " ",
          psychTestR::i18n("pbet_learn_test_2"),
          " ",
          shiny::tags$strong(psychTestR::i18n("test_phase")),
          "."
        ),
        shiny::tags$p(
          psychTestR::i18n("pbet_learn_test_3"),
          " ",
          shiny::tags$em(psychTestR::i18n("learning_phase")),
          ", ",
          psychTestR::i18n("pbet_learn_test_4"),
          "."
        ),
        shiny::tags$p(psychTestR::i18n("pbet_learn_test_5"))
      )


      psychTestR::one_button_page(ui, button_text = psychTestR::i18n("Next"))

    }),


    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$p(psychTestR::i18n("pbet_learn_test_6")),
        shiny::tags$p(psychTestR::i18n("pbet_learn_test_7"))
      ),
      button_text = psychTestR::i18n("Next")
    ),
    psychTestR::one_button_page(
      shiny::tags$div(
        shiny::tags$p(psychTestR::i18n("pbet_learn_test_8")),
        shiny::tags$p(psychTestR::i18n("pbet_learn_test_9"))
      ),
      button_text = psychTestR::i18n("Next")
    ),
    psychTestR::one_button_page(
      shiny::tags$p(
        psychTestR::i18n("pbet_learn_test_10"),
        " ",
        shiny::tags$strong(psychTestR::i18n("learning_phase")),
        " ",
        psychTestR::i18n("pbet_learn_test_11"),
        "."
      ),
      button_text = psychTestR::i18n("Next")
    ),
    psychTestR::one_button_page(
      psychTestR::i18n("pbet_learn_test_12"),
      button_text = psychTestR::i18n("Next")
    ),
    # Examples
    if(with_examples) learn_phase(
      2L,
      # no examples fixed
      trial_paradigm,
      bpm,
      page_type,
      mute_midi_playback,
      asynchronous_api_mode,
      phase = "example"
    ),
    psychTestR::one_button_page(
      psychTestR::i18n("ready_for_real_thing"),
      button_text = psychTestR::i18n("Next")
    ),
    # Real items
    learn_phase(
      no_trials,
      trial_paradigm,
      bpm,
      page_type,
      mute_midi_playback,
      asynchronous_api_mode
    ),
    psychTestR::one_button_page(
      shiny::tags$p(
        psychTestR::i18n("pbet_learn_test_13"),
        " ",
        shiny::tags$strong(psychTestR::i18n("test_phase")),
        " ",
        psychTestR::i18n("pbet_learn_test_14")
      ),
      button_text = psychTestR::i18n("Next")
    ),
    test_phase(
      no_trials,
      trial_paradigm,
      bpm,
      page_type,
      mute_midi_playback,
      asynchronous_api_mode
    ),
    # Clear vars
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("additional", NA, state)
    }),
    psychTestR::text_input_page(
      "extra_comments_sight_vs_ear",
      one_line = FALSE,
      prompt = psychTestR::i18n("pbet_learn_test_share_thoughts"),
      button_text = psychTestR::i18n("Next")
    )
  )
}


learn_test_pbet_sampler <- function(no_trials) {
  psychTestR::code_block(function(state, ...) {
    clef <- psychTestR::get_global("clef", state)

    lowest_reading_note <- psychTestR::get_global("lowest_reading_note", state)
    highest_reading_note <- psychTestR::get_global("highest_reading_note", state)

    # Get sample
    item_bank_filtered <- Berkowitz_selected_musicxml_item_bank %>%
      dplyr::filter(clef == !!clef) %>%
      dplyr::filter(lowest_note > lowest_reading_note,
                    highest_note < highest_reading_note) %>%
      dplyr::filter(dplyr::between(N, 6, 24))

    user_sample <- item_bank_filtered %>%
      dplyr::group_by(SAA_rhythmic_difficulty_quartile) %>%
      dplyr::slice_sample(n = ceiling(no_trials / 4)) %>% # Handle case of under 4 trials with ceiling
      # Randomise order (and also handle case for < 4 trials i.e., remove any extras as a result of the quartile sampling)
      dplyr::slice_sample(n = no_trials) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(display_modality = sample(c(
        rep("visual", nrow(.) / 2), rep("auditory", nrow(.) / 2)
      ))) %>%
      # Randomise order again
      dplyr::slice_sample(n = no_trials)

    examples <- item_bank_filtered %>%
      dplyr::filter(!file_name %in% user_sample$file_name,
                    SAA_rhythmic_difficulty_quartile == 1) %>%
      dplyr::slice_sample(n = 2) %>%
      dplyr::mutate(display_modality = c("visual", "auditory"))

    psychTestR::set_global("examples_sample", examples, state)
    psychTestR::set_global("trials_sample", user_sample, state)

  })

}




learn_phase <- function(no_trials,
                        trial_paradigm,
                        bpm,
                        page_type,
                        mute_midi_playback = TRUE,
                        asynchronous_api_mode = TRUE,
                        phase = "learn") {
  1:no_trials %>%
    purrr::map(function(trial_no) {
      psychTestR::join(
        psychTestR::one_button_page(
          psychTestR::i18n("here_first_attempt"),
          button_text = psychTestR::i18n("Next")
        ),
        learn_trial(
          trial_no = trial_no,
          attempt = 1,
          trial_paradigm,
          bpm,
          page_type,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),
        learn_practice_trial(
          trial_no = trial_no,
          attempt = 1,
          page_type = page_type,
          bpm = bpm,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),

        psychTestR::one_button_page(
          psychTestR::i18n("here_second_attempt"),
          button_text = psychTestR::i18n("Next")
        ),
        learn_trial(
          trial_no = trial_no,
          attempt = 2,
          trial_paradigm,
          bpm,
          page_type,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),
        learn_practice_trial(
          trial_no = trial_no,
          attempt = 2,
          page_type = page_type,
          bpm = bpm,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),

        psychTestR::one_button_page(
          psychTestR::i18n("here_final_attempt"),
          button_text = psychTestR::i18n("Next")
        ),
        learn_trial(
          trial_no = trial_no,
          attempt = 3,
          trial_paradigm,
          bpm,
          page_type,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),
        learn_practice_trial(
          trial_no = trial_no,
          attempt = 3,
          page_type = page_type,
          bpm = bpm,
          phase = phase,
          mute_midi_playback = mute_midi_playback,
          asynchronous_api_mode = asynchronous_api_mode
        ),

        if (trial_no != no_trials)
          psychTestR::one_button_page(
            psychTestR::i18n("time_for_next_melody"),
            button_text = psychTestR::i18n("Next")
          )
      )
    })
}

test_phase <- function(no_trials,
                       trial_paradigm,
                       bpm,
                       page_type,
                       mute_midi_playback = TRUE,
                       asynchronous_api_mode = TRUE) {
  # Randomise

  trial_nos <- sample(1:no_trials)

  trial_nos %>%
    purrr::map(function(trial_no) {

      psychTestR::reactive_page(function(state, ...) {

        page_label <- paste0("play_by_ear_test_trial_", trial_no, "_attempt_1")


        logging::loginfo("trial_no: %s", trial_no)
        logging::loginfo("no_trials: %s", no_trials)

        user_sample <- psychTestR::get_global("trials_sample", state) %>%
            dplyr::slice(n = trial_no)

        quaver_seconds <- 60 / bpm / 2
        trial_dat <- user_sample[trial_no, ]
        no_trials <- nrow(user_sample)

        # For DB storage
        abs_melody <- itembankr::str_mel_to_vector(trial_dat$abs_melody)
        durations <- itembankr::str_mel_to_vector(trial_dat$durations) * quaver_seconds

        additional <- "NA" # For test, we have no visual information

        page_label <- paste0("play_by_ear_learn_trial_",
                             trial_no,
                             "_attempt_",
                             attempt)

        display_modality <- trial_dat$display_modality

        item_id <- trial_dat$item_id
        # Setup DB vars
        db_vars <- create_db_vars_template()
        db_vars$attempt <- attempt
        db_vars$item_id <- item_id
        db_vars$page_label <- page_label
        db_vars$display_modality <- display_modality
        db_vars$phase <- phase
        db_vars$feedback <- FALSE
        db_vars$additional <- additional
        db_vars$test_id <- 2L

        session_id <- psychTestR::get_global("session_id", state) %>% get_promise_value()
        db_vars$session_id <- session_id
        trial_time_started <- Sys.time()
        db_vars$trial_time_started <- trial_time_started
        psychTestR::set_global("trial_time_started", trial_time_started, state)

        db_vars$instrument <- psychTestR::get_global("inst", state)
        db_vars$melody_block_paradigm <- "visual_auditory_learn_test_paradigm"
        db_vars$module <- psychTestR::get_local(".module", state)
        db_vars$rhythmic <- TRUE
        db_vars$user_id <- psychTestR::get_global("user_id", state)


        psychTestR::set_global('melody_block_paradigm', "visual_auditory_learn_test_paradigm", state)
        psychTestR::set_global('number_attempts', attempt, state)
        psychTestR::set_global('additional', additional, state)
        psychTestR::set_global('item_id', item_id, state)
        psychTestR::set_global('page_label', page_label, state)
        psychTestR::set_global("display_modality", display_modality, state)
        psychTestR::set_global("phase", phase, state)

        transpose <- psychTestR::get_global("transpose_visual_notation", state)
        clef <- psychTestR::get_global("clef", state)

        logging::loginfo("display_modality: %s", display_modality)
        logging::loginfo("transpose: %s", transpose)
        logging::loginfo("clef: %s", clef)


        # In the test phase transpose the melody so it is in line with the user's reading clef
        if (phase == "test" && display_modality == "visual" && is.numeric(transpose)) {
          abs_melody <- abs_melody - transpose
          db_vars$stimuli <- abs_melody
        }


        pbet_trial(
          abs_melody,
          durations,
          transpose,
          db_vars,
          clef,
          mute_midi_playback = mute_midi_playback,
          trial_paradigm,
          attempt = 1L,
          bpm,
          page_type,
          trial_no = trial_no,
          no_trials = no_trials,
          page_label = page_label,
          phase = "test",
          asynchronous_api_mode = asynchronous_api_mode
        )

      })

    })
}

pbet_trial <- function(abs_melody,
                       durations,
                       transpose,
                       db_vars,
                       clef,
                       trial_paradigm,
                       attempt,
                       bpm,
                       page_type,
                       trial_no,
                       no_trials,
                       page_label = "play_by_ear",
                       phase = "test",
                       mute_midi_playback = TRUE,
                       asynchronous_api_mode = TRUE) {
  present_stimuli(
    stimuli = abs_melody,
    durations = durations,
    transpose_visual_notation = transpose,
    stimuli_type = "midi_notes",
    display_modality = "auditory",
    page_type = page_type,
    page_title = psychTestR::i18n("Play_the_melody_by_ear"),
    page_text = shiny::tags$div(
      shiny::tags$h3(paste0(
        psychTestR::i18n("Section_Progress"),
        ': ',
        trial_no,
        "/",
        no_trials
      )),
      if (phase != "test")
        shiny::tags$h4(psychTestR::i18n("Attempt"), " ", attempt, "/3"),
      set_melodic_stimuli(abs_melody, durations)
    ),
    trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
    get_answer = get_answer_async_midi_vs_audio,
    give_first_melody_note = TRUE,
    attempt = attempt,
    page_label = page_label,
    db_vars = db_vars,
    mute_midi_playback = mute_midi_playback,
    asynchronous_api_mode = asynchronous_api_mode,
    clef = clef
  )
}

init_trial_time_started <- function(page,
                                    attempt = 1L,
                                    additional = NULL,
                                    item_id = NULL,
                                    page_label = NULL,
                                    display_modality = "auditory",
                                    phase = "test",
                                    trial_time_started = Sys.time()) {
  stopifnot(psychTestR::is.test_element(page))

  psychTestR::join(# With a code_block, rather than reactive_page, we don't get two executions of the function,
    # which causes issues, such as the trial_time_started being the same as trial_time_completed
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("trial_time_started", trial_time_started, state)
      psychTestR::set_global('melody_block_paradigm', "visual_auditory_learn_test_paradigm", state)
      psychTestR::set_global('number_attempts', attempt, state)
      psychTestR::set_global('additional', additional, state)
      psychTestR::set_global('item_id', item_id, state)
      psychTestR::set_global('page_label', page_label, state)
      psychTestR::set_global("display_modality", display_modality, state)
      psychTestR::set_global("phase", phase, state)

    }), page)
}

learn_trial <- function(trial_no,
                        attempt,
                        trial_paradigm,
                        bpm,
                        page_type,
                        phase = "learn",
                        mute_midi_playback = TRUE,
                        asynchronous_api_mode = TRUE) {

  stopifnot(is.numeric(bpm))

  psychTestR::reactive_page(function(state, ...) {

    logging::loginfo("trial_no: %s", trial_no)
    logging::loginfo("no_trials: %s", no_trials)


    if (phase == "example") {
      user_sample <- psychTestR::get_global("examples_sample", state) %>%
        dplyr::slice(n = trial_no)
    } else {
      user_sample <- psychTestR::get_global("trials_sample", state) %>%
        dplyr::slice(n = trial_no)
    }


    quaver_seconds <- 60 / bpm / 2
    trial_dat <- user_sample[trial_no, ]
    no_trials <- nrow(user_sample)

    # For DB storage
    abs_melody <- itembankr::str_mel_to_vector(trial_dat$abs_melody)
    durations <- itembankr::str_mel_to_vector(trial_dat$durations) * quaver_seconds

    musicxml_file <- paste0("assets/", trial_dat$file_name)

    additional <- trial_dat %>%
      dplyr::select(flat_or_sharp, no_sharps_flats, no_bars, file_name) %>%
      dplyr::mutate(bpm = bpm)

    page_label <- paste0("play_by_ear_learn_trial_",
                         trial_no,
                         "_attempt_",
                         attempt)

    display_modality <- trial_dat$display_modality

    item_id <- trial_dat$item_id
    # Setup DB vars
    db_vars <- create_db_vars_template()
    db_vars$attempt <- attempt
    db_vars$item_id <- item_id
    db_vars$page_label <- page_label
    db_vars$display_modality <- display_modality
    db_vars$phase <- phase
    db_vars$feedback <- FALSE
    db_vars$additional <- additional
    db_vars$test_id <- 2L

    session_id <- psychTestR::get_global("session_id", state) %>% get_promise_value()
    db_vars$session_id <- session_id
    trial_time_started <- Sys.time()
    db_vars$trial_time_started <- trial_time_started
    psychTestR::set_global("trial_time_started", trial_time_started, state)

    db_vars$instrument <- psychTestR::get_global("inst", state)
    db_vars$melody_block_paradigm <- "visual_auditory_learn_test_paradigm"
    db_vars$module <- psychTestR::get_local(".module", state)
    db_vars$rhythmic <- TRUE
    db_vars$user_id <- psychTestR::get_global("user_id", state)


    psychTestR::set_global('melody_block_paradigm', "visual_auditory_learn_test_paradigm", state)
    psychTestR::set_global('number_attempts', attempt, state)
    psychTestR::set_global('additional', additional, state)
    psychTestR::set_global('item_id', item_id, state)
    psychTestR::set_global('page_label', page_label, state)
    psychTestR::set_global("display_modality", display_modality, state)
    psychTestR::set_global("phase", phase, state)

    transpose <- psychTestR::get_global("transpose_visual_notation", state)
    clef <- psychTestR::get_global("clef", state)

    logging::loginfo("display_modality: %s", display_modality)
    logging::loginfo("transpose: %s", transpose)
    logging::loginfo("clef: %s", clef)


    # In the test phase transpose the melody so it is in line with the user's reading clef
    if (phase == "test" && display_modality == "visual" && is.numeric(transpose)) {
      abs_melody <- abs_melody - transpose
      db_vars$stimuli <- abs_melody
    }


    if (display_modality == "visual") {
      trial <- learn_trial_visual(
        musicxml_file,
        page_type,
        trial_no,
        no_trials,
        attempt,
        abs_melody,
        durations,
        bpm,
        page_label,
        db_vars,
        mute_midi_playback,
        asynchronous_api_mode
      )

    } else {
      trial <- pbet_trial(
        abs_melody,
        durations,
        transpose,
        db_vars,
        clef,
        trial_paradigm,
        attempt,
        bpm,
        page_type,
        trial_no = trial_no,
        no_trials = no_trials,
        page_label,
        phase = phase,
        asynchronous_api_mode = asynchronous_api_mode
      )
    }

    return(trial)

  })
}


learn_trial_visual <- function(musicxml_file,
                               page_type,
                               trial_no,
                               no_trials,
                               attempt,
                               abs_melody,
                               durations,
                               bpm,
                               page_label,
                               db_vars,
                               mute_midi_playback,
                               asynchronous_api_mode) {
  present_stimuli(
    stimuli = musicxml_file,
    stimuli_type = "musicxml_file",
    display_modality = "visual",
    page_type = page_type,
    sheet_music_start_hidden = TRUE,
    show_sheet_music_after_record = TRUE,
    page_title = psychTestR::i18n("Play_the_melody_from_sight"),
    page_text = shiny::tags$div(
      shiny::tags$h3(paste0(
        psychTestR::i18n("Section_Progress"),
        ': ',
        trial_no,
        "/",
        no_trials
      )),
      shiny::tags$h4(psychTestR::i18n("Attempt"), " ", attempt, "/3"),
      set_melodic_stimuli(abs_melody, durations, musicxml_file),
      shiny::tags$p(psychTestR::i18n("use_metronome_correct_bpm"), " "),
      js_metronome(bpm)
    ),
    page_label = page_label,
    get_answer = get_answer_async_midi_vs_audio,
    show_record_button = TRUE,
    attempt = attempt,
    db_vars = db_vars,
    mute_midi_playback = mute_midi_playback,
    asynchronous_api_mode = asynchronous_api_mode
  )

}

learn_practice_trial <- function(trial_no,
                                 attempt,
                                 page_type,
                                 bpm,
                                 phase = "learn",
                                 mute_midi_playback = TRUE,
                                 asynchronous_api_mode = TRUE) {
  trial <-
    psychTestR::reactive_page(function(state, ...) {
      if (phase == "example") {
        trial_dat <- psychTestR::get_global("examples_sample", state) %>%
          dplyr::slice(n = trial_no)
      } else {
        trial_dat <- psychTestR::get_global("trials_sample", state) %>%
          dplyr::slice(n = trial_no)
      }

      trial_paradigm <- paradigm(page_type = page_type)

      quaver_seconds <- 60 / bpm / 2

      page_label <- paste0(
        "play_by_ear_learn_practice_from_memory_trial_",
        trial_no,
        "_attempt_",
        attempt
      )
      item_id <- trial_dat$item_id
      additional <- if (length(trial_dat$additional) == 0)
        NA
      else
        trial_dat$additional

      # Setup DB vars
      db_vars <- create_db_vars_template()
      db_vars$attempt <- attempt
      db_vars$item_id <- item_id
      db_vars$page_label <- page_label
      db_vars$display_modality <- trial_dat$display_modality
      db_vars$phase <- phase
      db_vars$feedback <- FALSE
      db_vars$additional <- additional
      db_vars$test_id <- 2L


      session_id <- psychTestR::get_global("session_id", state) %>% get_promise_value()
      db_vars$session_id <- session_id

      trial_time_started <- Sys.time()
      db_vars$trial_time_started <- trial_time_started
      psychTestR::set_global("trial_time_started", trial_time_started, state) # For MIDI

      db_vars$instrument <- psychTestR::get_global("inst", state)
      db_vars$melody_block_paradigm <- "visual_auditory_learn_test_paradigm"
      db_vars$module <- psychTestR::get_local(".module", state)
      db_vars$rhythmic <- TRUE
      db_vars$user_id <- psychTestR::get_global("user_id", state)

      present_stimuli(
        stimuli = itembankr::str_mel_to_vector(trial_dat$abs_melody),
        durations = itembankr::str_mel_to_vector(trial_dat$durations) * quaver_seconds,
        stimuli_type = "midi_notes",
        display_modality = "auditory",
        page_type = page_type,
        page_label = page_label,
        page_title = psychTestR::i18n("Play_from_memory"),
        page_text = shiny::tags$div(
          shiny::tags$p(psychTestR::i18n("play_from_memory_message")),
          shiny::tags$script(
            "document.getElementById('playButton').style.display = 'none';"
          )
        ),
        show_record_button = TRUE,
        trigger_end_of_stimulus_fun = trial_paradigm$trigger_end_of_stimulus_fun,
        get_answer = get_answer_async_midi_vs_audio,
        attempt = attempt,
        db_vars = db_vars,
        mute_midi_playback = mute_midi_playback,
        asynchronous_api_mode = asynchronous_api_mode
      )

    })



  trial <- trial %>%
    init_trial_time_started(
      attempt,
      additional = additional,
      item_id = item_id,
      page_label = page_label,
      display_modality = trial_dat$display_modality,
      phase = phase
    )

  return(trial)
}
