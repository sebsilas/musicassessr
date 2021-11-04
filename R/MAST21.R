MAST_long_notes <- tibble::tibble(
  melody = c("F", "B", "E", "C"),
  octave_3 = c(53, 59, 52, 48),
  octave_4 = c(65, 71, 64, 60)
)

MAST_octave_3_long_notes <- MAST_long_notes$octave_3
MAST_octave_4_long_notes <- MAST_long_notes$octave_4



MAST21_trials <- function(item_bank, num_items, num_examples = NULL, feedback = FALSE,
                                   get_answer = musicassessr::get_answer_pyin,
                                   sound = "piano",
                                   page_text = psychTestR::i18n("sing_melody_trial"),
                                   page_title = "Sing This Melody Plus Rhythm",
                                   instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.") {


  long_notes_3 <- purrr::map(MAST_octave_3_long_notes, function(melody) {
    play_long_tone_record_audio_page(note = melody)
  })

  long_notes_4 <- purrr::map(MAST_octave_4_long_notes, function(melody) {
    play_long_tone_record_audio_page(note = melody)
  })

  if(feedback & !is.function(feedback)) {
    feedback <- feedback_melodic_production
  }

  psychTestR::module("MAST21",
                     c(
                       # instructions

                      # long notes 1-4

                      psychTestR::conditional(
                        test = function(state, ...) {
                          range <- psychTestR::get_global("range", state)
                          range == "Baritone" | range == "Bass" | range == "Tenor"
                        },
                        logic = long_notes_3
                      ),

                      psychTestR::conditional(
                        test = function(state, ...) {
                          range <- psychTestR::get_global("range", state)
                          range == "Alto" | range == "Soprano"
                        },
                        logic = long_notes_4
                      ),

                       ## melody trials
                       musicassessr::multi_page_play_melody_loop(
                         stimuli_type = "midi_notes",
                         var_name = "MAST21",
                         presampled_items = itembankr::MAST21("phrases"),
                         page_title = page_title,
                         page_text = page_text,
                         get_answer = get_answer,
                         rel_to_abs_mel_function = pitch_classes_into_3_or_4,
                         feedback = feedback,
                         sound = sound
                       )
                     )
  )
}


pitch_classes_into_3_or_4 <- function(rel_melody, range, bottom_range = NULL, top_range = NULL, transpose = NULL) {
  # given some pitch classes and a vocal range, put the pitches in octave 3 or 4

  if(is.character(range)) {
    vocal_range <- vocal_ranges[[range]]
  }

  F3 <- 53
  F4 <- 65

  melody_start_in_3 <- itembankr::rel_to_abs_mel(rel_melody, start_note = F3)
  melody_start_in_4 <- itembankr::rel_to_abs_mel(rel_melody, start_note = F4)

  print(melody_start_in_3)
  print(melody_start_in_4)

  no_in_3 <- sum(as.numeric(melody_start_in_3 %in% vocal_range))
  no_in_4 <- sum(as.numeric(melody_start_in_4 %in% vocal_range))

  if(no_in_3 > no_in_4) {
    melody_to_use <- melody_start_in_3
  } else if (no_in_3 < no_in_4) {
    melody_to_use <- melody_start_in_4
  } else {
    # otherwise choose randomly
    flip <- sample(1:2, 1)
    if(flip == 1) {
      melody_to_use <- melody_start_in_4
    } else {
      melody_to_use <- melody_start_in_3
    }
  }

  if(!is.null(transpose) & !is.na(transpose) & is.numeric(transpose)) {
    melody_to_use <- melody_to_use + transpose
  }
  melody_to_use
}


sing_happy_birthday_page <- function(feedback = FALSE) {

  page <- record_audio_page(page_text = "Please sing Happy Birthday.",
                            get_answer = get_answer_simple_pyin_summary,
                            auto_next_page = TRUE)

  if(feedback) {
    c(
      page,
      psychTestR::reactive_page(function(state, answer, ...) {
        psychTestR::one_button_page(
          shiny::tags$div(
            shiny::tags$h1("Output"),
            shiny::tags$p(paste0('Min: ', answer$Min.)),
            shiny::tags$p(paste0('Max: ', answer$Max.)),
            shiny::tags$p(paste0('Mean: ', answer$Mean)),
            shiny::tags$p(paste0('Median: ', answer$Median))
          )
        )
      })
    )
  } else {
    page
  }
}

m <- function() {

  make_aws_credentials_global(list(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                                   bucket_name = "shinny-app-source-41630",
                                   bucket_region = "us-east-1",
                                   identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                                   destination_bucket = "shinny-app-destination-41630"))

  psychTestR::make_test(psychTestR::new_timeline(psychTestR::join(
    psychTestR::one_button_page(shiny::tags$div(
      tags$h1("MAST-21 Test Battery"),
      tags$p("This is a test protocol for the new MAST-21 battery"),
      musicassessr_js_scripts(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                              bucket_name = "shinny-app-source-41630",
                              bucket_region = "us-east-1",
                              identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                              destination_bucket = "shinny-app-destination-41630"))),

    psychTestR::one_button_page(tags$div(
      tags$p("The next page will show an example of the Sing Happy Birthday page, followed by a summary page of the MIDI pitch range.")
    )),

    sing_happy_birthday_page(feedback = TRUE),

    psychTestR::one_button_page(tags$div(
      tags$p("The next page will show an example of the page which allows the participant to choose a vocal range.")
    )),

    present_voice_ranges_page(),

    psychTestR::one_button_page(tags$div(
      tags$p("Now this has been selected, the MAST21 trials will be presented according to this range.")
    )),

    MAST21_trials(),

    psychTestR::final_page("That's it for now!")
  ), dict = psychTestR::i18n_dict$new(musicassessr_dict_df)), opt = psychTestR::test_options(title = "test", admin_password = "demo"))

}


# mel <- itembankr::str_mel_to_vector(itembankr::MAST21[12, "melody", drop = TRUE])
#
# pitch_classes_into_3_or_4(melody = mel, vocal_range = "Bass")



