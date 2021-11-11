#' Set whether a musicassessr test is running in production or locally (test)
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
set_musicassessr_state <- function(state = c("production", "test")) {
  musicassessr_state <<- state
  shiny::tags$script(paste0("const musicassessr_state = \'", musicassessr_state, "\';"))
}



F3 <- 53
F4 <- 65


MAST_long_notes <- tibble::tibble(
  melody = c("F", "B", "E", "C"),
  octave_3 = c(53, 59, 52, 48),
  octave_4 = c(65, 71, 64, 60)
)

MAST_octave_3_long_notes <- MAST_long_notes$octave_3
MAST_octave_4_long_notes <- MAST_long_notes$octave_4

MAST_melodies <- itembankr::MAST21("phrases")

MAST_melodies$octave_3 <- apply(MAST_melodies, MARGIN = 1, function(row) {
  transpose <- ifelse(is.na(row['transpose']), 0, as.integer(row['transpose']))
  paste0(itembankr::rel_to_abs_mel(rel_mel = itembankr::str_mel_to_vector(row['melody']), start_note = F3) + transpose, collapse = ",")
})

MAST_melodies$octave_4 <- apply(MAST_melodies, MARGIN = 1, function(row) {
  transpose <- ifelse(is.na(row['transpose']), 0, as.integer(row['transpose']))
  paste0(itembankr::rel_to_abs_mel(rel_mel = itembankr::str_mel_to_vector(row['melody']), start_note = F4) + transpose, collapse = ",")
})


#' Deploy a block of the MAST21 stimuli
#'
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_text
#' @param page_title_doo
#' @param page_title_daa
#' @param long_tone_title
#' @param long_tone_text
#' @param instruction_text
#' @param microphone_calibration_page
#'
#' @return
#' @export
#'
#' @examples
MAST21_trials <- function(item_bank, num_items, num_examples = NULL, feedback = FALSE,
                         get_answer = musicassessr::get_answer_pyin,
                         sound = "piano",
                         page_text = psychTestR::i18n("sing_melody_trial"),
                         page_title_doo = "Sing this melody with a \"Dooo\" sound.",
                         page_title_daa = "Sing this melody with a \"Daah\" sound.",
                        long_tone_title = "Sing the note",
                        long_tone_text = "Please sing the note after you hear it, then click Stop.",
                         instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.",
                        microphone_calibration_page = TRUE) {


  long_notes_3 <- purrr::map(MAST_octave_3_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = "tone",
                    note_length = 4, page_type = "record_audio_page",
                    page_title = long_tone_title, page_text = long_tone_text)
  })

  long_notes_4 <- purrr::map(MAST_octave_4_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = "tone",
                    note_length = 4, page_type = "record_audio_page",
                    page_title = long_tone_title, page_text = long_tone_text)
  })

  melodies_octave_3<- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_3']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title_daa)
  })

  melodies_octave_4 <- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_4']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title_daa)
  })


  if(feedback & !is.function(feedback)) {
    feedback <- feedback_melodic_production
  }

  psychTestR::module("MAST21",
                     c(
                       # instructions

                       if(microphone_calibration_page) microphone_calibration_page(),

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


                      psychTestR::conditional(
                        test = function(state, ...) {
                          range <- psychTestR::get_global("range", state)
                          range == "Baritone" | range == "Bass" | range == "Tenor"
                        },
                        logic = melodies_octave_3
                      ),

                      psychTestR::conditional(
                        test = function(state, ...) {
                          range <- psychTestR::get_global("range", state)
                          range == "Alto" | range == "Soprano"
                        },
                        logic = melodies_octave_4
                      ),

                      psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

                     )
  )
}



#' A page to identify a user's singing range by asking them to sing Happy Birthday
#'
#' @param feedback
#'
#' @return
#' @export
#'
#' @examples
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


#library(PDCT)
#library(mpt)
#library(mdt)
#library(psyquest)


UPEI_2021_battery <- function(state = "production",
                              aws_credentials = list(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                                                     bucket_name = "shinny-app-source-41630",
                                                     bucket_region = "us-east-1",
                                                     identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                                                     destination_bucket = "shinny-app-destination-41630")) {

  make_aws_credentials_global(aws_credentials)

  psychTestR::make_test(psychTestR::new_timeline(psychTestR::join(
    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$h1("MAST-21 Test Battery"),
      shiny::tags$p("This is a test protocol for the new MAST-21 battery"),
      musicassessr_js_scripts(api_url = aws_credentials$api_url,
                              bucket_name = aws_credentials$bucket_name,
                              bucket_region = aws_credentials$bucket_region,
                              identity_pool_id = aws_credentials$identity_pool_id,
                              destination_bucket = aws_credentials$destination_bucket),
      set_musicassessr_state(state)
      )),

    psychTestR::get_p_id(prompt = shiny::tags$div(
      shiny::tags$p("Please provide your participation identifier below created from:"),
      shiny::tags$ul(
      shiny::tags$li("1st 3 letters of the first name of your parent, guardian, or relative"),
      shiny::tags$li("Day of your birthday (2 numbers – 01 to 31)"),
      shiny::tags$li("1st 3 letters of the street you lived on growing up"),
      shiny::tags$br(),
      shiny::tags$p("For example: joh11tav")))),

    get_voice_range_page(with_examples = FALSE),

    sing_happy_birthday_page(feedback = TRUE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

    MAST21_trials(sound = "voice_daa"),

    sing_happy_birthday_page(feedback = TRUE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),


    MAST21_trials(sound = "voice_doo"),

    sing_happy_birthday_page(feedback = TRUE),

    MST(aws_credentials = aws_credentials,
        num_items = list(
          long_tones = 6L, arrhythmic = 12L, rhythmic = 0L
        )),

    PDT::PDT(),

    mpt::mpt(),

    mdt::mdt(),

    psyquest::GMS(),

    psyquest::SES(),

    sing_happy_birthday_page(feedback = TRUE),

    psychTestR::final_page("Thank you for participating!")
  ), dict = psychTestR::i18n_dict$new(musicassessr_dict_df)), opt = psychTestR::test_options(title = "test", admin_password = "demo"))

}

#UPEI_2021_battery()


