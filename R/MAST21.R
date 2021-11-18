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
                        long_tone_title_doo = "Sing the note with a \"Dooo\" sound.",
                        long_tone_title_daa = "Sing the note with a \"Daah\" sound.",
                        long_tone_text = "Please sing the note after you hear it, then click Stop.",
                         instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.") {

  if(sound == "voice_doo") {
    page_title <- page_title_doo
    long_tone_title <- long_tone_title_doo
  } else if(sound == "voice_daa") {
    page_title <- page_title_daa
    long_tone_title <- long_tone_title_daa
  } else {
    page_title <- "Please sing the melody"
    long_tone_title <- "Please sing the note"
  }

  long_notes_3 <- purrr::map(MAST_octave_3_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page",
                    page_title = long_tone_title, page_text = long_tone_text)
  })

  long_notes_4 <- purrr::map(MAST_octave_4_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page",
                    page_title = long_tone_title, page_text = long_tone_text)
  })

  melodies_octave_3<- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_3']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title)
  })

  melodies_octave_4 <- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_4']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title)
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
                      )
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



# library(PDT)
# library(mpt)
# library(mdt)
# library(psyquest)
# library(MST)
# library(musicassessr)


UPEI_2021_battery <- function(state = "production",
                              aws_credentials = list(api_url = "https://255uxe6ajl.execute-api.us-east-1.amazonaws.com/api",
                                                     bucket_name = "shinny-app-source-41630",
                                                     bucket_region = "us-east-1",
                                                     identity_pool_id = "us-east-1:feecdf7e-cdf6-416f-94d0-a6de428c8c6b",
                                                     destination_bucket = "shinny-app-destination-41630")) {


  musicassessr::make_aws_credentials_global(aws_credentials)

  psychTestR::make_test(
    psychTestR::join(
    psychTestR::new_timeline(psychTestR::join(
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h1("UPEI 2021 Testing"),
        shiny::tags$p("This is a protocol for the UPEI 2021 singing study."),
        musicassessr_js_scripts(api_url = aws_credentials$api_url,
                                bucket_name = aws_credentials$bucket_name,
                                bucket_region = aws_credentials$bucket_region,
                                identity_pool_id = aws_credentials$identity_pool_id,
                                destination_bucket = aws_credentials$destination_bucket,
                                musicassessr_state = state),
        )),

    psychTestR::get_p_id(prompt = shiny::tags$div(
      shiny::tags$p("Please provide your participation identifier below created from:"),
      shiny::tags$ul(
      shiny::tags$li("1st 3 letters of the first name of your parent, guardian, or relative"),
      shiny::tags$li("Day of your birthday (2 numbers – 01 to 31)"),
      shiny::tags$li("1st 3 letters of the street you lived on growing up"),
      shiny::tags$br(),
      shiny::tags$p("For example: joh11tav")))),

    musicassessr::microphone_calibration_page(),

    musicassessr::get_voice_range_page(with_examples = FALSE),

    musicassessr::sing_happy_birthday_page(feedback = TRUE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

    musicassessr::MAST21_trials(sound = "voice_daa"),

    musicassessr::sing_happy_birthday_page(feedback = TRUE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

    musicassessr::MAST21_trials(sound = "voice_doo"),

    musicassessr::sing_happy_birthday_page(feedback = TRUE)
      ),dict = musicassessr::dict(NULL)),

    MST::MST(aws_credentials = aws_credentials,
        num_items = list(
          long_tones = 6L, arrhythmic = 10L, rhythmic = 0L
        ),
        examples = 2L,
        final_results = FALSE,
        state = state,
        absolute_url = "https://adaptiveeartraining.com",
        SNR_test = FALSE,
        get_range = FALSE,
        gold_msi = FALSE,
        demographics = FALSE,
        with_final_page = FALSE),

    PDT::PDT(with_final_page = FALSE),

    mpt::mpt(),

    mdt::mdt(),

    psyquest::GMS(),

    psyquest::SES(),

    musicassessr::sing_happy_birthday_page(feedback = TRUE),

    psychTestR::final_page("Thank you for participating!")
  ),
  opt = psychTestR::test_options(title = "UPEI",
                           admin_password = "test",
                           display = psychTestR::display_options(
                             left_margin = 1L,
                             right_margin = 1L,
                             css = system.file('www/css/style.css', package = "musicassessr")
                           ),
                           languages = c("en"))
  )

}

#UPEI_2021_battery("test")



# https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt


