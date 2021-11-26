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
                         page_title_melody = "Please sing the melody",
                        page_title_long_note = "Sing the note with a \"Dooo\" sound.",
                        page_text_long_note = "Please sing the note after you hear it, then click Stop.",
                         instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.") {


  long_notes_3 <- purrr::map(MAST_octave_3_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page",
                    page_title = page_title_long_note, page_text = page_text_long_note)
  })

  long_notes_4 <- purrr::map(MAST_octave_4_long_notes, function(melody) {
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page",
                    page_title = page_title_long_note, page_text = page_text_long_note)
  })

  melodies_octave_3<- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_3']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title_melody)
  })

  melodies_octave_4 <- apply(MAST_melodies, MARGIN = 1,  function(row) {
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row['octave_4']), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row['durations']),
                    get_answer = musicassessr::get_answer_pyin, page_text = page_text, page_title = page_title_melody)
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

  page <- record_audio_page(label = "sing_hbd",
                            page_text = "Please sing Happy Birthday.",
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



get_dob_page <- function(text = "When is your date of birth?") {
  psychTestR::page(
    ui = shiny::tags$div(
      shiny::tags$p(text),
      shiny::selectInput(inputId = "day", label = "Day", choices = as.character(1:31), width = "40%"),
      shiny::selectInput(inputId = "month", label = "Month", choices = month.name, width = "40%"),
      shiny::selectInput(inputId = "year", label = "Year", choices = as.character(1900:2021), width = "40%"),
      psychTestR::trigger_button("next", "Next")
    ),
    get_answer = function(input, ...) {
      list(day = input$day,
           month = input$month,
           year = input$year)
    })
}

UPEI_extra_questions <- function() {
  psychTestR::join(

    get_dob_page(),

    psychTestR::NAFC_page(label = "father_education",
                          prompt = "Please indicate your father or male guardian’s highest level of education: ",
                          choices = c("some high school", "completed high school", "enrolled in first year university",
                                      "enrolled in 2nd , 3rd, or 4th year university", "completed a community college diploma or degree",
                                      "completed a University Bachelor’s degree or other University Undergraduate Degree",
                                      "enrolled in or completed a Master’s Degree",
                                      "enrolled in or completed a Doctoral Degree",
                                      "other (please specify)",
                                      "do not know",
                                      "prefer not to say")),


    psychTestR::NAFC_page(label = "mother_education",
                          prompt = "Please indicate your mother or female guardian’s highest level of education: ",
                          choices = c("some high school", "completed high school",
                                      "enrolled in first year university",
                                        "enrolled in 2nd , 3rd, or 4th year university",
                                        "completed a community college diploma or degree",
                                        "completed a University Bachelor’s degree or other University Undergraduate Degree",
                                        "enrolled in or completed a Master’s Degree",
                                        "enrolled in or completed a Doctoral Degree",
                                        "other (please specify)",
                                        "do not know",
                                        "prefer not to say")),

    psychTestR::one_button_page("Finally, here are several questions about music-theory knowledge."),

    psychTestR::NAFC_page(label = "music_theory_1",
                          prompt = "Musicians refer to do mi sol as a particular structure. What is the name of that structure?",
                          choices = c("major", "minor", "diminished", "augmented", "not sure")),

    psychTestR::NAFC_page(label = "music_theory_2",
                          prompt = "What triad appears once in the major scale?",
                          choices = c("major", "minor", "diminished", "augmented", "not sure")),

    psychTestR::NAFC_page(label = "music_theory_3",
                          prompt = "What triad has two major thirds?",
                          choices = c("major", "minor", "diminished", "augmented", "not sure")),

    psychTestR::NAFC_page(label = "music_theory_4",
                          prompt = "What triad has two major thirds?",
                          choices = c("major", "minor", "diminished", "augmented", "not sure")),

    psychTestR::NAFC_page(label = "music_theory_5",
                          prompt = "Which chord progression represents a typical ending of a piece of music?",
                          choices = c("I - V", "II - VI", "VI - V", "V - I", "not sure")),

    psychTestR::NAFC_page(label = "music_theory_6",
                          prompt = "Would you like to receive the results of Session 2.",
                          choices = c("yes", "no")),

    psychTestR::text_input_page(label = "music_theory_7",
                                prompt = "If there is any other information you feel is important regarding your knowledge of popular music or any aspect of this questionnaire, please feel free to give a brief description below: "),

    psychTestR::NAFC_page(label = "prize_draw",
                          prompt = "Would you like to enter the draw (1 of 25 chances to win $50)?",
                          choices = c("yes", "no")),

    psychTestR::NAFC_page(label = "bonus_credits",
                          prompt = "For students currently enrolled in Psychology 1010 Introductory Psychology:  Would you like to receive a bonus point toward your Psychology 1010 lab grade?",
                          choices = c("yes", "no")),

    psychTestR::conditional(test = function(state, answer, ...) {
      psychTestR::answer(state) == "yes" },
                          logic = psychTestR::NAFC_page(label = "upei_professor",,
                                     prompt = "Please indicate which is your professor: ",
                                     choices = c("Dr. Stacey MacKinnon",
                                                 "Dr. Philip Smith",
                                                 "Prof. Cheryl Wartman",
                                                 "Dr.  Elizabeth Williams"))),

   psychTestR::final_page(body = shiny::tags$div(
     shiny::tags$p("You have now completed all the questions in this survey.  If you are interested in knowing more about the study, relevant information is provided in the following debriefing statement: "),
     shiny::tags$h1("Learning and Memory for Popular Music and Imitation of Brief Melodies"),
     shiny::tags$h2("Debriefing Statement: Session 2"),
     shiny::tags$p("We would like to express our thanks and appreciation for your participation in Session 2 of this research project. Your contribution helps to advance our understanding of the knowledge acquired about popular music over the lifetime, and to specifically address the question of whether there is a time of life when such knowledge is easier to obtain than at other times. "),
     shiny::tags$p("Session 2 focused on imitation of tones and brief melodies.  The study consisted of the presentation of long notes and short melodies that you were asked to imitate."),
     shiny::tags$p("This research is part of a larger study looking into the relationship between adolescence and musical knowledge acquisition.  Part of musical knowledge is singing.  Singing is a musical behavior acquired naturally early in life, just like language is acquired.  In comparison to language acquisition, relatively little attention has been paid to singing development.  The study in which you participated aimed to obtain some basic data regarding singing accuracy (expecting performance to become less accurate with increasing numbers of notes to remember) and comparing males and females (where males have greater challenges to singing, due to issues of voice change in adolescence). "),
     shiny::tags$p("This study is the first in which information on singing (from Session 2) will be related to information on knowledge and memory for popular music (from Session 1).  The overall interest of the study is whether there is a time during adolescence when it is easiest to acquire musical information.  The specific question that can be answered by the vocal imitation study is whether better vocal accuracy is associated with better memory for music. Also, for the first time, this experiment was conducted using the participant’s own computer in the participant’s own quiet environment.  In the past the study has been conducted in a laboratory environment.  We are interested in determining the extent to which the individual differences in equipment and setting will affect the variability of the data."),
     shiny::tags$p("Your participation has helped students in the laboratory gain experience relevant to their honours degrees, and it has contributed to the exploration of important new research questions about how the developmental period of adolescence is related to musical knowledge. It also will add to our understanding of basic vocal abilities in older adolescents and early adulthood, for which very little information has been available."),
     shiny::tags$p("If you have any further questions regarding this research study please feel free to contact  Kristen Gallant, at kbgallant5470@upei.ca (902-566-6023 – laboratory phone); Dr. Amy Simon, 902-566-6023; Dr. Annabel Cohen at acohen@upei.ca, 902-628-4325  (office phone)."),
     shiny::tags$p("If you have indicated your interest in receiving a summary of the results of the study, you will be provided a link to this information by April 30, 2022."),
     shiny::tags$p("Thank you for all your help!  Your contribution to this research is very much appreciated.")))

    )
}

# psychTestR::make_test(UPEI_extra_questions())


#' UPEI Battery
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
UPEI_2021_battery <- function(state = "production") {

  psychTestR::make_test(
    psychTestR::join(
    psychTestR::new_timeline(psychTestR::join(

      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h1("UPEI 2021 Testing"),
        shiny::tags$p("This is a protocol for the UPEI 2021 singing study."),
        musicassessr_js_scripts(musicassessr_state = state)
        )),

    psychTestR::get_p_id(prompt = shiny::tags$div(
      shiny::tags$p("Please provide your participation identifier below created from:"),
      shiny::tags$ul(
      shiny::tags$li("1st 3 letters of the first name of your parent, guardian, or relative"),
      shiny::tags$li("Day of your birthday (2 numbers – 01 to 31)"),
      shiny::tags$li("1st 3 letters of the street you lived on growing up"),
      shiny::tags$br(),
      shiny::tags$p("For example: joh11tav"))))

      ),dict = musicassessr::dict(NULL)),


    MST::MST(num_items = list(
               long_tones = 6L, arrhythmic = 10L, rhythmic = 0L
             ),
             examples = 2L,
             final_results = FALSE,
             state = state,
             absolute_url = "https://adaptiveeartraining.com",
             SNR_test = TRUE,
             get_range = TRUE,
             gold_msi = FALSE,
             demographics = FALSE,
             with_final_page = FALSE,
             melody_sound = "piano"),

    psychTestR::new_timeline(psychTestR::join(
      musicassessr::get_voice_range_page(with_examples = FALSE),

      psychTestR::code_block(function(state, ...) {
        snap <- sample(1:2, 1)
        psychTestR::set_global("snap", snap, state)
      }),

      musicassessr::sing_happy_birthday_page(feedback = FALSE),

      psychTestR::conditional(test = function(state, ...) {
        psychTestR::get_global("snap", state) == 1
      }, logic = psychTestR::join (
        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

        psychTestR::module(label = "MAST21_daah",
          musicassessr::MAST21_trials(sound = "piano",
                                    page_title_long_note = "Please sing back the note with a \"Daah\" sound.",
                                    page_title_melody = "Please sing back the melody with a \"Daah\" sound.")
        ),

        musicassessr::sing_happy_birthday_page(feedback = FALSE),

        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

        psychTestR::module(label = "MAST21_dooo",
          musicassessr::MAST21_trials(sound = "piano",
                                      page_title_long_note = "Please sing back the note with a \"Dooo\" sound.",
                                      page_title_melody = "Please sing back the melody with a \"Dooo\" sound.")
        )

       )),

      psychTestR::conditional(test = function(state, ...) {
        psychTestR::get_global("snap", state) == 2
      }, logic = psychTestR::join(
        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

        psychTestR::module(label = "MAST21_dooo",
          musicassessr::MAST21_trials(sound = "piano",
                                      page_title_long_note = "Please sing back the note with a \"Dooo\" sound.",
                                      page_title_melody = "Please sing back the melody with a \"Dooo\" sound.")),

        musicassessr::sing_happy_birthday_page(feedback = FALSE),

        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

        psychTestR::module(label = "MAST21_daah",
          musicassessr::MAST21_trials(sound = "piano",
                                      page_title_long_note = "Please sing back the note with a \"Daah\" sound.",
                                      page_title_melody = "Please sing back the melody with a \"Daah\" sound."))


      )),

      musicassessr::sing_happy_birthday_page(feedback = FALSE)
      ) ,dict = musicassessr::dict(NULL)),

    PDT::PDT(with_final_page = FALSE),

    mpt::mpt(num_items = 10L),

    mdt::mdt(num_items = 10L),

    psyquest::GMS(),

    psyquest::SES(),

    psychTestR::one_button_page(shiny::tags$div(
      musicassessr_js_scripts(musicassessr_state = state),
      shiny::tags$p("On the next page you will sing Happy Birthday again."))),

    musicassessr::sing_happy_birthday_page(feedback = FALSE),

    UPEI_extra_questions()

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


# library(PDT)
# library(mpt)
# library(mdt)
# library(psyquest)
# library(MST)
# library(musicassessr)

# UPEI_2021_battery("test")



# https://stackoverflow.com/questions/39322089/node-js-port-3000-already-in-use-but-it-actually-isnt


