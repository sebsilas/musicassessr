

MAST_low_wavs_ordered <-  c("1_F_low.wav",
                            "2_B_low.wav",
                            "3_E_low.wav",
                            "4_C_low.wav",
                            "5_FF_low.wav",
                            "6_FC_low.wav",
                            "7_FE_low.wav",
                            "8_FB_low.wav" ,
                            "9_FAC_low.wav",
                            "10_FAbC_low.wav",
                            "11_FAbCb_low.wav",
                            "12_FACs_low.wav",
                            "13_FACAF_low.wav",
                            "14_FAbCAbF_low.wav",
                            "15_FAbCbAbF_low.wav",
                            "16_FACsAF_low.wav",
                            "17_BJ1_low.wav",
                            "18_BJ2_low.wav",
                            "19_BJ3_low.wav",
                            "20_BJ4_low.wav",
                            "21_BJfull_low.wav")

MAST_high_wavs_ordered <- c("1_F_high.wav",
                             "2_B_high.wav",
                             "3_E_high.wav",
                             "4_C_high.wav",
                             "5_FF_high.wav",
                             "6_FC_high.wav",
                             "7_FE_high.wav",
                             "8_FB_high.wav" ,
                             "9_FAC_high.wav",
                             "10_FAbC_high.wav",
                             "11_FAbCb_high.wav",
                             "12_FACs_high.wav",
                             "13_FACAF_high.wav",
                             "14_FAbCAbF_high.wav",
                             "15_FAbCbAbF_high.wav",
                             "16_FACsAF_high.wav",
                             "17_BJ1_high.wav",
                             "18_BJ2_high.wav",
                             "19_BJ3_high.wav",
                             "20_BJ4_high.wav",
                             "21_BJfull_high.wav")


MAST_wav <- function(trial_type = c("normal", "daa", "doo"),
                     high_or_low = c("high", "low")) {

  if(high_or_low == "high") {
    file_dir <- 'musicassessr-assets/MAST21_high/'
    files_list <- MAST_high_wavs_ordered

  } else {
    file_dir <- 'musicassessr-assets/MAST21_low/'
    files_list <- MAST_low_wavs_ordered
  }

  text_note_daa <- "Please sing back the note with a 'Daa' sound then click 'Stop'."
  text_melody_daa <- "Please sing back the melody with a 'Daa' sound then click 'Stop'."
  text_note_doo <- "Please sing back the note with a 'Doo' sound then click 'Stop'."
  text_melody_doo <- "Please sing back melody note with a 'Doo' sound then click 'Stop'."
  text_note <- "Please sing back the note then click 'Stop'."
  text_melody <- "Please sing back the melody then click 'Stop'."

  res <- lapply(files_list, function(file) {

    if(startsWith(file, "1_") | startsWith(file, "2_") |
       startsWith(file, "3_") | startsWith(file, "4_")) {

      if(trial_type == "daa") {
        text <- text_note_daa
      } else if(trial_type == "doo") {
        text <- text_note_doo
      } else {
        text <- text_note
      }

    } else {
      if(trial_type == "daa") {
        text <- text_melody_daa
      } else if(trial_type == "doo") {
        text <- text_melody_doo
      } else {
        text <- text_melody
      }
    }

    x <- paste0(file_dir,  file)
    page_lab <- paste0("MAST21_", high_or_low, "_", which(files_list == file))

    present_stimuli(
      stimuli = x,
      stimuli_type = "audio",
      display_modality = "auditory",
      page_type = "record_audio_page",
      page_text = text,
      hideOnPlay = TRUE,
      auto_next_page = TRUE,
      page_label = page_lab)
  })

  res <- insert.every.other.pos.in.list(res,
                                        psychTestR::elt_save_results_to_disk(complete = FALSE))

  psychTestR::module(paste0("MAST21_", high_or_low),
                     res)

}




#MAST_wav(trial_type = "doo", high_or_low = "high")
#MAST_wav(trial_type = "doo", high_or_low = "low")





MAST21_wav_block_daa <- psychTestR::module("MAST21_daa",

  psychTestR::join(

    psychTestR::conditional(
     test = function(state, ...) {
       range <- psychTestR::get_global("range", state)
       range %in% c("Baritone", "Bass", "Tenor")
     },
     logic = MAST_wav(trial_type = "daa", high_or_low = "low")
    ),

    psychTestR::conditional(
     test = function(state, ...) {
       range <- psychTestR::get_global("range", state)
       range %in% c("Alto", "Soprano")
     },
     logic = MAST_wav(trial_type = "daa", high_or_low = "high")
    )
))



MAST21_wav_block_doo <- psychTestR::module("MAST21_doo",

  psychTestR::join(

   psychTestR::conditional(
     test = function(state, ...) {
       range <- psychTestR::get_global("range", state)
       range %in% c("Baritone", "Bass", "Tenor")
     },
     logic = MAST_wav(trial_type = "doo", high_or_low = "low")
   ),

   psychTestR::conditional(
     test = function(state, ...) {
       range <- psychTestR::get_global("range", state)
       range %in% c("Alto", "Soprano")
     },
     logic = MAST_wav(trial_type = "doo", high_or_low = "high")
   )
  ))



MAST21_wav <- function(state = "production",
                   include_microphone_calibration_page = FALSE,
                   set_musicassessr_state = FALSE) {

  psychTestR::module("MAST21",
    psychTestR::new_timeline(psychTestR::join(

      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p("You will now have another test of short singing examples.
                        There are 2 sets of 21 questions.
                        The first 20 are very short. Like the previous test, you will hear a melody and be asked to imitate. Unlike the previous test, there is only one chance with each imitation.
                        You will be asked to sing the two sets of questions on two different syllables /da/ and /du/. ")
      )),

      if(include_microphone_calibration_page) microphone_calibration_page(),

      musicassessr::get_voice_range_page(with_examples = FALSE),

      psychTestR::elt_save_results_to_disk(complete = FALSE),


      psychTestR::code_block(function(state, ...) {
        snap <- sample(1:2, 1)
        psychTestR::set_global("snap", snap, state)
      }),

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd1"),

      psychTestR::elt_save_results_to_disk(complete = FALSE),


      psychTestR::conditional(test = function(state, ...) {
        psychTestR::get_global("snap", state) == 1
      }, logic = psychTestR::join (
        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

        MAST21_wav_block_daa,

        psychTestR::elt_save_results_to_disk(complete = FALSE),


        musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd2"),

        psychTestR::elt_save_results_to_disk(complete = FALSE),


        psychTestR::one_button_page(shiny::tags$div(
          shiny::tags$p("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."))),

        MAST21_wav_block_doo

      )),

      psychTestR::conditional(test = function(state, ...) {
        psychTestR::get_global("snap", state) == 2
      }, logic = psychTestR::join(
        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),

        MAST21_wav_block_doo,

        psychTestR::elt_save_results_to_disk(complete = FALSE),

        musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd3"),

        psychTestR::elt_save_results_to_disk(complete = FALSE),


        psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Daah\" sound."),

        MAST21_wav_block_daa

      )),

      psychTestR::elt_save_results_to_disk(complete = FALSE),

      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd4")
    ), dict = musicassessr::dict(NULL))
  )
}





MAST21_wav_block <- MAST21_wav(include_microphone_calibration_page = TRUE)


usethis::use_data(MAST21_wav_block, internal = TRUE, overwrite = TRUE)



