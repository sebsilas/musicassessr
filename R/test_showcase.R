
#' Launch the musical test showcase of musicassessr tests
#'
#' @param app_name
#' @param musicassessr_aws
#' @param feedback
#' @param num_items_rtt
#' @param num_items_saa
#' @param num_items_pbet
#' @param showcase_name
#'
#' @return
#' @export
#'
#' @examples
musical_test_showcase <- function(app_name = "test_showcase",
                                  musicassessr_aws = FALSE,
                                  feedback = TRUE,
                                  num_items_rtt = list(free_recall = 2L, sync_beat = 2L, call_and_response = 2L),
                                  num_items_saa = list(long_tones = 2L, arrhythmic = 2L, rhythmic = 2L),
                                  num_items_pbet = list(
                                    find_this_note = 0L,
                                    arrhythmic = list(key_hard = 1L, key_easy = 1L),
                                    rhythmic = list(key_hard = 1L, key_easy = 1L),
                                    interval_perception = 0L,
                                    wjd_audio = list(key_easy = 0L, key_hard = 0L)),
                                   showcase_name = "musicassessr") {

  rtt_feedback <- if(feedback) RTT::rhythm_feedback(type = "researcher") else RTT::rhythm_feedback(type = "none")

  RTT_built <- RTT::RTT(page_type = "record_midi_page", feedback = rtt_feedback, num_items_rtt, call_and_response_end = "manual")

  tl <- function() {
      psychTestR::join(

      heading_page("Rhythm Tapping Test (RTT)",
                   after = shiny::tags$div(shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/drum.png'))),

      # # Rhythm Tapping Test
      wrap_musicassessr_timeline(RTT_built()),

      heading_page("Singing Ability Assessment",
                   after = shiny::tags$div(shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/SAA_intro.png'))),

      # Singing Ability Test
      SAA::SAA(app_name = app_name,
               num_items = num_items_saa,
               skip_setup = 'except_microphone',
               examples = 0L,
               default_range = list(bottom_range = 43, top_range = 69), # Seb default range
               musicassessr_aws = musicassessr_aws,
               gold_msi = FALSE,
               demographics = FALSE,
               feedback = feedback,
               final_results = FALSE,
               max_goes = 1L),

      heading_page("Play By Ear Test",
                   after = shiny::tags$div(
                     shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/music.png', height = 100, width = 100),
                     shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/img/saxophone.png', height = 100, width = 100),
                   )),

      # Playing By Ear Test
      PBET::PBET(app_name = app_name,
                 musicassessr_aws = musicassessr_aws,
                 input_type = "midi_keyboard",
                 num_items = num_items_pbet,
                 max_goes = 1L,
                 examples = PBET::no_examples(),
                 default_range = musicassessr::set_default_range("Piano"),
                 gold_msi = FALSE,
                 demographics = FALSE,
                 give_first_melody_note = TRUE,
                 feedback = feedback)
      )
  }

  musicassessr::make_musicassessr_test(
    welcome_page = heading_page(heading = paste0("Welcome to the ", showcase_name, " showcase!"), after = shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/BLUESongBirdLogo.png', width = 500, height = 189)),
    final_page = psychTestR::final_page(shiny::tags$div(shiny::tags$h1(paste0("Thank you for completing the ", showcase_name, " showcase!")), shiny::tags$img(src = 'https://adaptiveeartraining.com/assets/BLUESongBirdLogo.png', width = 500, height = 189))),
    elts = tl,
    title = paste0(showcase_name, " Showcase"),
    admin_password = "demo",
    opt = musicassessr::musicassessr_opt(app_name = app_name,
                                         midi_input = TRUE,
                                         record_audio = TRUE,
                                         visual_notation = TRUE,
                                         musicassessr_aws = musicassessr_aws,
                                         setup_options = setup_pages_options(skip_setup = TRUE))
  )


}



