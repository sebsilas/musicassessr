

WJD_mini <- WJD::WJD("phrases")  %>% dplyr::slice_sample(n = 10)
WJD_mini$correct <- 0

review_buffer <- function() {
  psychTestR::make_test(
    elts = psychTestR::join(
      psychTestR::code_block(function(state, ...) {
        psychTestR::set_local("more_review_items", TRUE, state)
        psychTestR::set_local("review_items", WJD_mini, state)

      }),
      psychTestR::one_button_page("Welcome!"),
      psychTestR::while_loop(
        test = function(state, ...) {
          psychTestR::get_local("more_review_items",  state)
        },
        logic = psychTestR::join(
          psychTestR::reactive_page(function(state, ...) {
            review_items <- psychTestR::get_local("review_items", state)
            item <- review_items %>% dplyr::slice(1L)
            present_stimuli(stimuli = itembankr::str_mel_to_vector(item$orig_abs_melody),
                            stimuli_type = "midi_notes",
                            display_modality = "visual",
                            page_type = "NAFC_page",
                            label = "item",
                            choices = c("0", "1"),
                            on_complete = function(answer, state, ...) {
                              review_items <- psychTestR::get_local("review_items", state)
                              item <- review_items %>% dplyr::slice(1L)
                              if(nrow(review_items) > 1) {
                                review_items <- review_items %>% dplyr::slice(2L:nrow(review_items))
                                if(answer == "0") { # i.e., if wrong
                                  review_items <- rbind(review_items, item)
                                }
                              } else { # i.e., when there is  only one more trial left
                                if(answer == "0") { # i.e., if wrong
                                  review_items <- item
                                } else {
                                  psychTestR::set_local("more_review_items", FALSE,  state)
                                }

                              }
                              psychTestR::set_local("review_items", review_items,  state)
                            })
          })
        )

      ),
      psychTestR::final_page("This is the end!")
  ),
  opt = psychTestR::test_options(title = "Review Buffer Test",
                                 admin_password = "test",
                                 display = psychTestR::display_options(
                                   left_margin = 1L,
                                   right_margin = 1L,
                                   css = system.file('www/css/style.css', package = "musicassessr")
                                 ),
                                 additional_scripts = musicassessr::musicassessr_js(state = musicassessr_state, visual_notation = TRUE),
                                 languages = c("en")))
}

# review_buffer()
