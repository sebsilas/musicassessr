
# NB: completely decouple the visualising table stuff from the main functionality
# i.e., the main functionality shouldn't be based around the table displaying at all

#WJD_mini <- WJD::WJD("phrases")  %>% dplyr::slice_sample(n = 10)
#WJD_mini$correct <- 0
#WJD_mini$time_next_due <- Sys.time() + lubridate::minutes(1)


sample_from_item_bank_and_remove <- function(state) {
  item_bank <- psychTestR::get_local("item_bank", state)
  sample_i <- sample(1:nrow(item_bank), 1)
  item <- item_bank %>% dplyr::slice(sample_i)
  item_bank <- item_bank %>% dplyr::slice(-sample_i)

  if(nrow(item_bank) == 0) {
    item_bank <- FALSE
  }
  psychTestR::set_local("item_bank", item_bank, state)
  return(item)
}

timebound_review <- function(answer, state, minimum_complete = 5, ...) {

  review_items <- psychTestR::get_local("review_items", state)
  item_bank <- psychTestR::get_local("item_bank", state)
  item <- psychTestR::get_local("item", state)
  complete_items <- psychTestR::get_local("complete_items", state)

  if(nrow(review_items) > 1 | nrow(complete_items) < minimum_complete) {
    if(answer == "1") { # i.e., if correct
      psychTestR::set_local("complete_items", rbind(complete_items, item), state)
    } else {
      item$time_next_due <- Sys.time() + lubridate::minutes(1)
      review_items <- rbind(review_items, item)
    }
  } else { # i.e., when there is  only one more trial left
    if(answer == "1") { # i.e., if correct
      psychTestR::set_local("more_review_items", FALSE,  state)
    } else {
      item$time_next_due <- Sys.time() + lubridate::minutes(1)
      review_items <- item
    }

  }
  psychTestR::set_local("review_items", review_items,  state)
}

grab_review <- function(review_items, state) {
  due_now <- review_items %>% dplyr::filter(
    time_next_due < Sys.time()
  ) %>% dplyr::slice(1)

  if(length(due_now) > 0) {
    return(due_now)
  } else {
    if(nrow(psychTestR::get_local("item_bank", state)) == 0) {
      item <- review_items %>% dplyr::slice(1) # if item bank empty and nothing technically due for review, try and clear out the reviews
      return(item)
    } else {
      return("Finished!")
    }
  }
}


review_buffer_timebound <- function(minimum_complete = 5) {
  psychTestR::make_test(
    elts = psychTestR::join(

      psychTestR::code_block(function(state, ...) {
        psychTestR::set_local("more_review_items", TRUE, state)
        psychTestR::set_local("review_items", tibble::tibble(a = numeric()), state)
        psychTestR::set_local("complete_items", tibble::tibble(a = numeric()), state)
        psychTestR::set_local("item_bank", WJD_mini, state)
      }),

      psychTestR::one_button_page("Welcome!"),

      psychTestR::while_loop(
        test = function(state, ...) {
          complete_items <- psychTestR::get_local("complete_items", state)
          psychTestR::get_local("more_review_items",  state)  | minimum_complete > nrow(complete_items)
        },
        logic = psychTestR::join(
          psychTestR::code_block(function(state, ...) {

            review_items <- psychTestR::get_local("review_items", state)
            item_bank <- psychTestR::get_local("item_bank", state)
            complete_items <- psychTestR::get_local("complete_items", state)

            if(nrow(review_items) == 0) { # then there is nothing to review
              item <- sample_from_item_bank_and_remove(state)
            } else {
              item <- grab_review(review_items, state)
              if(nrow(item) == 0 & item_bank != FALSE) {
                item <- sample_from_item_bank_and_remove(state)
              } else {
                print('ping la')
                item <- grab_review(review_items, state)
              }
            }

            psychTestR::set_local("item", item, state)

          }),
          psychTestR::reactive_page(function(state, ...) {

            review_items <- psychTestR::get_local("review_items", state)
            item_bank <- psychTestR::get_local("item_bank", state)

            if(item_bank == FALSE) {
              item_bank <- list(orig_abs_melody = " ")
            }
            complete_items <- psychTestR::get_local("complete_items", state)
            item <- psychTestR::get_local("item", state)


            present_stimuli(stimuli = itembankr::str_mel_to_vector(item$orig_abs_melody),
                            stimuli_type = "midi_notes",
                            display_modality = "visual",
                            page_type = "NAFC_page",
                            label = "item",
                            choices = as.character(0:1),
                            on_complete = timebound_review,
                            page_text_first = FALSE,
                            page_text = shiny::tags$div(

                              shiny::tags$h2("Remaining in item bank"),

                              list_to_shiny_table(
                                as.list(
                                  if_null_return_empty_text(item_bank$orig_abs_melody)
                                ), rownames = FALSE),

                              shiny::tags$h2("Review items"),

                              list_to_shiny_table(
                                as.list(
                                  if_null_return_empty_text(review_items$orig_abs_melody)
                                ), rownames = FALSE),

                              shiny::tags$h2("Complete items"),

                              list_to_shiny_table(
                                as.list(
                                  if_null_return_empty_text(complete_items$orig_abs_melody)
                                  ), rownames = FALSE)
                              )

      )
  })
  )),
      psychTestR::final_page("This is the end!")
  ),
  opt = psychTestR::test_options(title = "Review Buffer Test",
                                 admin_password = "test",
                                 display = psychTestR::display_options(
                                   left_margin = 1L,
                                   right_margin = 1L,
                                   css = system.file('www/css/style.css', package = "musicassessr")
                                 ),
                                 additional_scripts = musicassessr::musicassessr_js(state = "test", visual_notation = TRUE),
                                 languages = c("en")))
}

if_null_return_empty_text <- function(x) {
  if(is.null(x)) {
    return(" ")
  } else {
    return(x)
  }
}

#review_buffer_timebound()
