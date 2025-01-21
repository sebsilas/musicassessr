


custom_questions <- function() {

  agree_disagree <-
    c(
      psychTestR::i18n("strongly_disagree"),
      psychTestR::i18n("disagree"),
      psychTestR::i18n("neither_agree_nor_disagree"),
      psychTestR::i18n("agree"),
      psychTestR::i18n("strongly_agree")
    )
  never_rarely <-
    c(
      psychTestR::i18n("never"),
      psychTestR::i18n("rarely"),
      psychTestR::i18n("sometimes"),
      psychTestR::i18n("very_often"),
      psychTestR::i18n("always")
    )


  psychTestR::join(

    psychTestR::checkbox_page(
      label = "musician_type",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 1/7")),
                               shiny::tags$p(psychTestR::i18n("i_am_a_musician"))),
      choices = c(
        psychTestR::i18n("professional_musician"),
        psychTestR::i18n("postgraduate_music_student"),
        psychTestR::i18n("undergraduate_music_student"),
        psychTestR::i18n("instrumental_student"),
        psychTestR::i18n("amateur_adult")
      ),
      force_answer = TRUE
    ),

    psychTestR::NAFC_page(
      label = "primary_genre",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 2/7")),
                               shiny::tags$p(psychTestR::i18n("primary_genre_prompt"))),
      choices = c(psychTestR::i18n("classical"), psychTestR::i18n("jazz"),
                  psychTestR::i18n("folk"), psychTestR::i18n("other"))
    ),


    psychTestR::NAFC_page(
      label = "memory_comfortable",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 3/7")),
                               shiny::tags$p(psychTestR::i18n("comfortable_memory_prompt"))),
      choices = agree_disagree
    ),

    psychTestR::NAFC_page(
      label = "memory_frequency",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 4/7")),
                               shiny::tags$p(psychTestR::i18n("practice_memory_prompt"))),
      choices = never_rarely
    ),

    psychTestR::NAFC_page(
      label = "perform_from_memory",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 5/7")),
                               shiny::tags$p(psychTestR::i18n("perform_memory_prompt"))),
      choices = never_rarely
    ),

    psychTestR::dropdown_page(
      label = "highest_grade",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 6/7")),
                               shiny::tags$p(psychTestR::i18n("highest_grade_prompt"))),
      choices = as.character(c(psychTestR::i18n("not_sure"), 1:8))
    ),

    psychTestR::NAFC_page(
      label = "perfect_pitch",
      prompt = shiny::tags$div(shiny::tags$h2(paste0(psychTestR::i18n("question"), " 7/7")),
                               shiny::tags$p(psychTestR::i18n("perfect_pitch_prompt"))),
      choices = c(psychTestR::i18n("yes"), psychTestR::i18n("no"), psychTestR::i18n("not_sure"))
    )

  )
}

#' Questionnaires
#'
#' @returns
#' @export
#'
#' @examples
questionnaires <- function() {
  psychTestR::join(

    psyquest::DEG(year_range = c(1930, 2024)),

    psyquest::GMS(subscales = c("Musical Training", "Singing Abilities",
                                "Absolute Pitch", "Start Age")),

    psychTestR::new_timeline(
      psychTestR::join(

        select_musical_instrument_page(set_range_based_on_selection = FALSE,
                                       include_other_in_dropdown = TRUE,
                                       with_voice = TRUE,
                                       prompt = psychTestR::i18n("main_instrument_question")),

        custom_questions(),

        psychTestR::reactive_page(function(state, ...) {

          p_id <- psychTestR::get_session_info(state, complete = TRUE)$p_id


          msg <- shiny::tags$div(
            shiny::tags$p(psychTestR::i18n("thank_you_message")),
            shiny::tags$p(psychTestR::i18n("send_code_message")),
            shiny::tags$p(shiny::tags$strong(p_id))
          )

          psychTestR::final_page(msg)

        })
      ), dict = musicassessr::musicassessr_dict)
  )
}
