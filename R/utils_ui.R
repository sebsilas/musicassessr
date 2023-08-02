
#' Should the end of the test present a final page or not?
#'
#' @param final
#' @param task_name
#' @param img
#'
#' @return
#' @export
#'
#' @examples
final_page_or_continue_to_new_test <- function(final = TRUE, task_name, img = NULL) {
  if(final) {
    txt <- paste0(psychTestR::i18n("test_complete_1"), " ", task_name, psychTestR::i18n("test_complete_2"))
    img <- if(is.null(img)) shiny::tags$div() else shiny::tags$img(src = img, height = 300, width = 300)
    ui <- shiny::tags$div(img, shiny::tags$br(), txt)
    psychTestR::final_page(ui)
  } else {
    psychTestR::one_button_page(psychTestR::i18n("proceed_next_test"))
  }
}



validate_page_types <- function(page_type_string, args) {

  # check if certain page types have their required arguments
  # and give a descriptive error message back to user if they haven't specified something correctly

  if(page_type_string %in% c("NAFC_page", "dropdown_page")) {

    if(is.null(args$label) | is.null(args$choices)) {
      stop('You must specify a label and choices for NAFC_page or dropdown_page')
    }
  }

  if(page_type_string == "slider_page") {

    if(is.null(args$label) | is.null(args$min)
       | is.null(args$max) | is.null(args$value)
    ) {
      stop('You must specify a label, min, max and value arguments for slider pages')
    }
  }

  if(page_type_string == "text_input_page") {
    if(is.null(args$label)) {
      stop('You must specify a label for text_input_page')
    }
  }
}


#' Deploy a filler task
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
filler_task <- function(type = c("none", "surveys")) {
  if (match.arg(type) == "none") {
    musicassessr::empty_code_block()
  } else if (match.arg(type) == "surveys") {
    psychTestR::join(
      psyquest::GMS(subscales = "Musical Training"),
      musicassessr::deploy_demographics(TRUE)
    )
  } else {
    stop("Filler task type not recognised")
  }
}


wrap_musicassessr_timeline <- function(tl) {
  psychTestR::new_timeline(tl, dict = musicassessr::dict(NULL))
}
