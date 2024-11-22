
#' Should the end of the test present a final page or not?
#'
#' @param final
#' @param task_name
#' @param img
#' @param present_continue_to_new_test_page
#' @param redirect_url
#'
#' @return
#' @export
#'
#' @examples
final_page_or_continue_to_new_test <- function(final = TRUE, task_name, img = NULL, present_continue_to_new_test_page = TRUE, redirect_url = NULL) {

  if(final) {
    txt <- paste0(psychTestR::i18n("test_complete_1"), " ", task_name, " ", psychTestR::i18n("test_complete_2"))
    img <- if(is.null(img)) shiny::tags$div() else shiny::tags$img(src = img, height = 300, width = 300)

    if(is.null(img)) {
      ui <- shiny::tags$div(shiny::tags$p(txt))
    } else {
      ui <- shiny::tags$div(img, shiny::tags$br(), shiny::tags$p(txt))
    }


    if(is.scalar.character(redirect_url)) {
      ui <- shiny::tags$div(ui, shiny::tags$p(psychTestR::i18n("redirect_message")))
      return(redirect_page(text = ui, ms = 3000, url = redirect_url, final = TRUE))
    } else {
      return(psychTestR::final_page(ui))
    }


  } else {
    if(present_continue_to_new_test_page) {
      return(psychTestR::one_button_page(psychTestR::i18n("proceed_next_test")))
    } else {
      return(empty_code_block())
    }

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


#' Wrap a timeline with musicassessr dict
#'
#' @param tl
#'
#' @return
#' @export
#'
#' @examples
wrap_musicassessr_timeline <- function(tl) {
  psychTestR::new_timeline(tl,
                           dict = musicassessr::musicassessr_dict)
}

heading_page <- function(heading = "This is your heading!",
                         after = NULL) {

  ui <- shiny::tags$div(
    shiny::tags$h1(heading),
    if(is.null(after)) shiny::tags$p("") else after
    )

  psychTestR::one_button_page(ui)
}


progress_bar <- function(progress) {
  shiny::HTML(
    paste0('
    <div class="progress">
      <div class="progress-bar progress-bar-striped active" role="progressbar"
      aria-valuenow="', progress, '" aria-valuemin="0" aria-valuemax="100" style="width:', progress, '%">
        ', progress, '%
      </div>
        </div>
    ')
  )
}

musicassessr_css <- function() {
  shiny::tags$link(rel="stylesheet", type="text/css", href="https://musicassessr.com/assets/css/style_songbird.css")
}


#' Conditional logic if posed a yes or no question
#'
#' @param question_page Should be an NAFC_page with Yes and No only options
#' @param logic_if_use Logic to execute if the user clicks Yes.
#'
#' @return
#' @export
#'
#' @examples
conditional_proceed_if_yes <- function(question_page, logic_if_yes) {
  psychTestR::join(

    question_page,

    psychTestR::conditional(function(state, ...) {
      answer <- psychTestR::answer(state)
      lang <- psychTestR::get_session_info(state, complete = FALSE)$language

      if(lang != "en") {
        answer <- translate_from_dict(answer, lang)
      }
      answer == "Yes"
    }, logic_if_yes )
  )
}


sure_you_want_to_continue_button <- function(extra_js_to_execute_on_click = NULL,
                                             confirmation_msg = psychTestR::i18n("sure_continue")) {
  shiny::tags$button(
    id = "next",
    type = "button",
    class = "btn btn-default action-button",
    psychTestR::i18n("Next"),
    onclick = paste0("if(confirm('", confirmation_msg, "')) {
        trigger_button(this.id);", extra_js_to_execute_on_click, "
        } else {
          return false;
        }")
  )
}

