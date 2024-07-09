# These functions are officially deprecated but we leave in for backwards compatibility.

#' get_user_info_page
#'
#' Check a user has the requirements to run our test and record their browser information.
#'
#' This function is deprecated and will be removed in a future version.
#'
#' @param chrome_only Logical. If `TRUE`, only Chrome browser is recommended. Default is `TRUE`.
#'
#' @return None.
#' @name get_user_info_page
#' @export
#'
#' @deprecated This function is deprecated and will be removed in a future version. Use `psychTestR::test_options` instead.
get_user_info_page <- function(chrome_only = TRUE) {

  if (chrome_only) {
    page_text <- shiny::tags$p(psychTestR::i18n("browser_recommendation"))
  } else {
    page_text <- shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Browser_Requirements")),
                                 shiny::tags$p(psychTestR::i18n("browser_requirements")),
                                 shiny::tags$ul(
                                   shiny::tags$li(psychTestR::i18n("chrome")),
                                   shiny::tags$li(psychTestR::i18n("edge")),
                                   shiny::tags$li(psychTestR::i18n("firefox")),
                                   shiny::tags$li(psychTestR::i18n("opera"))
                                 ))
  }

  ui <- shiny::tags$div(
    page_text,
    shiny::tags$p(psychTestR::i18n("browser_requirements2")),
    shiny::tags$div(shiny::tags$input(id = "user_info"), class="_hidden"),
    shiny::tags$button(psychTestR::i18n("Next"), id = "getUserInfoButton", onclick = "getUserInfo();testFeatureCapability();next_page();", class = "btn btn-default action-button")
  )

  psychTestR::page(ui = ui, label = "user_info", save_answer = TRUE, get_answer = user_info_check)
}

#' user_info_check
#'
#' Check the browser information and save it including participant ID.
#'
#' @param input Input from the user.
#' @param state Current state of the user.
#' @param ... Additional arguments.
#'
#' @name user_info_check
#' @return A list containing the user information if the browser is capable, otherwise an error is displayed.
user_info_check <- function(input, state, ...) {
  # Check the info and save it including participant ID
  if (input$browser_capable == FALSE) {
    psychTestR::display_error(psychTestR::i18n("incorrect_browser"))
  } else {
    list("user_info" = rjson::fromJSON(input$user_info))
  }
}
