user_info_check <- function(input, state, ...)  {
  # check the info and save it including participant ID
  if (input$browser_capable == FALSE) {
    psychTestR::display_error(i18n("incorrect_browser"))
  } else {
    list("user_info" = rjson::fromJSON(input$user_info))
  }
}



#' Check a user has the requirements to run our test and record their browser information
#'
#' @param chrome_only
#'
#' @return
#' @export
#'
#' @examples
get_user_info_page<- function(chrome_only = TRUE) {

  if(chrome_only) {
    page_text <- shiny::tags$p("You should ideally be running this test in Google Chrome or Opera.")
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
    shiny::tags$button("Next", id="getUserInfoButton", onclick="getUserInfo();testFeatureCapability();next_page();", class="btn btn-default action-button")
  )

  psychTestR::page(ui = ui, label = "user_info", save_answer = TRUE, get_answer = user_info_check)

}
