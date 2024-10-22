

#' Songbird welcome
#'
#' @param success
#' @param username
#'
#' @return
#' @export
#'
#' @examples
songbird_welcome_page_fun <- function(success, username) {

  if(success) {
    page <- psychTestR::one_button_page(
      shiny::tags$div(
        tags$link(rel="stylesheet", type="text/css", href="https://musicassessr.com/assets/css/style_songbird.css"),
        shiny::tags$script("upload_to_s3 = true; console.log('Turning S3 mode on');"),
        tags$p(paste0("Welcome ", username, "!"))
      )
    )
  } else {
    page <- psychTestR::final_page(
      shiny::tags$div(
        tags$link(rel="stylesheet", type="text/css", href="https://musicassessr.com/assets/css/style_songbird.css"),
        tags$p(paste0("Sorry, you were not validated."))
      )
    )
  }

  return(page)
}
