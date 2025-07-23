
#' Appointment finder app
#'
#' @returns
#' @export
#'
#' @examples
run_appt_finder_app <- function() {
  # Load required packages
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Terminplaner"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::dateInput("start_date", "Startdatum auswählen:", value = base::Sys.Date())
        ),

        shiny::mainPanel(
          shiny::tableOutput("appointment_table")
        )
      )
    ),

    server = function(input, output) {

      # Appointment generating function
      generate_appointments <- function(start_date) {

        start_date <- lubridate::as_date(start_date)
        day_after <- start_date + 1

        appointments <- list(
          "Ursprüngliches Datum" = start_date,
          "Tag danach" = day_after,
          "1 Woche später" = day_after + 7,
          "2 Wochen später" = day_after + 14,
          "3 Wochen später" = day_after + 21,
          "4 Wochen später" = day_after + 28
        )

        # Return as a data.frame
        tibble::tibble(
          Termin = names(appointments),
          Datum = purrr::map_chr(appointments, as.character)
        )
      }

      output$appointment_table <- shiny::renderTable({
        generate_appointments(input$start_date)
      })
    }
  )
}

#' Get longitudinal study items
#'
#' @param participant_number
#' @param num_test_session
#'
#' @returns
#' @export
#'
#' @examples
get_longitudinal_session_items <- function(participant_number, num_test_session) {

  p_items <- pbet_hmtm_longitudunal_study_matrix %>%
    dplyr::filter(participant_no == !! participant_number)

  if(num_test_session %in% c(1L, 6L)) {
    return(p_items)
  } else if(num_test_session == 2L) {

    p_items <- p_items %>% dplyr::filter(test_after_one_day)

    return(p_items)

  } else if(num_test_session == 3L) {

    p_items <- p_items %>% dplyr::filter(test_after_one_week)

    return(p_items)

  } else if(num_test_session == 4L) {

    p_items <- p_items %>% dplyr::filter(test_after_two_weeks)

    return(p_items)

  } else if(num_test_session == 5L) {

    p_items <- p_items %>% dplyr::filter(test_after_three_weeks)

    return(p_items)

  } else if(num_test_session == 6L) {

    p_items <- p_items %>% dplyr::filter(test_after_four_weeks)

    return(p_items)

  } else {
    stop("Invalid test session")
  }
}


generate_appointments <- function(start_date) {
  # Convert to Date if it's a string
  start_date <- as.Date(start_date)

  # Day after the start date
  day_after <- start_date + 1

  # Create list of appointments
  appointments <- list(
    original_date = start_date,
    day_after = day_after,
    week_after = day_after + 7,
    two_weeks_after = day_after + 14,
    three_weeks_after = day_after + 21,
    four_weeks_after = day_after + 28
  )

  return(appointments)
}


# t <- get_longitudinal_session_items(1L, 1L)

# t <- generate_appointments("2025-05-25") # Seb
#
