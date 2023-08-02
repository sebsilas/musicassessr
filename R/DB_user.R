




ma <- function() {

  shinyusermanagement::run_app(title = "musicassessr Login",
                               url = "https://adaptiveeartraining.com/music-assessment",
                               pre_login_content = prelogin,
                               logged_in_ui = logged_in_ui,
                               logged_in_message = "Welcome to the musicassessr test launch control panel.")
}




user_performance_plots <- function(username) {

  trials <- get_table("trials") %>% dplyr::filter(test_username == !! username)
  sessions <- get_table("sessions") %>% dplyr::filter(test_username == !! username)


  joint <- trials %>%
    dplyr::left_join(sessions, by = c("session_id", "test", "test_username")) %>%
    dplyr::mutate(session_no = as.numeric(factor(session_id))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(session_id, test) %>%
    dplyr::mutate(trial_no = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(session_by_attempt = (10 * session_no) + attempt)


  melodies_which_appear_more_than_once <- joint %>%
    dplyr::count(abs_melody, session_no) %>%
    dplyr::count(abs_melody) %>%
    dplyr::filter(n > 1)

  print(melodies_which_appear_more_than_once)


  ability_across_sessions <- sessions %>%
      dplyr::rename(Test = test) %>%
      ggplot2::ggplot(ggplot2::aes(x = time_completed, y = ability_estimate, group = Test, color = Test)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      #geom_smooth(method = 'lm') +
      ggplot2::labs(
        x = 'Time Completed',
        y = 'Ability Estimate'
      )

  mean_opti3_across_sessions <- sessions %>%
      dplyr::rename(Test = test) %>%
      ggplot2::ggplot(ggplot2::aes(x = time_completed, y = mean_opti3, group = Test, color = Test)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      #geom_smooth(method = 'lm') +
      ggplot2::labs(
        x = 'Time Completed',
        y = 'Mean opti3'
      )

  melody_by_session_average <- joint %>%
      dplyr::rename(Test = test) %>%
      dplyr::group_by(abs_melody, Test, session_id) %>%
      dplyr::summarise(opti3 = mean(opti3, na.rm = TRUE),
                       time_completed = time_completed) %>%
      dplyr::filter(abs_melody %in% melodies_which_appear_more_than_once$abs_melody) %>%
          ggplot2::ggplot(ggplot2::aes(x = time_completed, y = opti3, group = abs_melody, color = abs_melody)) +
          ggplot2::geom_point() +
          ggplot2::geom_line(alpha = 0.6) +
          ggplot2::facet_wrap(~abs_melody, scales = "free") +
          ggplot2::labs(
            x = 'Time Completed',
            y = 'opti3'
          ) + ggplot2::theme(legend.position = "none")

  # by attempt and sessions

  melody_by_attempt_and_sessions <- joint %>%
      dplyr::rename(Test = test) %>%
        dplyr::filter(abs_melody %in% melodies_which_appear_more_than_once$abs_melody) %>%
          ggplot2::ggplot(ggplot2::aes(x = session_by_attempt, y = opti3, group = abs_melody, color = as.factor(session_no))) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          #ggplot2::geom_smooth(method = "lm") +
          ggplot2::facet_wrap(~abs_melody, scales = "free") +
          ggplot2::labs(
            x = 'Time Completed',
            y = 'opti3'
          ) + ggplot2::theme(legend.position = "none") +
          ggplot2::ylim(0:1)

    # by attempt and sessions

  average_melody_opti3_across_sessions_for_repeated_melodies <- joint %>%
    dplyr::rename(Test = test) %>%
    dplyr::filter(abs_melody %in% melodies_which_appear_more_than_once$abs_melody) %>%
    dplyr::group_by(time_started) %>%
    dplyr::summarise(
      mean_opti3 = mean(opti3, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    unique() %>%
      ggplot2::ggplot(ggplot2::aes(x = time_started, y = mean_opti3)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(group = 1) +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::labs(
        x = 'Time Completed',
        y = 'opti3'
      ) + ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0:1)


  list(
    shiny::renderPlot({ ability_across_sessions }),
    shiny::renderPlot({ mean_opti3_across_sessions }),
    shiny::renderPlot({ melody_by_session_average }),
    shiny::renderPlot({ melody_by_attempt_and_sessions }),
    shiny::renderPlot({ average_melody_opti3_across_sessions_for_repeated_melodies })
  )

}



available_tests_ui <- function() {

  available_tests <- get_online_available_tests()

  test_links <- purrr::map(available_tests, function(test) {
    shiny::tags$div(
      shiny::tags$h3(test_acronym_to_name(test)),
      shiny::tags$button("Launch Test!",
                         class = "btn",
                         type = "button",
                         onclick= paste0("window.open(\'", "https://adaptiveeartraining.com/", test, "\','_blank')")),
      shiny::tags$hr(),
    )
  })

  shiny::tags$div(
    shiny::tags$h2("Available Tests"),
    shiny::tags$hr(),
    test_links
  )
}

get_online_available_tests <- function() {
  available_tests <- get_table("musicassessr_tests") %>%
    dplyr::filter(globally_available) %>%
    dplyr::pull(test)
}


prelogin <- function() {
  shiny::tagList(
    shiny::tags$p("Welcome to the musicassessr test launch centre."),
    shiny::tags$p("Please login to do some music tests!")
  )
}



logged_in_ui <- function(account_settings, username) {

  shiny::tabsetPanel(
    shiny::tabPanel("Tests", available_tests_ui()),
    shiny::tabPanel("Your Progress", progress_ui(username)),
    account_settings
  )

}

progress_ui <- function(username) {

  d <- user_performance_plots(username)

  tags$div(
    shiny::tags$p(d[[1]]),
    shiny::tags$p(d[[2]]),
    shiny::tags$p(d[[3]]),
    shiny::tags$p(d[[4]]),
    shiny::tags$p(d[[5]])
  )

}


authenticate_session_token <- function(username, proposed_token) {

  session_token_table <- musicassessr::get_table("session_tokens")


  if(is.null(username) | is.null(proposed_token)){
    return(FALSE)
  }

  session_token <-  session_token_table %>%
    dplyr::filter(username == !! username) %>%
    dplyr::pull(session_token)

  session_token == proposed_token
}



#' Validate user entry to test based on session token look up in PostgreSQL DB
#'
#' @param validate_user_entry_into_test
#' @param elts
#'
#' @return
#' @export
#'
#' @examples
validate_user_entry_into_test <- function(validate_user_entry_into_test, elts) {

  if(validate_user_entry_into_test) {

    psychTestR::join(

      psychTestR::reactive_page(function(state, ...) {

        url_params <- psychTestR::get_url_params(state)

        username <- url_params$username
        proposed_token <- url_params$session_token

        psychTestR::set_global("token_authenticated",
                               authenticate_session_token(username, proposed_token),
                               state)

        psychTestR::one_button_page(paste0("Welcome ", username, ". If you do not see your username, you have not been validated."))

      }),

      psychTestR::conditional(test = function(state, ...) {
        # invert the logic
        !psychTestR::get_global("token_authenticated", state)
      }, logic = psychTestR::final_page("Unfortunately you were not validated.")),

      elts
    )
  } else {
    elts
  }

}
