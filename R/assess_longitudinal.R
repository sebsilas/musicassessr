

plot_types <- c("ability_across_sessions",
                "mean_opti3_across_sessions",
                "melody_by_session_average",
                "melody_by_attempt_and_sessions",
                "average_melody_opti3_across_sessions_for_repeated_melodies")


#' Plot longitudinal
#'
#' @param type
#'
#' @return
#' @export
#'
#' @examples
plot_longitudinal <- function(type = c("ability_across_sessions",
                                       "mean_opti3_across_sessions",
                                       "melody_by_session_average",
                                       "melody_by_attempt_and_sessions",
                                       "average_melody_opti3_across_sessions_for_repeated_melodies")) {

  print(paste0('plot type: ', type))

  trials <- get_table("trials")
  sessions <- get_table("sessions")
  production <- get_table("production")


  joint <- trials %>%
    dplyr::left_join(sessions, by = c("test_username", "session_id", "test")) %>%
    dplyr::group_by(test_username) %>%
    dplyr::mutate(session_no = as.numeric(factor(session_id))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(test_username, session_id, test) %>%
    dplyr::mutate(trial_no = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(session_by_attempt = (10*session_no) + attempt)


  melodies_which_appear_more_than_once <- joint %>%
    dplyr::count(abs_melody, session_no, test_username) %>%
    dplyr::count(abs_melody, test_username) %>%
    dplyr::filter(n > 1)

  seb_mels <- melodies_which_appear_more_than_once %>%
    dplyr::filter(test_username == "Seb") %>%
    dplyr::pull(abs_melody)

  sylvia_mels <- melodies_which_appear_more_than_once %>%
    dplyr::filter(test_username == "Sylvia") %>%
    dplyr::pull(abs_melody)


  if(type == "ability_across_sessions") {

    p <- sessions %>%
      dplyr::rename(Test = test) %>%
        ggplot2::ggplot(ggplot2::aes(x = time_completed, y = ability_estimate, group = Test, color = Test)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        #geom_smooth(method = 'lm') +
        ggplot2::facet_wrap(~test_username) +
        ggplot2::labs(
          x = 'Time Completed',
          y = 'Ability Estimate'
        )

  }

  else if(type == "mean_opti3_across_sessions") {

    p <- sessions %>%
      dplyr::rename(Test = test) %>%
      ggplot2::ggplot(ggplot2::aes(x = time_completed, y = mean_opti3, group = Test, color = Test)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      #geom_smooth(method = 'lm') +
      ggplot2::facet_wrap(~test_username) +
      ggplot2::labs(
        x = 'Time Completed',
        y = 'Mean opti3'
      )

  }

  else if(type == "melody_by_session_average") {

    # average

    p <- joint %>%
      dplyr::rename(Test = test) %>%
      dplyr::group_by(abs_melody, test_username, Test, session_id) %>%
      dplyr::summarise(opti3 = mean(opti3, na.rm = TRUE),
                       time_completed = time_completed) %>%
      dplyr::filter(abs_melody %in% seb_mels & test_username == "Seb" |
                      abs_melody %in% sylvia_mels & test_username == "Sylvia") %>%
      ggplot2::ggplot(ggplot2::aes(x = time_completed, y = opti3, group = abs_melody, color = abs_melody)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(alpha = 0.6) +
      ggplot2::facet_wrap(ggplot2::vars(test_username, abs_melody), scales = "free") +
      ggplot2::labs(
        x = 'Time Completed',
        y = 'opti3'
      ) + ggplot2::theme(legend.position = "none")

  }

  else if(type == "melody_by_attempt_and_sessions") {

    # by attempt and sessions

    p <- joint %>%
      dplyr::rename(Test = test) %>%
      dplyr::filter(abs_melody %in% seb_mels & test_username == "Seb" |
                      abs_melody %in% sylvia_mels & test_username == "Sylvia") %>%
        ggplot2::ggplot(ggplot2::aes(x = session_by_attempt, y = opti3, group = abs_melody, color = as.factor(session_no))) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        #ggplot2::geom_smooth(method = "lm") +
        ggplot2::facet_wrap(dplyr::vars(test_username, abs_melody), scales = "free") +
        ggplot2::labs(
          x = 'Time Completed',
          y = 'opti3'
        ) + ggplot2::theme(legend.position = "none") +
      ggplot2::ylim(0:1)
  }

  else if(type == "average_melody_opti3_across_sessions_for_repeated_melodies") {

    # by attempt and sessions

    p <- joint %>%
      dplyr::rename(Test = test) %>%
      dplyr::filter(abs_melody %in% seb_mels & test_username == "Seb" |
                      abs_melody %in% sylvia_mels & test_username == "Sylvia") %>%
      dplyr::group_by(test_username, time_started) %>%
      dplyr::summarise(
        mean_opti3 = mean(opti3, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      unique() %>%
        ggplot2::ggplot(ggplot2::aes(x = time_started, y = mean_opti3)) +
        ggplot2::geom_point(ggplot2::aes(color = test_username, group = test_username)) +
        ggplot2::geom_line(ggplot2::aes(color = test_username, group = test_username), group = 1) +
        ggplot2::geom_smooth(method = "lm") +
        ggplot2::facet_wrap(dplyr::vars(test_username)) +
        ggplot2::labs(
          x = 'Time Completed',
          y = 'opti3'
        ) + ggplot2::theme(legend.position = "none") +
            ggplot2::ylim(0:1)
  }

  else {
    stop("Plot type not recognised")
  }
  print(p)

}


flick_through_plots <- function(n = 1) {
  while(n != length(plot_types)+1) {
    plot_longitudinal(plot_types[n])
    askYesNo("Move on?")
    n <- n + 1
  }
}











# trials <- get_table("trials")
# sessions <- get_table("sessions")
# production <- get_table("production")

# CAREFUL. This is for editing the DBs manually!
# trials <- get_table("trials")
# sessions <- get_table("sessions")
# production <- get_table("production")
#
#
# trials2 <- trials
# sessions2 <- sessions
# production2 <- production
#
# correct_session_id <- "49e83bdf0a8d8588b95e35f0d58108fd3130858d945dc07eb5df1eb022c6f29f"
# wrong_session_id <- "2e40096627bb1d4e14513e1129b599909b09e7e61a1857b2d830a93ef957b178"
#
# correct_time_started <- sessions[53, "time_started"]
#
# # sessions
# sessions2$session_id[sessions2$session_id == wrong_session_id] <- correct_session_id
# sessions2[54, "time_started"] <- correct_time_started
#
# # trials
# trials2$session_id[trials2$session_id == wrong_session_id] <- correct_session_id
#
# # production
# production2$session_id[production2$session_id == wrong_session_id] <- correct_session_id
# # save out
#
# host <- "localhost"
# username <- "postgres"
# password <- "ilikecheesepie432"
#
# con <- connect_to_db(host, username, password)
#
# DBI::dbWriteTable(con, "sessions", sessions2, overwrite = TRUE)
# DBI::dbWriteTable(con, "trials", trials2, overwrite = TRUE)
# DBI::dbWriteTable(con, "production", production2, overwrite = TRUE)
#
# DBI::dbDisconnect(con)

