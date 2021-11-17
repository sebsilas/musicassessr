
# create some dummy data::


fake_id <- function(x) paste0(sample(1:10, 10), collapse = "")

#
# sessions <- tibble::tibble(
#   test_username = "Seb",
#   test = "MST",
#   session_id = lapply(1:10, fake_id),
#   time_started = unlist(lapply(1:10, function(days) as.POSIXct(lubridate::now() - lubridate::days(days), format = "%Y-%m-d-H:%M:%OS"))),
#   time_completed = unlist(lapply(1:10, function(days) as.POSIXct(lubridate::now() - lubridate::days(days), format = "%Y-%m-d-H:%M:%OS"))),
#   session_length = sample(1:10, 10),
#   mean_opti3 = runif(10),
#   mean_accuracy = runif(10),
#   ability_estimate = runif(10)
# )
#
#
# # plot sessions over time
#
# ggplot2::ggplot(sessions, ggplot2::aes(x = time_started)) +
#   ggplot2::geom_point(ggplot2::aes(y = mean_opti3), color = "pink") +
#   ggplot2::geom_point(ggplot2::aes(y = mean_accuracy), color = "yellow") +
#   ggplot2::geom_point(ggplot2::aes(y = ability_estimate), color = "green") +
#   ggplot2::geom_smooth(ggplot2::aes(y = mean_opti3), color = "pink",  se = FALSE) +
#   ggplot2::geom_smooth(ggplot2::aes(y = mean_accuracy), color = "yellow",  se = FALSE) +
#   ggplot2::geom_smooth(ggplot2::aes(y = ability_estimate), color = "green",  se = FALSE)
#
#
#
#


# trials <- tibble::tibble(
#   # NB: take the times and dates from psychTestR
#   test_username = "Seb",
#   test = "MST",
#   trial_id = 1:100,
#   session_id = rep(paste0(sample(1:10, 10), collapse = ""), 100),
#   time_started = unlist(lapply(1:100, function(days) as.POSIXct(lubridate::now() - lubridate::days(days), format = "%Y-%m-d-H:%M:%OS"))),
#   time_completed = unlist(lapply(1:100, function(days) as.POSIXct(lubridate::now() - lubridate::days(days), format = "%Y-%m-d-H:%M:%OS"))),
#   session_length = sample(1:100, 100),
#   melody = rep(item_sampler(itembankr::WJD("phrases"), 10) %>% dplyr::pull(melody), 10),
#   opti3 = runif(100),
#   accuracy = runif(100),
#   ability_estimate = runif(100))


# # plot melody development over time

# all melodies

# ggplot2::ggplot(melodies) +
#   ggplot2::geom_line(ggplot2::aes(x = time_started, y = opti3, group = melody, color = melody)) +
#   ggplot2::geom_smooth(ggplot2::aes(x = time_started, y = opti3), method = "glm")



plot_melody_performance_over_time <- function(melody_to_plot, method = "glm") {

  melody_perf <- melodies %>% dplyr::filter(melody == melody_to_plot)

  ggplot2::ggplot(melody_perf, ggplot2::aes(x = time_started, y = opti3)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = method)
}

# plot_melody_performance_over_time("-3")
# plot_melody_performance_over_time("7,-7")
# plot_melody_performance_over_time("-5,-3,6,-3,2,2,-2,-2,-1,1")

