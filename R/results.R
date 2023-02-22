
#' Plot a normal distribution with a certain score highlighted
#'
#' @param data
#' @param mean
#' @param sd
#' @param highlighted_score
#' @param title
#' @param color
#' @param plot_point
#' @param alpha
#' @param vertical_line_colour
#'
#' @return
#' @export
#'
#' @examples
plot_normal_dist_plus_score <- function(data = NULL,
                                        mean = NULL,
                                        sd = NULL,
                                        highlighted_score,
                                        title = ggplot2::waiver(),
                                        colour = "purple",
                                        plot_point = FALSE,
                                        alpha = 0.1,
                                        vertical_line_colour = "orange") {

  if(!is.null(data) & is.null(mean) & is.null(sd)) {
    sd <- sd(data$score)
    mean <- mean(data$score)
  } else if(is.null(data) & !is.null(mean) & !is.null(sd)) {
    data <- tibble::tibble(score = highlighted_score)
  } else {
    stop("Unsupported combination.")
  }

  highlighted_score_y <- pnorm(highlighted_score + 1, mean = mean, sd = sd) - pnorm(highlighted_score, mean = mean, sd = sd)

  if(highlighted_score < mean) {
    hjust <- "left"
  } else {
    hjust <- "right"
  }

  if(highlighted_score > 75 | highlighted_score < 25) {
    vjust <- -1
  } else {
    vjust <- 0
  }

  ggplot2::ggplot(data=data, ggplot2::aes(x = score)) +
    { if(plot_point) ggplot2::geom_point(ggplot2::aes(y = pnorm(score + 1, mean = mean(score, na.rm = TRUE), sd = sd(score, na.rm = TRUE)) - pnorm(score, mean = mean(score, na.rm = TRUE), sd = sd(score, na.rm = TRUE))), alpha = 0.6, size = 0.2, position = ggplot2::position_jitter(w = 0.0010, h = 0.0010)) } +
    ggplot2::stat_function(fun = dnorm, args = c(mean = mean, sd = sd), geom = "polygon", color = colour, fill = colour, alpha = alpha) +
    ggplot2::geom_point(ggplot2::aes(x=highlighted_score, y = highlighted_score_y), colour="purple") +
    ggplot2::geom_vline(xintercept = mean, color = vertical_line_colour) +
    ggplot2::xlim(0, 100) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank()) +
    ggplot2::annotate(geom = "text", x = highlighted_score, y = highlighted_score_y, label = "Your Score!", hjust = hjust, vjust = vjust) +
    ggplot2::labs(
      title = title,
      x = "Population Score"
    )

}

# plot_normal_dist_plus_score(mean = 50, sd = 10, highlighted_score = 100)
# plot_normal_dist_plus_score(data = tibble::tibble(score = 1:100), highlighted_score = 50, plot_point = TRUE)

#' Render scores in a shiny table
#'
#' @param res
#'
#' @return
#' @export
#'
#' @examples
render_scores_table <- function(res) {
  shiny::renderTable({
    df <- res
    df_names <- names(df)
    df <- base::t(df)
    row.names(df) <- df_names
    df
  }, rownames = TRUE, colnames = FALSE, width = "50%")
}

# leaderboard stuff

load_leaderboard <- function(leaderboard_name = 'leaderboard.rda') {
  if(file.exists(paste0('output/', leaderboard_name))) {
    load(paste0('output/', leaderboard_name))
  } else {
    # or create if doesn't exist
    leaderboard <- tibble::tibble(
      username = character(0),
      score = numeric(0)
    )
    save(leaderboard, file = paste0('output/', leaderboard_name))
  }
  leaderboard
}

add_score_to_leaderboard <- function(username, score, leaderboard_name) {

  leaderboard <- load_leaderboard()

  new_score <- tibble::tibble(
    username = username,
    score = score
  )


  leaderboard <- rbind(leaderboard, new_score) %>% dplyr::arrange(desc(score))

  leaderboard <- leaderboard[!is.na(leaderboard$score), ]

  save(leaderboard, file = paste0('output/', leaderboard_name))

  # and present new leaderboard
  leaderboard
}







#' A page for sharing scores.
#'
#' @param test_name
#' @param url
#' @param hashtag
#' @param socials
#' @param leaderboard_name
#' @param distribution_mean
#' @param distribution_sd
#' @param data_not_from_leaderboard
#'
#' @return
#' @export
#'
#' @examples
share_score_page <- function(test_name,
                             url,
                             hashtag = "CanISing",
                             socials = TRUE,
                             leaderboard_name = 'leaderboard.rda',
                             distribution_mean = NULL,
                             distribution_sd = NULL,
                             data_not_from_leaderboard = NULL) {

  hashtag <- paste0("%23", hashtag)

  psychTestR::reactive_page(function(state, answer, ...) {

    score <- psychTestR::get_local("final_score", state)

    username <- answer

    updated_leaderboard <- add_score_to_leaderboard(username, score, leaderboard_name = leaderboard_name)

    leaderboard_table <- shiny::renderTable({
      head(updated_leaderboard, 10)
    }, rownames = TRUE, colnames = FALSE, width = "50%")


    data_to_use <- if(!is.null(data_not_from_leaderboard)) data_not_from_leaderboard else updated_leaderboard

    dist_plot <- shiny::renderPlot({
      plot_normal_dist_plus_score(data_to_use, score, mean = distribution_mean, sd = distribution_sd)
      }, width = 500)


    socials_html <- create_socials(socials, test_name, score, hashtag, test, url)


    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$h1("Leaderboard"),
      shiny::tags$div(leaderboard_table),
      shiny::tags$div(dist_plot),
      socials_html
    ))

  })

}


create_share_button <- function(img, url) {
  shiny::tags$td(
    shiny::tags$a(img(src = paste0("musicassessr-assets/img/", img)),
                  href = url,
                  target="_blank",
                  rel="nofollow noopener")
  )
}


#' Create an interface to share test score on socials
#'
#' @param socials
#' @param test_name
#' @param score
#' @param hashtag
#' @param test
#' @param url
#'
#' @return
#' @export
#'
#' @examples
create_socials <- function(socials, test_name, score, hashtag, test, url) {
  if(socials) {

    text <- paste0("I just completed the " , test_name, " and got a score of ", score, ". See what you got now!", hashtag, collapse = "_")
    text2 <- paste0("I just completed the " , test_name, " and got a score of ", score, ". See what you got now! ", hashtag, collapse = "%20")

    socials <- tibble::tibble(img = c("telegram.png", "facebook.png", "mail.png", "reddit.png", "whatsapp.png", "twitter.png"),
                              url = c(
                                paste0("https://telegram.me/share/url?url=", url, "&text=", text2),
                                paste0('https://www.facebook.com/sharer.php?u=', url, '&quote=', text),
                                paste0("mailto:%7Bemail_address%7D?subject=Check%20out%20my%20", test_name, " score!&body=", text2),
                                paste0("https://reddit.com/submit?url=", url, "&title=", text),
                                paste0('whatsapp://send?text=', text, ' ', url),
                                paste0('https://twitter.com/intent/tweet?url=', url, '&text=', text2)))

    socials_html <- shiny::tags$div(
      shiny::tags$h3(" Please share your score!"),
      shiny::tags$br(),
      shiny::tags$table(style = " border-spacing: 10px; border-collapse: separate;",
                        shiny::tags$tr(
                          purrr::pmap(socials, function(img, url){
                            create_share_button(img, url)
                          })

                        ),
                        shiny::tags$br()))
  } else {
    socials_html <- shiny::tags$div()
  }
}


collapse_results <- function(res) {

  results <- lapply(res, function(x) {
    lapply(x, function(y) {
      if(is.list(y)) {
        lapply(y, as.character)
      } else {
        if(length(y) > 1) {
          paste0(y, collapse = ",")
        } else {
          as.character(y)
        }
      }
    })
  })

  results <- lapply(results, unlist, recursive = FALSE)

  #dplyr::bind_rows(results)

}

#' Tidy melodies from psychTestR results object
#'
#' @param melody_results The melody results from psychTestR.
#' @param use_for_production Which get_answer output to use for production (the other will be removed).
#'
#' @return
#' @export
#'
#' @examples
tidy_melodies <- function(melody_results, use_for_production = c("production", "pyin_pitch_track")) {

  melody_results <- lapply(melody_results, function(x) {

    x$additional_scoring_measures <- NULL
    warning('Currently, tidy_melodies removes any additional_scoring_measures.')


    if(use_for_production == "production") {
      x$pyin_pitch_track <- NULL
    } else if(use_for_production == "pyin_pitch_track") {
      x$production <- NULL
    } else {
      stop('use_for_production must be "production" or "pyin_pitch_trach"')
    }

    lapply(x, function(y) {
      if(is.list(y)) {
        lapply(y, as.character)
      } else {
        if(length(y) > 1) {
          paste0(y, collapse = ",")
        } else {
          as.character(y)
        }
      }
    })
  })

  melody_results <- lapply(melody_results, unlist, recursive = FALSE)

  dplyr::bind_rows(melody_results)

}




