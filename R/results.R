
plot_normal_dist_plus_score <- function(data, highlighted_score) {
  std <- sd(data$score)
  m <- mean(data$score)
  highlighted_score_y <- pnorm(highlighted_score + 1, mean = m, sd = std) - pnorm(highlighted_score, mean = m, sd = std)
  ggplot2::ggplot(data=data, ggplot2::aes(x = score)) +
    ggplot2::stat_function(fun = dnorm, args = c(mean = m, sd = std), alpha = .4) +
    ggplot2::geom_point(ggplot2::aes(x=highlighted_score, y = highlighted_score_y), colour="purple") +
    ggplot2::geom_vline(xintercept = m, color = "orange", alpha = .8) +
    ggplot2::xlim(0, m + std * 5) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
}


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
#'
#' @return
#' @export
#'
#' @examples
share_score_page <- function(test_name, url, hashtag = "CanISing", socials = TRUE, leaderboard_name = 'leaderboard.rda') {

  hashtag <- paste0("%23", hashtag)

  psychTestR::reactive_page(function(state, answer, ...) {

  score <- psychTestR::get_local("final_score", state)

  username <- answer

  updated_leaderboard <- add_score_to_leaderboard(username, score, leaderboard_name = leaderboard_name)

  leaderboard_table <- shiny::renderTable({
    head(updated_leaderboard, 10)
  }, rownames = TRUE, colnames = FALSE, width = "50%")

  dist_plot <- shiny::renderPlot({ plot_normal_dist_plus_score(updated_leaderboard, score)  }, width = 500)

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
      shiny::tags$h1(" Please share your score!"),
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
#' @param melody_results
#'
#' @return
#' @export
#'
#' @examples
tidy_melodies <- function(melody_results) {

  melody_results <- lapply(melody_results, function(x) {
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



# # dummy data for testing
# t_res <- readRDS('/Users/sebsilas/Downloads/results.rds')
#
#
# processed_results$long_tone
# processed_results$arrhythmic
# processed_results$rhythmic

# l <- list.files('/Users/sebsilas/Downloads/results 3/', full.names = TRUE)
#
# r <- lapply(l, function(x) as.list(readRDS(x)))
#
#
# r2 <- lapply(r, collapse_results)
#
# test <- data.frame(r2[[1]])
# test2 <- as.data.frame(r2[[2]])



