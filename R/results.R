
present_scores <- function(res, num_items_long_tone, num_items_arrhythmic, num_items_rhythmic) {

  if(num_items_long_tone > 0) {
    # long tones
    long_tones <- as.data.frame(lapply(res$MST.long_note_trials$long_tone_, paste0, collapse = ","))

    long_tone_summary <- long_tones %>%
      dplyr::select(note_accuracy, note_precision, dtw_distance) %>%
        dplyr::mutate_if(is.character,as.numeric) %>%
          dplyr::summarise(mean_note_accuracy = mean(note_accuracy, na.rm = TRUE),
                           note_precision = mean(note_precision, na.rm = TRUE),
                           mean_dtw_distance = mean(note_precision, na.rm = TRUE))
  }

  if(num_items_arrhythmic > 0) {
    # arrhythmic
    arrhythmic_melodies <- tidy_melodies(res$MST.arrhythmic_melodies)

    arrhythmic_melody_summary <- arrhythmic_melodies %>% dplyr::select(
    correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
     correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
      similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
        dplyr::mutate_if(is.character,as.numeric) %>%
          dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
  }

  if(num_items_rhythmic > 0) {
    # rhythmic
    rhythmic_melodies <- tidy_melodies(res$MST.rhythmic_melodies)

    rhythmic_melody_summary <- rhythmic_melodies %>% dplyr::select(
      correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
      correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
      similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
        dplyr::mutate_if(is.character,as.numeric) %>%
          dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
  }

  list("long_note" = ifelse(is.null(long_tone_summary), data.frame(mean_note_accuracy = 1, note_precision = 1, mean_dtw_distance = 1), long_tone_summary),
       "arrhythmic" = ifelse(is.null(arrhythmic_melody_summary), data.frame(accuracy = 1, similarity = 1), arrhythmic_melody_summary),
       "rhythmic" = ifelse(is.null(rhythmic_melody_summary), data.frame(accuracy = 1, similarity = 1), rhythmic_melody_summary))

}




#' Present Final Results
#'
#' @param test_name
#' @param url
#' @param num_items_long_tone
#' @param num_items_arrhythmic
#' @param num_items_rhythmic
#'
#' @return
#' @export
#'
#' @examples
final_results <- function(test_name, url, num_items_long_tone, num_items_arrhythmic, num_items_rhythmic) {
  c(
    psychTestR::reactive_page(function(state, ...) {
      res <- as.list(psychTestR::get_results(state, complete = FALSE))

      processed_results <- present_scores(res, num_items_long_tone, num_items_arrhythmic, num_items_rhythmic)

      psychTestR::set_local("similarity",
                            mean(processed_results$arrhythmic$similarity,
                                 processed_results$arrhythmic$similarity, na.rm = TRUE)
                            , state)

      psychTestR::set_local("accuracy",
                            mean(processed_results$rhythmic$accuracy_octaves_allowed,
                                 processed_results$rhythmic$accuracy_octaves_allowed, na.rm = TRUE),
                            state)

      final_score <- produce_naive_final_singing_score(res)

      psychTestR::set_local("final_score", final_score, state)


      psychTestR::text_input_page(
        label = "final_score",
        prompt = shiny::tags$div(style = "width: 500px;",
                                 shiny::tags$h2('Final Results'),
                                 shiny::tags$h3('Long Note Scores'),

                                 shiny::renderTable({

                                   long_note_df <- processed_results$long_note
                                   long_note_df_names <- names(long_note_df)
                                   long_note_df <- base::t(long_note_df)
                                   row.names(long_note_df) <- long_note_df_names
                                   long_note_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Arrhythmic Melody Scores'),

                                 shiny::renderTable({

                                   arrhythmic_df <- processed_results$arrhythmic
                                   arrhythmic_df_names <- names(arrhythmic_df)
                                   arrhythmic_df <- base::t(arrhythmic_df)
                                   row.names(arrhythmic_df) <- arrhythmic_df_names
                                   arrhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Rhythmic Melody Scores'),

                                 shiny::renderTable({

                                   rhythmic_df <- processed_results$rhythmic
                                   rhythmic_df_names <- names(rhythmic_df)
                                   rhythmic_df <- base::t(rhythmic_df)
                                   row.names(rhythmic_df) <- rhythmic_df_names
                                   rhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Total Score'),
                                 shiny::tags$p(final_score),
                                 shiny::tags$p("Enter a username to see the scoreboard: ")

        )
      )

    }),

    share_score_page(test_name, url)
  )
}






present_scores_pbet <- function(res, num_items_arrhythmic, num_items_rhythmic) {

  if(num_items_arrhythmic > 0) {
    # arrhythmic
    arrhythmic_melodies <- tidy_melodies(res$PBET.arrhythmic_melodies)

    arrhythmic_melody_summary <- arrhythmic_melodies %>% dplyr::select(
      correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
      correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
      similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
  }

  if(num_items_rhythmic > 0) {
    # rhythmic
    rhythmic_melodies <- tidy_melodies(res$PBET.rhythmic_melodies)

    rhythmic_melody_summary <- rhythmic_melodies %>% dplyr::select(
      correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
      correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
      similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
  }

  list("arrhythmic" = ifelse(is.null(arrhythmic_melody_summary), data.frame(accuracy = 1, similarity = 1), arrhythmic_melody_summary),
       "rhythmic" = ifelse(is.null(rhythmic_melody_summary), data.frame(accuracy = 1, similarity = 1), rhythmic_melody_summary))

}




#' Present Final Results PBET
#'
#' @param test_name
#' @param url
#' @param num_items_find_this_note
#' @param num_items_arrhythmic
#' @param num_items_rhythmic
#'
#' @return
#' @export
#'
#' @examples
final_results_pbet <- function(test_name = "PBET",
                               url = "https://adaptiveeartraining.com",
                               num_items_find_this_note = 0L,
                               num_items_arrhythmic = 0L,
                               num_items_rhythmic = 0L) {

  c(
    psychTestR::reactive_page(function(state, ...) {
      res <- as.list(psychTestR::get_results(state, complete = FALSE))

      processed_results <- present_scores_pbet(res, num_items_arrhythmic, num_items_rhythmic)

      psychTestR::set_local("similarity",
                            mean(processed_results$arrhythmic$similarity,
                                 processed_results$arrhythmic$similarity, na.rm = TRUE)
                            , state)

      psychTestR::set_local("accuracy",
                            mean(processed_results$rhythmic$accuracy_octaves_allowed,
                                 processed_results$rhythmic$accuracy_octaves_allowed, na.rm = TRUE),
                            state)

      final_score <- produce_naive_final_pbet_score(res, num_items_arrhythmic, num_items_rhythmic)

      psychTestR::set_local("final_score", final_score, state)


      psychTestR::text_input_page(
        label = "final_score",
        prompt = shiny::tags$div(style = "width: 500px;",
                                 shiny::tags$h2('Final Results'),

                                 shiny::tags$h3('Arrhythmic Melody Scores'),

                                 shiny::renderTable({

                                   arrhythmic_df <- processed_results$arrhythmic
                                   arrhythmic_df_names <- names(arrhythmic_df)
                                   arrhythmic_df <- base::t(arrhythmic_df)
                                   row.names(arrhythmic_df) <- arrhythmic_df_names
                                   arrhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Rhythmic Melody Scores'),

                                 shiny::renderTable({

                                   rhythmic_df <- processed_results$rhythmic
                                   rhythmic_df_names <- names(rhythmic_df)
                                   rhythmic_df <- base::t(rhythmic_df)
                                   row.names(rhythmic_df) <- rhythmic_df_names
                                   rhythmic_df
                                 }, rownames = TRUE, colnames = FALSE, width = "50%"),

                                 shiny::tags$h3('Total Score'),
                                 shiny::tags$p(final_score),
                                 shiny::tags$p("Enter a username to see the scoreboard: ")

        )
      )

    }),

    share_score_page(test_name, url, hashtag = "PlayByEar")
  )
}


# leaderboard stuff

load_leaderboard <- function() {
  if(file.exists('output/leaderboard.rda')) {
    load('output/leaderboard.rda')
  } else {
    # or create if doesn't exist
    leaderboard <- tibble::tibble(
      username = character(0),
      score = numeric(0)
    )
    save(leaderboard, file = 'output/leaderboard.rda')
  }
  leaderboard
}

add_score_to_leaderboard <- function(username, score) {

  leaderboard <- load_leaderboard()

  new_score <- tibble::tibble(
    username = username,
    score = score
  )
  leaderboard <- rbind(leaderboard, new_score) %>% dplyr::arrange(desc(score))

  leaderboard <- leaderboard[!is.na(leaderboard$score), ]

  save(leaderboard, file = 'output/leaderboard.rda')

  # and present new leaderboard
  leaderboard
}


produce_naive_final_pbet_score <- function(score_result_object,
                                           num_items_arrhythmic,
                                           num_items_rhythmic) {

  print('produce_naive_final_pbet_score')
  scor <- present_scores_pbet(score_result_object, num_items_arrhythmic, num_items_rhythmic)
  print('scor')
  print(scor)
  arr_scors <- scor$arrhythmic
  print('arr_scors: ')
  print(arr_scors)
  final_arr <- round(arr_scors[[1]] * 1000)
  print('final_arr: ')
  print(final_arr)
  rhy_scors <- scor$rhythmic
  print('rhy_scors: ')
  print(rhy_scors)
  final_rhy <- round(rhy_scors[[1]] * 1000)
  print('final_rhy: ')
  print(final_rhy)
  final_score <- final_arr + final_rhy
  print('final_score: ')
  print(final_score)
  final_score

}

produce_naive_final_singing_score <- function(score_result_object) {

  scor <- present_scores(score_result_object)

  ln_scores <- scor$long_note
  final_ln <- round(ln_scores$mean_note_accuracy * ln_scores$note_precision * ln_scores$mean_dtw_distance /1000000)

  arr_scors <- scor$arrhythmic
  final_arr <- round(arr_scors$correct_by_note_events_log_normal + arr_scors$accuracy + ifelse(is.nan(arr_scors$similarity), 1, arr_scors$similarity) * 1000)

  rhy_scors <- scor$rhythmic
  final_rhy <- round(rhy_scors$correct_by_note_events_log_normal + rhy_scors$accuracy + ifelse(is.nan(arr_scors$similarity), 1, arr_scors$similarity) * 1000)

  final_score <- final_arr + final_rhy - final_ln
  final_score

}



share_score_page <- function(test_name, url, hashtag = "CanISing") {

  hashtag <- paste0("%23", hashtag)

  psychTestR::reactive_page(function(state, answer, ...) {

  score <- psychTestR::get_local("final_score", state)
  username <- answer

  leaderboard <- shiny::renderTable({
    add_score_to_leaderboard(username, score)
  }, rownames = TRUE, colnames = FALSE, width = "50%")



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


  psychTestR::one_button_page(shiny::tags$div(
    shiny::tags$h1("Leaderboard"),
    shiny::tags$div(leaderboard),
    shiny::tags$h1("Share Your Score!"),
    shiny::tags$table(style = " border-spacing: 10px; border-collapse: separate;",
      shiny::tags$tr(
        purrr::pmap(socials, function(img, url){
          create_share_button(img, url)
          })

      )
    )
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



