

# Long note screening noise classification

classify_whether_noise <- function(res, display_noise_trial_notificiation = FALSE) {
  res <- res %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      long_note_na_count_test = dplyr::case_when(long_note_na_count > 0 ~ "noise"),
      long_note_dtw_distance_test = dplyr::case_when(long_note_dtw_distance_max > 200000 ~ "noise"),
      long_note_note_accuracy_test = dplyr::case_when(long_note_accuracy_max > 4000 ~ "noise"),
      long_note_freq_min_test = dplyr::case_when(long_note_freq_min < 82 ~ "noise"),
      long_note_freq_max_test = dplyr::case_when(long_note_freq_max > 1046 ~ "noise"),
      long_note_autocorrelation_mean_test = dplyr::case_when(long_note_autocorrelation_mean > .8 ~ "noise"),
      long_note_var_test = dplyr::case_when(long_note_var > 15000 ~ "noise"),
      long_note_run_test_test = dplyr::case_when(long_note_run_test > -15 ~ "noise")
    ) %>%
    dplyr::mutate(dplyr::across(long_note_na_count_test:long_note_run_test_test, ~tidyr::replace_na(., "long_notes"))) %>%
    tidyr::pivot_longer(long_note_na_count_test:long_note_run_test_test, values_to = "prediction")

  failed_tests <- res %>%
    dplyr::filter(prediction == "noise") %>% dplyr::pull(name)

  if(length(failed_tests) == 0) {
    failed_tests <- NA
  }

  res_counts <- res %>% dplyr::count(prediction)

  if(any(grepl("noise", res_counts$prediction))) {
    no_tests_failed <- res_counts %>% dplyr::filter(prediction == "noise") %>% dplyr::pull(n)
    if(length(no_tests_failed) == 0) {
      no_tests_failed <- 0
    }
    cat('no tests failed: ', no_tests_failed, '\n')
    if(no_tests_failed >= 2) {
      prediction <- "noise"
    } else {
      prediction <- "long_notes"
    }

  } else {
    prediction <- "long_notes"
  }


  list(prediction = prediction,
       failed_tests = failed_tests)
}

#' End of long note trial screening code block
#'
#' @return
#' @export
#'
#' @examples
end_of_long_note_trial_screening <- function(failure_page = 'http://www.google.com') {
  psychTestR::join(
    psychTestR::code_block(function(state, answer, ...) {
      print('end_of_long_note_trial_screening')
      res <- as.list(psychTestR::get_results(state, complete = FALSE))$long_tone_trials

      if(is.null(res)) {
        res <- as.list(psychTestR::get_results(state, complete = FALSE))$SAA.long_tone_trials
      }
      no_trials <- length(res)
      res <- purrr:::map_chr(res, function(trial) {
        as.vector(unlist(trial['noise_classification']))
      })
      t <- table(res)
      no_noise <- as.numeric(t['noise'])
      no_long_notes <- as.numeric(t['long_notes'])

      if(is.null(no_noise) | is.na(no_noise)) {
        no_noise <- 0
      }

      if(is.null(no_long_notes) | is.na(no_long_notes)) {
        no_long_notes <- 0
      }

      if(no_noise == 0 | no_long_notes == 6) {
        final_prediction <- "long_notes"
        pretty_final_prediction <- "Long Notes"
      } else {
        if(no_noise >= (no_trials/2)+1) { # i.e., majority rule
          final_prediction <- "noise"
          pretty_final_prediction <- "Noise"
        } else {
          final_prediction <- "long_notes"
          pretty_final_prediction <- "Long Notes"
        }

      }
      # noise_classification
      psychTestR::set_global("final_long_note_block_prediction_pretty", pretty_final_prediction, state)
      psychTestR::set_global("final_long_note_block_prediction", final_prediction, state)
      final_prediction <- psychTestR::set_global("no_failed_long_note_trials", no_noise, state)

    }),
    long_tone_screening_v2(failure_page))
}



long_tone_screening <- function(failure_page = 'http://www.google.com') {
  psychTestR::join(

    psychTestR::code_block(function(state, ...) {
      res <- as.list(psychTestR::results(state))
      participant_long_note_summary <- res$SAA.long_note_trials
      participant_long_note_summary <- purrr::map_dfr(1:length(participant_long_note_summary), function(i) {
        tibble::tibble(trial = names(participant_long_note_summary)[i], long_note_IRT = participant_long_note_summary[[i]]$long_note_IRT)
      })
      # check long note score from model
      avg_long_tone_screening <- participant_long_note_summary %>%
        dplyr::summarise(avg_long_note_IRT = mean(long_note_IRT, na.rm = TRUE)) %>%
        dplyr::pull(avg_long_note_IRT)
      psychTestR::set_global("long_tone_screening_score", avg_long_tone_screening, state)
    }),

    psychTestR::conditional(test = function(state, ...) {
      score <- psychTestR::get_global("long_tone_screening_score", state)
      !dplyr::between(score, -0.9040568, 1.763759) # i.e., proceed if the score is between these values (and skip the redirect page)
    }, logic = redirect_page(url = failure_page, final = TRUE))


  )
}

long_tone_screening_v2 <- function(failure_page) {
  psychTestR::conditional(test = function(state, ...) {
    final_long_note_block_prediction <- psychTestR::get_global("final_long_note_block_prediction", state)
    final_long_note_block_prediction == "noise"
  }, logic = redirect_page(url = failure_page, final = TRUE))
}


