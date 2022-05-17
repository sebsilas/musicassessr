# singing

score_cents_deviation_from_nearest_stimuli_pitch <- function(user_prod_pitches, stimuli, freq) {


  nearest_pitches <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = stimuli,
                                                                           user_production_pitches = user_prod_pitches,
                                                                           allOctaves = TRUE)

  res <- itembankr::vector_cents_between_two_vectors(freq, hrep::midi_to_freq(nearest_pitches))

  res <- mean(abs(res), na.rm = TRUE)
  res

}

### long tone scoring


#' Compute long tone pitch metrics
#'
#' @param target_pitch
#' @param freq
#'
#' @return
#' @export
#'
#' @examples
long_note_pitch_metrics <- function(target_pitch, freq) {


  ## dtw scoring

  ref <- itembankr::produce_arrhythmic_durations(length(freq), hrep::midi_to_freq(target_pitch))
  dtw.distance <- dtw::dtw(freq, ref)$distance

  # note accuracy, interval accuracy, note precision, interval precision
  # see DOI: 10.1121/1.3478782

  cents_vector_in_rel_to_target_note <- itembankr::vector_cents(target_pitch, freq)

  # the note accuracy is the average deviation from the target note
  note.accuracy <- mean(abs(cents_vector_in_rel_to_target_note))

  cents_vector_in_rel_to_mean <- itembankr::vector_cents(mean(freq), freq)
  # the precision is the standard deviation of pitches not in relation to the target note
  # but instead in relation to whatever the mean was the the participant sang
  # in this long note context, then, this measures how stable the pitch was and has a slightly different meaning
  # note precision in the context of melody trials, which is the consistency of producing the same pitch classes on multiple occurences
  # note that note precision has no reference to target pitch and therefore thus independent of accuracy
  note.precision <- sqrt( sum(cents_vector_in_rel_to_mean^2)/length(freq) )

  list("note_accuracy" = note.accuracy,
       "note_precision" = note.precision,
       "dtw_distance" = dtw.distance,
       "agg_dv_long_note" = NA,
       "na_count" = sum(c(is.na(freq) | is.nan(freq))),
       "dtw_distance_max" = max(dtw.distance),
       "note_accuracy_max" = max(note.accuracy),
       "note_precision_max" = max(note.precision),
       "var" = var(freq),
       "run_test" = as.numeric(randtests::runs.test(freq)$statistic),
        "freq_max" = max(freq),
       "freq_min" = min(freq),
        "autocorrelation_mean" = mean(abs(stats::acf(freq,  na.action = stats::na.pass, plot = FALSE)$acf))
       )

}


classify_whether_noise <- function(res, display_noise_trial_notificiation = FALSE) {
  res <- res %>% tibble::as_tibble() %>% dplyr::mutate(
    na_count_test = dplyr::case_when(na_count > 0 ~ "noise"),
    dtw_distance_test = dplyr::case_when(dtw_distance_max > 200000 ~ "noise"),
    note_accuracy_test = dplyr::case_when(note_accuracy_max > 4000 ~ "noise"),
    note_precision_test = dplyr::case_when(note_precision_max > 1000 ~ "noise"),
    freq_min_test = dplyr::case_when(freq_min < 82 ~ "noise"),
    freq_max_test = dplyr::case_when(freq_max > 1046 ~ "noise"),
    autocorrelation_mean_test = dplyr::case_when(autocorrelation_mean > .8 ~ "noise"),
    var_test = dplyr::case_when(var > 15000 ~ "noise"),
    run_test_test = dplyr::case_when(run_test > -15 ~ "noise")
  ) %>% dplyr::mutate(dplyr::across(na_count_test:run_test_test, ~tidyr::replace_na(., "long_notes"))) %>%
    tidyr::pivot_longer(na_count_test:run_test_test, values_to = "prediction")

  failed_tests <- res %>% dplyr::filter(prediction == "noise") %>% dplyr::pull(name)

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
      print(res)
      if(is.null(res)) {
        res <- as.list(psychTestR::get_results(state, complete = FALSE))$SAA.long_tone_trials
      }
      print(res)
      no_trials <- length(res)
      res <- purrr:::map_chr(res, function(trial) {
        as.vector(unlist(trial['noise_classification']))
      })
      t <- table(res)
      no_noise <- as.numeric(t['noise'])
      no_long_notes <- as.numeric(t['long_notes'])

      print(no_noise)
      print(no_long_notes)

      if(is.null(no_noise) | is.na(no_noise)) {
        no_noise <- 0
      }

      if(is.null(no_long_notes) | is.na(no_long_notes)) {
        no_long_notes <- 0
      }
      print('post')
      print(no_noise)
      print(no_long_notes)


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


get_note_precision <- function(pyin_result) {
  note_precision <- pyin_result %>%
    dplyr::group_by(sci_notation) %>%
    dplyr::summarise(sd_for_pitch_class = sd(freq, na.rm = TRUE),
                     participant_precision = mean(sd_for_pitch_class, na.rm = TRUE)) %>%
    dplyr::summarise(note_precision_melody = mean(participant_precision, na.rm = TRUE)) %>%
    dplyr::pull(note_precision_melody)
}



## low level

find_closest_value <- function(x, vector, return_value) {
  # given a value, x, and a vector of values,
  # return the index of the value in the vector, or the value itself, which is closest to x
  # if return_value == TRUE, return the value, otherwise the index
  res <- base::which.min(abs(vector - x))
  res <- ifelse(return_value == TRUE, vector[res], res)
}

#find_closest_value(14, c(1, 6, 12, 28, 33), TRUE)

get_all_octaves_in_gamut <- function(note, gamut_min = midi.gamut.min, gamut_max = midi.gamut.max) {

  # given a note and a range/gamut, find all midi octaves of that note within the specified range/gamut
  res <- c(note)

  # first go down
  while(note > gamut_min) {
    note <- note - 12
    res <- c(res, note)
  }
  # then go up
  while(note < gamut_max) {
    note <- note + 12
    res <- c(res, note)
  }
  res <- res[!duplicated(res)]
  res <- res[order(res)]
  res
}


find_closest_stimuli_pitch_to_user_production_pitches <- function(stimuli_pitches, user_production_pitches, allOctaves = TRUE) {

  # if allOctaves is true, get the possible pitches in all other octaves. this should therefore resolve issues
  # where someone was presented stimuli out of their range and is penalised for it
  if (allOctaves) {
    res <- sapply(user_production_pitches, find_closest_value, get_all_octaves_in_gamut(stimuli_pitches), return_value = TRUE)
  } else {
    res <- sapply(user_production_pitches, find_closest_value, stimuli_pitches, return_value = TRUE)
  }
  res
}


#as <- get_all_octaves_in_gamut(41, midi.gamut.min, midi.gamut.max)

#as2 <- unlist(lapply(c(51, 39, 41, 43), function(x) get_all_octaves_in_gamut(x, midi.gamut.min, midi.gamut.max)))

