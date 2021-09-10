#' pyin
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#'
#' @return
#' @export
#'
#' @examples
pyin <- function(file_name, transform_file = NULL, normalise = FALSE, hidePrint = TRUE) {

  if(is.null(transform_file)) {
    args <- c("-d",
              "vamp:pyin:pyin:notes",
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }

  if(hidePrint) {
    sa_out <- system2(command = "/opt/sonic-annotator/sonic-annotator",
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = "/opt/sonic-annotator/sonic-annotator",
                      args = args,
                      stdout = TRUE)
  }

  if(length(sa_out) == 0) {
    res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
  } else {
    res <- read.csv(text = sa_out, header = FALSE) %>%
      dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        dur = round(dur, 2),
        freq = round(freq, 2),
        note = round(hrep::freq_to_midi(freq)))

    file_name <- res$V1[[1]]

    res <- res %>% dplyr::select(-V1)

    res <- tibble::tibble(file_name, res)
  }
}


pyin <- function(file_name, transform_file = NULL, normalise = FALSE, hidePrint = TRUE) {
  print('pyin')
  file_name <- '/Users/sebsilas/true.wav'
  print(file_name)
  if(is.null(transform_file)) {
    args <- c("-d",
              "vamp:pyin:pyin:notes",
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }

  if(hidePrint) {
    sa_out <- system2(command = "/Users/sebsilas/sonic-annotator",
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = "/Users/sebsilas/sonic-annotator",
                      args = args,
                      stdout = TRUE)
  }

  if(length(sa_out) == 0) {
    res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
  } else {
    res <- read.csv(text = sa_out, header = FALSE) %>%
      dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        dur = round(dur, 2),
        freq = round(freq, 2),
        note = round(hrep::freq_to_midi(freq)))

    file_name <- res$V1[[1]]

    res <- res %>% dplyr::select(-V1)

    res <- tibble::tibble(file_name, res)
  }
}
#pyin('/Users/sebsilas/true.wav')


#' Use pyin on a file
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin <- function(input, ...) {
  print('get_answer_pyin')
  print(input)
  print(input$answer_meta_data)
  file <- paste0('/srv/shiny-server/files/', input$key, '.wav')
  pyin_res <- pyin(file)

  c(
    list(file = file,
         user_satisfied = input$user_satisfied,
         user_rating = input$user_rating),

    melody_scoring_from_user_input(input, result = pyin_res, trial_type = "audio", singing_measures = TRUE)
  )


}


log_normal <- function(x, a = 1) exp(-(log(x)/a)^2)


reverse_answer <- function(ans) {
  # if reverse == TRUE, then check for the reverse of the answer
  if (length(ans) == 1) {
    ans <- stringi::stri_reverse(ans)
  }
  else {
    ans <- rev(ans)
  }
  ans
}

check_answer <- function(user_response, correct_answer,
                         reverse = FALSE,
                         type = c("single_digits", "numbers", "chunks_numbers",
                                  "single_letters", "chunk_letters",
                                  "words",
                                  "midi_pitches",
                                  "rhythms",
                                  "melodies_with_rhythms")
) {

  # check that the lengths are the same
  stopifnot(length(user_response) == length(correct_answer))

  # reverse answer if need be
  if (reverse == TRUE) {
    correct_answer <- reverse_answer(correct_answer)
  }

  # match based on type of input
  if (length(user_response) > 1) {
    correct <- identical(user_response, correct_answer)
  } else if (type == "midi_pitches") {
    # itembankr::ngrukkon()
  } else if (type == "rhythms") {
    ## rhythm metric
    # itembankr::ngrukkon()
  } else if (type == "melodies_with_rhythms") {

  }

  else {
    correct <- user_response == correct_answer
  }


  correct
}

check_similarity <- function(user_response, correct_answer, reverse = FALSE) {
  # if reverse == TRUE, then check for the reverse of the answer

  if (reverse == TRUE) {
    #correct <-
  }
  else {
    #user_response ==
  }
}


# get_answer functions

#' Dummy get_answer function
#'
#' @param input
#' @param state
#' @param text
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_null <- function(input, state, text = "example_page", ...) {
  print("in get answer null")
  list(text)
}


get_answer_midi_note_mode <- function(input, state, ...) {

  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {
    list(note = NA)
  } else {
    list(note = getmode(rjson::fromJSON(input$user_response_midi_note_on)))
  }
}



melody_scoring_from_user_input <- function(input, result, trial_type, user_melody_input = NULL, singing_measures) {

  # onset, dur, freq, note

  if(is.numeric(result$freq)) {
    result <- result %>% dplyr::mutate(cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
                                 # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
                                 pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                                 pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                                 sci_notation = itembankr::midi_to_sci_notation(round(hrep::freq_to_midi(freq))),
                                 interval = c(NA, diff(note)),
                                 ioi = c(NA, diff(onset)),
                                 ioi_class = itembankr::classify_duration(ioi))

    user_melody_input <- result$note
    onsets_noteon <- result$onset

  }
  else {
    print("the result wasn't good")
    return(NA)
  }

  # grab midi related stuff
  if(trial_type == "midi") {
    user_response_midi_note_off <- rjson::fromJSON(input$user_response_midi_note_off)
    onsets_noteon <- rjson::fromJSON(input$onsets_noteon)
    onsets_noteoff <- rjson::fromJSON(input$onsets_noteoff)
  }
  else {
    onsets_noteoff <- NA
    user_response_midi_note_off <- NA
  }


  if(length(user_melody_input) == 0) {
    if(trial_type == "midi") { shiny::showNotification(i18n("nothing_entered")) }
    return(list(user_melody_input = NA,
                reason = "nothing_entered"
                ))
  }

  else {

    stimuli <- itembankr::str_mel_to_vector(input$answer_meta_data$abs_melody, ",")
    stimuli_length <- input$answer_meta_data$N
    durations <- diff(onsets_noteon)/1000

    # calculate measures
    trial_length <- onsets_noteon[length(onsets_noteon)]

    # octave dependent
    no_correct <- sum(as.numeric(user_melody_input %in% stimuli))
    no_errors <- length(user_melody_input) - no_correct
    errors_boolean <- as.vector(!user_melody_input %in% stimuli)

    # octaves independent (i.e octave errors allowed)
    user_pitch_classes <- itembankr::midi_to_pitch_class(user_melody_input) # pitch_class allows octave errors
    stimuli_pitch_classes <- itembankr::midi_to_pitch_class(stimuli)

    correct_boolean_octaves_allowed <- as.vector(user_pitch_classes %in% stimuli_pitch_classes)
    errors_boolean_octaves_allowed <- as.vector(!user_pitch_classes %in% stimuli_pitch_classes)

    no_correct_octaves_allowed <- sum(correct_boolean_octaves_allowed)
    no_errors_octaves_allowed <- sum(errors_boolean_octaves_allowed)

    # accuracy
    accuracy <- get_note_accuracy(stimuli, user_melody_input, no_correct, no_errors)
    accuracy_octaves_allowed <- get_note_accuracy(stimuli, user_melody_input, no_correct_octaves_allowed, no_errors_octaves_allowed)

    # similarity
    similarity <- get_similarity(stimuli, stimuli_length, user_melody_input, durations)
    no_note_events <- length(user_melody_input)

    # by note events measures
    correct_by_note_events <- no_correct/no_note_events
    correct_by_note_events_log_normal <- correct_by_note_events * log_normal(no_note_events/stimuli_length)

    correct_by_note_events_octaves_allowed <- no_correct_octaves_allowed/no_note_events
    correct_by_note_events_octaves_allowed_log_normal <- correct_by_note_events_octaves_allowed * log_normal(no_note_events/stimuli_length)

    if(singing_measures) {
      # singing stuff
      # note precision
      note_precision <- result %>%
        dplyr::group_by(sci_notation) %>%
        dplyr::summarise(sd_for_pitch_class = sd(freq, na.rm = TRUE),
                  participant_precision = mean(sd_for_pitch_class, na.rm = TRUE)) %>%
                    dplyr::summarise(note_precision_melody = mean(participant_precision, na.rm = TRUE))

      # cents_deviation_from_nearest_stimuli_pitch
      mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = result$note,
                                                       stimuli = stimuli, freq = result$freq)

      # mean cents deviation
      mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(result$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)
    } else {
      note_precision <- NA
      mean_cents_deviation_from_nearest_stimuli_pitch <- NA
      mean_cents_deviation_from_nearest_midi_pitch <- NA
    }

    list(stimuli = stimuli,
         stimuli_length = stimuli_length,
         user_response_note = user_melody_input,
         user_response_midi_note_off = user_response_midi_note_off,
         durations = durations,
         user_response_pitch_classes = user_pitch_classes,
         onsets_noteon = onsets_noteon,
         onsets_noteoff = onsets_noteoff,
         trial_length = trial_length,
         no_note_events = no_note_events,
         no_correct = no_correct,
         no_errors = no_errors,
         errors_boolean = errors_boolean,
         correct_boolean_octaves_allowed = correct_boolean_octaves_allowed,
         errors_boolean_octaves_allowed = errors_boolean_octaves_allowed,
         no_correct_octaves_allowed = no_correct_octaves_allowed,
         no_errors_octaves_allowed = no_errors_octaves_allowed,
         correct_by_note_events = correct_by_note_events,
         correct_by_note_events_log_normal = correct_by_note_events_log_normal,
         correct_by_note_events_octaves_allowed = correct_by_note_events_octaves_allowed,
         correct_by_note_events_octaves_allowed_log_normal = correct_by_note_events_octaves_allowed_log_normal,
         accuracy = accuracy,
         accuracy_octaves_allowed = accuracy_octaves_allowed,
         opti3 = similarity,
         note_precision = note_precision,
         mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
         mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
         answer_meta_data = input$answer_meta_data
    )
  }
}





get_answer_midi <- function(input, state, ...) {
  print('get_answer_midi')

  user_response_midi_note_on <- rjson::fromJSON(input$user_response_midi_note_on)

  melody_scoring_from_user_input(input = input, user_melody_input = user_response_midi_note_on, trial_type = "midi", singing_measures = FALSE)
}


get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  print("get_answer_average_frequency_ff")
  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, ...) {
      freqs <- rjson::fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = floor(mean(notes)))
    }

  } else if (floor_or_ceiling == "ceiling") {

    function(input, ...) {
      # process some new info
      freqs <- rjson::fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = ceiling(mean(notes)))
    }

  } else {

    function(input, ...) {
      # process some new info
      freqs <- rjson::fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = round(mean(notes)))
    }
  }

}



#' Get answer and recode the last answer if its corresponding promise has resolved
#'
#' @param input
#' @param state
#' @param opt
#' @param scoring
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_store_async <- function(input, state, opt, scoring = "melody", ...) {

  current_results <- as.list(psychTestR::get_results(state, complete = FALSE))$results

  latest_result <- current_results[[length(current_results)]]

  if(length(latest_result) > 1 & promises::is.promise(latest_result$promise_result)) {
    if(future::resolved(latest_result$promise_result)) {
      promises::then(latest_result$promise_result,
           function(value) {

             if(scoring == "melody") {
               print('boom melody')
               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    result = value,
                                                                    trial_type = "audio",
                                                                    singing_measures = TRUE)
             } else if(scoring == "long_note") {
               print('booom long-note')
               print(input$stimuli)
               print(value$freq)
               target_pitch <- rjson::fromJSON(input$stimuli)
               pitch_vector <- rjson::fromJSON(value$freq)
               ans_plus_meta_data <- long_note_pitch_metrics(target_pitch, pitch_vector)

             } else {
               warning('Unknown scoring function')
             }

             # update psychTestR results object
             psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data

           })
    }
  }

  print("checked latest")
  print(input$sourceBucket)
  print(input$key)
  print(input$destBucket)
  print(api_url)

  json <- rjson::toJSON(list(sourceBucket = input$sourceBucket,
                             key = input$key,
                             destBucket = input$destBucket))

  page_promise <- future::future({ async::synchronise(do(api_url, json)) }) %...>% (function(result) {
        result
  })

  list(key = input$key,
       promise_result = page_promise)

}

do <- function(api_url, json) {

  headers <- c("content-type" = "application/json")
  async::http_post(api_url, data = json, headers = headers)$
    then(async::http_stop_for_status)$
    then(function(x) {

      key <- rjson::fromJSON(rawToChar(x$content))$key
      bucket <- rjson::fromJSON(rawToChar(x$content))$Bucket
      link_href <- paste0("https://", bucket, ".s3.amazonaws.com/", key)
      print('link_href!')
      print(link_href)
      csv <- readr::read_csv(link_href, col_names = c("onset", "dur", "freq"))
      print('answer back!')
      print(csv)
      csv

    })
}


#' Simple saving of the corresponding URL of a trial file
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_save_aws_key <- function(input, ...) {
  print('get_answer_save_aws_key')
  print(input$key)
  print(input$file_url)
  print(input$user_rating)
  list(key = input$key,
       file_url = input$file_url,
       user_satisfied = input$user_satisfied,
       user_rating = input$user_rating)
}


get_answer_store_async_long_note <- function(...) {
  print('get_answer_store_async_long_note!')
  get_answer_store_async(scoring = "long_note", ...)
}


#' Recode an asynchronous result through a psychTestR on_complete function
#'
#' @param input
#' @param state
#' @param opt
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
on_complete_recode_async_penultimate <- function(input, state, opt, ...) {

  print('on_complete_recode_async_penultimate!')

  current_results <- as.list(psychTestR::get_results(state, complete = FALSE))$results

  # first check penultimate result, then latest (this needs factoring!!!)


  penultimate_result <- current_results[[length(current_results)-1]]

  if(length(penultimate_result) > 1 ) {
    if(promises::is.promise(penultimate_result$promise_result)) {
      if(future::resolved(penultimate_result$promise_result)) {
        promises::then(penultimate_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$pitch,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio",
                                                                    singing_measures = TRUE)

               psychTestR::results(state)$results[[length(current_results)-1]]$promise_result <- ans_plus_meta_data


             })
      }
    }
  }

  print('checked penultimate')


  latest_result <- current_results[[length(current_results)]]
  print('latest_result')
  print(latest_result)

  if(length(latest_result) > 1) {
    if(promises::is.promise(latest_result$promise_result)) {
      if(future::resolved(latest_result$promise_result)) {
        promises::then(latest_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$pitch,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio",
                                                                    singing_measures = TRUE)

               psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data


             })
      }
    }
  }


  print("checked latest")

}

on_complete_recode_async <- function(input, state, opt, ...) {

  print('on_complete_recode_async!')

  current_results <- as.list(psychTestR::get_results(state, complete = FALSE))$results

  # first check penultimate result, then latest (this needs factoring!!!)


  latest_result <- current_results[[length(current_results)]]
  print('latest_result')
  print(latest_result)

  if(length(latest_result) > 1) {
    if(promises::is.promise(latest_result$promise_result)) {
      if(future::resolved(latest_result$promise_result)) {
        promises::then(latest_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$pitch,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio",
                                                                    singing_measures = TRUE)

               psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data

             })
      }
    }
  }


  print("checked latest")

}


get_note_accuracy <- function(stimuli, user_melody_input, no_correct, no_errors) {
  # accuracy
  if (no_errors == 0 & no_correct == length(stimuli)) {
    accuracy <- 1
  } else {
    accuracy <- no_correct/length(user_melody_input)
  }

}

get_similarity <- function(stimuli, stimuli_length, user_melody_input, durations) {
  # similarity
  if(length(user_melody_input) < 3 | stimuli_length < 3) {
    similarity <- NA
    ng <- NA
  } else {
    similarity <- itembankr::opti3(pitch_vec1 = stimuli,
                                   dur_vec1 = rep(0.5, stimuli_length),
                                   pitch_vec2 = user_melody_input,
                                   dur_vec2 = durations)
  }
}
# do <- cbind(WJD[5, ], data.frame(abs_melody = "64,62,57"))
# ta <- melody_scoring_from_user_input(input = list(answer_meta_data = do),
#                                      user_melody_input = 60:65,
#                                      onsets_noteon = 1:5,
#                                      trial_type = "audio"
#                                      )

# tests
# check_answer("a", "b", type = "single_letters")
# check_answer("a", "a", type = "single_letters")
# check_answer("aaa", "bbb", reverse = FALSE, type = "chunk_letters")
# check_answer("aaa", "aaa", type = "chunk_letters")
# check_answer(11, 22, type = "single_numbers")
# check_answer(11, 11, type = "single_numbers")
# check_answer(11, 11, reverse = TRUE,  type = "chunk_digits")
# check_answer(123, 123, reverse = TRUE, type = "chunk_digits")
# check_answer(123, 321, reverse = TRUE, type = "chunk_digits")
#
# check_answer(1:3, 3:1, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 3:1, reverse = FALSE, type = "chunk_digits")
# check_answer(1:3, 1:3, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 1:4, reverse = TRUE, type = "chunk_digits")
#
# check_answer(c(10,50,60), c(10,50,60), type = "midi_pitches")
# check_answer(c(10,50,60), c(10,50,60), reverse = TRUE, type = "midi_pitches")
# check_answer(c(10,50,60), c(60,50,10), reverse = TRUE, type = "midi_pitches")
#
#

