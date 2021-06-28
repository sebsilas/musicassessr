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
  }

  else if (type == "midi_pitches") {
    # ngrukkon()
  }

  else if (type == "rhythms") {
    ## rhythm metric
    # ngrukkon()
  }

  else if (type == "melodies_with_rhythms") {

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

get_answer_null <- function(input, state, text = "example_page", ...) {
  print("in get answer null")
  list(text)
}


get_answer_midi_note_mode <- function(input, state, ...) {

  if(length(fromJSON(input$user_response_midi_note_on)) == 0) {
    list(note = NA)
  } else {
    list(note = getmode(fromJSON(input$user_response_midi_note_on)))
  }
}


get_note_accuracy <- function(stimuli, user_melody_input, no_correct, no_errors) {
  # accuracy
  if (no_errors == 0 & no_correct == length(stimuli)) {
    accuracy <- 1
  }
  else {
    accuracy <- no_correct/length(user_melody_input)
  }

}

get_similarity <- function(stimuli, stimuli_length, user_melody_input, durations) {
  # similarity
  if(length(user_melody_input) < 3) {
    similarity <- NA
    ng <- NA
  }
  else {
    similarity <- opti3(pitch_vec1 = stimuli,
                        dur_vec1 = rep(0.5, stimuli_length),
                        pitch_vec2 = user_melody_input,
                        dur_vec2 = durations)
  }
}


melody_scoring_from_user_input <- function(input, user_melody_input, onsets_noteon = NA, trial_type) {

  # grab midi related stuff
  if(trial_type == "midi") {
    user_response_midi_note_off <- fromJSON(input$user_response_midi_note_off)
    onsets_noteon <- fromJSON(input$onsets_noteon)
    onsets_noteoff <- fromJSON(input$onsets_noteoff)
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
    stimuli <- str.mel.to.vector(input$answer_meta_data$abs_melody, ",")
    stimuli_length <- input$answer_meta_data$N
    durations <- diff(onsets_noteon)/1000


    # calculate measures
    trial_length <- onsets_noteon[length(onsets_noteon)]

    # octave dependent
    no_correct <- sum(as.numeric(user_melody_input %in% stimuli))
    no_errors <- length(user_melody_input) - no_correct
    errors_boolean <- as.vector(!user_melody_input %in% stimuli)

    # octaves independent (i.e octave errors allowed)
    user_pitch_classes <- midi.to.pitch.class(user_melody_input) # pitch_class allows octave errors
    stimuli_pitch_classes <- midi.to.pitch.class(stimuli)

    correct_boolean_octaves_allowed <- as.vector(user_pitch_classes %in% stimuli_pitch_classes)
    errors_boolean_octaves_allowed <- as.vector(!user_pitch_classes %in% stimuli_pitch_classes)

    no_correct_octaves_allowed <- sum(correct_boolean_octaves_allowed)
    no_errors_octaves_allowed <- sum(errors_boolean_octaves_allowed)


    accuracy <- get_note_accuracy(stimuli, user_melody_input, no_correct, no_errors)
    accuracy_octaves_allowed <- get_note_accuracy(stimuli, user_melody_input, no_correct_octaves_allowed, no_errors_octaves_allowed)

    similarity <- get_similarity(stimuli, stimuli_length, user_melody_input, durations)
    no_note_events <- length(user_melody_input)

    # by note events measures
    correct_by_note_events <- no_correct/no_note_events
    correct_by_note_events_log_normal <- correct_by_note_events * log_normal(no_note_events/stimuli_length)

    correct_by_note_events_octaves_allowed <- no_correct_octaves_allowed/no_note_events
    correct_by_note_events_octaves_allowed_log_normal <- correct_by_note_events_octaves_allowed * log_normal(no_note_events/stimuli_length)


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
         answer_meta_data = input$answer_meta_data
    )
  }
}

get_answer_midi <- function(input, state, ...) {
  print('get_answer_midi')

  user_response_midi_note_on <- fromJSON(input$user_response_midi_note_on)

  melody_scoring_from_user_input(input = input, user_melody_input = user_response_midi_note_on, trial_type = "midi")
}


get_answer_average_frequency_ff <- function(floor_or_ceiling, ...) {

  print("get_answer_average_frequency_ff")
  # function factory
  # either round up or down to not go too low or too high for the user when rounding

  if (floor_or_ceiling == "floor") {

    function(input, ...) {
      freqs <- fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = floor(mean(notes)))
    }

  }

  else if (floor_or_ceiling == "ceiling") {

    function(input, ...) {
      # process some new info
      freqs <- fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = ceiling(mean(notes)))
    }

  }


  else {

    function(input, ...) {
      # process some new info
      freqs <- fromJSON(input$user_response_frequencies)
      notes <- tidy_freqs(freqs)
      list(user_response = round(mean(notes)))
    }
  }

}


get_answer_store_async <- function(input, state, opt, ...) {

  print('get_answer_store_async!')

  current_results <- as.list(get_results(state, complete = FALSE))$results

  latest_result <- current_results[[length(current_results)]]

  if(length(latest_result) > 1 & is.promise(latest_result$promise_result)) {
    if(future::resolved(latest_result$promise_result)) {
      then(latest_result$promise_result,
           function(value) {

             ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                  user_melody_input = value$midi,
                                                                  onsets_noteon = value$onset,
                                                                  trial_type = "audio")

             psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data


           })
    }
  }

  print("checked latest")

  json <- rjson::toJSON(list(sourceBucket = input$sourceBucket,
                             key = input$key,
                             destBucket = input$destBucket))


  do <- function() {
    headers <- c("content-type" = "application/json")
    http_post(api_url, data = json, headers = headers)$
      then(http_stop_for_status)$
      then(function(x) {

        key <- rjson::fromJSON(rawToChar(x$content))$key
        bucket <- rjson::fromJSON(rawToChar(x$content))$Bucket
        link_href <- paste0("https://", bucket, ".s3.amazonaws.com/", key)
        csv <- read_csv(link_href, col_names = c("onset", "dur", "freq"))
        print('answer back!')
        print(csv)
        if(is.numeric(csv$freq)) {
          csv <- csv %>% mutate(midi = round(freq_to_midi(freq)))
          print(csv$midi)
        }
        else {
          print("the result wasn't good")
          csv <- NA
        }
        csv

      })
  }

  user_response <- NULL

  page_promise <- future({ synchronise(do()) })%...>% (function(result) {
        result
  })

  list(key = input$key,
       promise_result = page_promise)

}



on_complete_recode_async_penultimate <- function(input, state, opt, ...) {

  print('on_complete_recode_async_penultimate!')

  current_results <- as.list(get_results(state, complete = FALSE))$results

  # first check penultimate result, then latest (this needs factoring!!!)


  penultimate_result <- current_results[[length(current_results)-1]]

  if(length(penultimate_result) > 1 ) {
    if(is.promise(penultimate_result$promise_result)) {
      if(future::resolved(penultimate_result$promise_result)) {
        then(penultimate_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$midi,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio")

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
    if(is.promise(latest_result$promise_result)) {
      if(future::resolved(latest_result$promise_result)) {
        then(latest_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$midi,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio")

               psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data


             })
      }
    }
  }


  print("checked latest")

}

on_complete_recode_async <- function(input, state, opt, ...) {

  print('on_complete_recode_async!')

  current_results <- as.list(get_results(state, complete = FALSE))$results

  # first check penultimate result, then latest (this needs factoring!!!)


  latest_result <- current_results[[length(current_results)]]
  print('latest_result')
  print(latest_result)

  if(length(latest_result) > 1) {
    if(is.promise(latest_result$promise_result)) {
      if(future::resolved(latest_result$promise_result)) {
        then(latest_result$promise_result,
             function(value) {

               ans_plus_meta_data <- melody_scoring_from_user_input(input = input,
                                                                    user_melody_input = value$midi,
                                                                    onsets_noteon = value$onset,
                                                                    trial_type = "audio")

               psychTestR::results(state)$results[[length(current_results)]]$promise_result <- ans_plus_meta_data

             })
      }
    }
  }


  print("checked latest")

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

