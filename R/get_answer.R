#pyin('/Users/sebsilas/true.wav')
#library(dplyr)
melconv <- function(file_name, return_notes_and_durs = TRUE) {

  # then use melconv
  melconv_res <- system2(command = '../melospy/bin/melconv',
                    args = c('-f midi',
                             paste0('-i ', file_name)),
                    stdout = TRUE, stderr = FALSE)

  res <- strsplit(file_name, "/", fixed = TRUE)[[1]]
  res <- res[length(res)]
  res <- strsplit(res, ".", fixed = TRUE)[[1]][1]
  # res <- paste0('/Users/sebsilas/musicassessr/', res, '.mid')
  res <- paste0(res, '.mid')
  if(return_notes_and_durs) {
    itembankr::midi_file_to_notes_and_durations(res)
  } else {
    tuneR::readMidi(res)
  }

}


# melconv_res <- system2(command = '../melospy/bin/melconv',
#                        args = c('-f midi',
#                                 paste0('-i ', '/Users/sebsilas/musicassessr/6236558644498592.csv')),
#                        stdout = TRUE, stderr = FALSE)
#
#
#
# melconv_res <- melconv('/Users/sebsilas/29-3-2021--8-19_vamp_pyin_pyin_notes.csv')

melconv_from_pyin_res <- function(pyin_res) {

  #pyin_res <- pyin(audio_file)

  # sort the problematic file format

  # f <- readr::read_csv(audio_file, col_names = c('onset_s','duration_s', 'pitch_hz')) %>%
  #   dplyr::select(onset_s, pitch_hz, duration_s)

  pyin_res <- pyin_res %>% dplyr::select(onset, freq, dur)

  new_file <- paste0(paste0(sample(1:9, 20, replace = TRUE), collapse = ""), '.csv')

  print(new_file)
  # write out
  write.table(pyin_res, file = new_file, row.names = FALSE, col.names = FALSE, sep=",")

  melconv(new_file)

}

#r <- melconv_from_pyin_res('/Users/sebsilas/true.wav')

#' pyin
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#' @param type
#'
#' @return
#' @export
#'
#' @examples
pyin <- function(file_name, transform_file = NULL,
                 normalise = FALSE, hidePrint = TRUE, type = "notes") {

  if(type == "pitch_track") {
    vamp_cmd <- "vamp:pyin:pyin:smoothedpitchtrack"
  } else if(type == "notes") {
    vamp_cmd <- "vamp:pyin:pyin:notes"
  } else {
    stop("Unknown type")
  }

  if(is.null(transform_file)) {
    args <- c("-d",
              vamp_cmd,
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
    if(type == "notes") {
      res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
    } else {
      res <- tibble::tibble(onset = NA, freq = NA, file_name = file_name)
    }
  } else {
    res <- read.csv(text = sa_out, header = FALSE)

    if(type == "notes") {
      res <- res %>%
        dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
        dplyr::mutate(
          onset = round(onset, 2),
          dur = round(dur, 2),
          freq = round(freq, 2),
          note = round(hrep::freq_to_midi(freq)))
    } else {
      res <- res %>%
        dplyr::rename(onset = V2, freq = V3) %>%
        dplyr::mutate(
          onset = round(onset, 2),
          freq = round(freq, 2))
    }

    file_name <- res$V1[[1]]

    res <- res %>% dplyr::select(-V1)

    res <- tibble::tibble(file_name, res)
  }
}



# pyin <- function(file_name, transform_file = NULL,
#                  normalise = FALSE, hidePrint = TRUE, type = "notes") {
#
#   file_name <- '/Users/sebsilas/true.wav'
#   #file_name <- '/Users/sebsilas/Downloads/13-8-2021--17-8--18.wav'
#
#
#   if(type == "pitch_track") {
#     vamp_cmd <- "vamp:pyin:pyin:smoothedpitchtrack"
#   } else if(type == "notes") {
#     vamp_cmd <- "vamp:pyin:pyin:notes"
#   } else {
#     stop("Unknown type")
#   }
#
#   if(is.null(transform_file)) {
#     args <- c("-d",
#               vamp_cmd,
#               file_name,
#               "-w",
#               "csv --csv-stdout")
#   } else {
#     args <- c(paste0('-t ', transform_file),
#               file_name,
#               "-w",
#               "csv --csv-stdout")
#   }
#
#   if(normalise == 1) {
#     args <- c(args, "--normalise")
#   }
#
#   if(hidePrint) {
#     sa_out <- system2(command = "/Users/sebsilas/sonic-annotator",
#                       args = args,
#                       stdout = TRUE, stderr = FALSE)
#   } else {
#     sa_out <- system2(command = "/Users/sebsilas/sonic-annotator",
#                       args = args,
#                       stdout = TRUE)
#   }
#
#   if(length(sa_out) == 0) {
#     if(type == "notes") {
#       res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
#     } else {
#       res <- tibble::tibble(onset = NA, freq = NA, file_name = file_name)
#     }
#   } else {
#     res <- read.csv(text = sa_out, header = FALSE)
#
#     if(type == "notes") {
#       res <- res %>%
#         dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
#         dplyr::mutate(
#           onset = round(onset, 2),
#           dur = round(dur, 2),
#           freq = round(freq, 2),
#           note = round(hrep::freq_to_midi(freq)))
#     } else {
#       res <- res %>%
#         dplyr::rename(onset = V2, freq = V3) %>%
#         dplyr::mutate(
#           onset = round(onset, 2),
#           freq = round(freq, 2))
#     }
#
#     file_name <- res$V1[[1]]
#
#     res <- res %>% dplyr::select(-V1)
#
#     res <- tibble::tibble(file_name, res)
#   }
# }


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
get_answer_pyin <- function(input, type = c("both", "note", "pitch_track"), state,  ...) {

  pyin_pitch_track <- NULL
  file <- paste0('/srv/shiny-server/files/', input$key, '.wav')

  if(type == "note") {
    pyin_res <- pyin(file)
  } else if(type == "pitch_track") {
    pyin_pitch_track <- pyin(file, type = "pitch_track")
  } else {
    pyin_res <- pyin(file)
    pyin_pitch_track <- pyin(file, type = "pitch_track")
  }

  print('get_answer_pyin')
  stimuli_both <- psychTestR::get_global("melody", state)
  print(stimuli_both)
  stimuli <- stimuli_both$melody
  print(stimuli)
  stimuli_durations <- stimuli_both$dur_list
  print(stimuli_durations)
  stimuli_durations <- ifelse(!is.na(stimuli_durations) | !is.null(stimuli_durations), stimuli_durations, NA)
  print(stimuli_durations)

  melconv_res <- melconv_from_pyin_res(pyin_res)

  if(is.na(pyin_res$onset)) {

    res <- list(error = NA, reason = "pyin returned no result", user_satisfied = input$user_satisfied)
  } else {

    res <- c(
      list(file = file,
           user_satisfied = input$user_satisfied,
           user_rating = input$user_rating,
           melconv_notes = itembankr::str_mel_to_vector(melconv_res$notes),
           melconv_dur = itembankr::str_mel_to_vector(melconv_res$dur)
           ),

      melody_scoring_from_user_input(input, result = if(!is.null(pyin_res)) pyin_res, trial_type = "audio", singing_measures = TRUE,
                                     pyin_pitch_track = if(!is.null(pyin_pitch_track)) pyin_pitch_track, stimuli = stimuli, stimuli_durations = stimuli_durations)
    )
  }
  res


}



#' get_answer pyin for long notes
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_answer_pyin_long_note <- function(input, ...) {

  print('get_answer_pyin_long_note')
  file <- paste0('/srv/shiny-server/files/', input$key, '.wav')
  pyin_res <- pyin(file, type = "pitch_track")
  print(pyin_res)

  c(
    list(file = file,
         stimuli = as.numeric(input$stimuli),
         onset = pyin_res$onset,
         freq = pyin_res$freq
         ),

    long_note_pitch_metrics(as.numeric(input$stimuli), pyin_res)
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
  print('getanswermidi note mode')
  if(length(rjson::fromJSON(input$user_response_midi_note_on)) == 0) {
    list(note = NA)
  } else {
    list(note = getmode(rjson::fromJSON(input$user_response_midi_note_on)))
  }
}



melody_scoring_from_user_input <- function(input, result, trial_type, user_melody_input = NULL, singing_measures, pyin_pitch_track = NULL, stimuli = NA, stimuli_durations = NA) {

  print('melody_scoring_from_user_input')
  print(stimuli)
  print(stimuli_durations)

  # onset, dur, freq, note

  if(is.numeric(result$freq)) {

    result <- result %>% dplyr::mutate(cents_deviation_from_nearest_midi_pitch = vector_cents_between_two_vectors(round(hrep::midi_to_freq(hrep::freq_to_midi(freq))), freq),
                                 # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
                                 pitch_class = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                                 pitch_class_numeric = itembankr::midi_to_pitch_class(round(hrep::freq_to_midi(freq))),
                                 sci_notation = itembankr::midi_to_sci_notation(note),
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
    return(list(user_melody_input = NA, reason = "nothing_entered"))
  }

  else {

    stimuli_length <- input$answer_meta_data$N

    if(is.null(result$dur)) {
      durations <- diff(onsets_noteon)
    } else {
      durations <- result$dur
    }

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
    similarity <- get_similarity(stimuli, stimuli_length, user_melody_input, durations, stimuli_durations)
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
                    dplyr::summarise(note_precision_melody = mean(participant_precision, na.rm = TRUE)) %>% dplyr::pull(note_precision_melody)

      # cents_deviation_from_nearest_stimuli_pitch
      mean_cents_deviation_from_nearest_stimuli_pitch <- score_cents_deviation_from_nearest_stimuli_pitch(user_prod_pitches = result$note,
                                                       stimuli = stimuli, freq = result$freq)

      # mean cents deviation
      mean_cents_deviation_from_nearest_midi_pitch <- mean(abs(result$cents_deviation_from_nearest_midi_pitch), na.rm = TRUE)

      # melody dtw
      user_prod_for_dtw <- prepare_mel_trial_user_prod_for_dtw(pyin_pitch_track, result)
      stimuli_for_dtw <- prepare_mel_stimuli_for_dtw(stimuli, stimuli_durations)

      melody_dtw <- dtw::dtw(user_prod_for_dtw, stimuli_for_dtw, keep = TRUE)

    } else {
      note_precision <- NA
      mean_cents_deviation_from_nearest_stimuli_pitch <- NA
      mean_cents_deviation_from_nearest_midi_pitch <- NA
      melody_dtw <- NA
    }
    print('just before list')
    list(stimuli = stimuli,
         stimuli_durations = stimuli_durations,
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
         similarity = similarity,
         note_precision = note_precision,
         melody_dtw = melody_dtw$distance,
         melody_dtw_plot = plot_dtw_melody(stimuli, stimuli_durations, pyin_pitch_track),
         mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
         mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
         answer_meta_data = input$answer_meta_data)
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
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = floor(mean(notes)))
      }
    }

  } else if (floor_or_ceiling == "ceiling") {

    function(input, ...) {
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = ceiling(mean(notes)))
      }
    }

  } else {

    function(input, ...) {
      if(is.null(input$user_response_frequencies)) {
        list(user_response = NA)
      } else {
        freqs <- rjson::fromJSON(input$user_response_frequencies)
        notes <- tidy_freqs(freqs)
        list(user_response = round(mean(notes)))
      }
    }
  }

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
       user_rating = ifelse(is.null(input$user_rating), NA, input$user_rating))
}


get_answer_store_async_long_note <- function(...) {
  print('get_answer_store_async_long_note!')
  get_answer_store_async(scoring = "long_note", ...)
}




get_note_accuracy <- function(stimuli, user_melody_input, no_correct, no_errors) {
  # accuracy
  if (no_errors == 0 & no_correct == length(stimuli)) {
    accuracy <- 1
  } else {
    accuracy <- no_correct/length(user_melody_input)
  }

}

get_similarity <- function(stimuli, stimuli_length, user_melody_input, durations, stimuli_durations = NA) {
  # similarity
  if(length(user_melody_input) < 3 | stimuli_length < 3) {
    similarity <- NA
    #ng <- NA
  } else {

    if(is.na(stimuli_durations)) {
      stimuli_durations <- rep(0.5, stimuli_length)
    }

    similarity <- itembankr::opti3(pitch_vec1 = stimuli,
                                   dur_vec1 = stimuli_durations,
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

