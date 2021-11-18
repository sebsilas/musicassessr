
key_rankings_for_inst <- function(inst, remove_atonal = TRUE) {
  if(nchar(inst) > 4) {
    inst <- instrument_list[[inst]]
  }
  res <- dplyr::filter(key_rankings, instrument == inst) %>% dplyr::arrange(dplyr::desc(n))
  if (remove_atonal) {
    res <- res %>% dplyr::filter(key != "")
  }
  res
}


easy_keys_for_inst <- function(instrument_name) {
  ranking <- key_rankings_for_inst(instrument_name)
  easy_keys <- ranking[1:floor(nrow(ranking)/2), ]
  warning('Manually adding easy keys for Piano: C, F,  G, D')
  if(instrument_name == "Piano") {
    easy_keys <- rbind(easy_keys,
                  data.frame(instrument = rep("p", 4),
                            key = c("C-maj", "F-maj", "G-maj", "D-maj"),
                            n = rep(0, 4),
                            key_centre = c("C", "F", "G", "D"),
                            key_tonality = rep("major", 4)))
  }
  easy_keys
}


hard_keys_for_inst <- function(instrument_name) {
  # get the easy keys and just make sure that the sampled key is not in that list
  easy_keys <- easy_keys_for_inst(instrument_name)$key
  dplyr::filter(keys_table, !key %in% easy_keys)
}


sample_from_df <- function(df, no_to_sample, replacement = FALSE) {
  n <- sample(x = nrow(df), size = no_to_sample, replace = replacement)
  df[n, ]
}

sample_easy_key <- function(inst_name, no_to_sample = 1, replacement = TRUE) {
  res <- sample_from_df(easy_keys_for_inst(inst_name), no_to_sample, replacement = replacement)
  res$difficulty <- rep("easy", no_to_sample)
  res
}


sample_hard_key <- function(inst_name, no_to_sample = 1, replacement = TRUE) {
  res <- sample_from_df(hard_keys_for_inst(inst_name), no_to_sample, replace = replacement)
  res$difficulty <- rep("hard", no_to_sample)
}

sample_melody_in_key <- function(item_bank = WJD, inst, bottom_range, top_range, difficulty, length = NULL) {

  if (difficulty == "easy") {
    key <- sample_easy_key(inst)
  } else {
    key <- sample_hard_key(inst)
  }

  key_tonality <- key$key_tonality
  key_centre <- key$key_centre
  user_span <- top_range - bottom_range

  # sample melody

  item_bank_subset <- subset_item_bank(item_bank, tonality = key_tonality, span_max = user_span, item_length = length)

  found_melody <- FALSE

  while(found_melody == FALSE) {
    i <- sample(1:nrow(item_bank_subset), 1)
    meta_data <- item_bank_subset[i, ]
    rel_mel <- meta_data$melody

    # now put it in a key
    key_centres <- pitch.class.to.midi.notes(key_centre)

    key_centres_in_range <- key_centres[key_centres > bottom_range & key_centres < top_range]

    # first try it with the first note as being the key centre
    abs_mel <- itembankr::rel_to_abs_mel(str_mel_to_vector(rel_mel, ","), start_note = key_centres_in_range[1])
    # check key
    mel_key <- get_implicit_harmonies(abs_mel)
    mel_key_centre <- unlist(strsplit(mel_key$key, "-"))[[1]]
    # how far away is it from being the correct tonal centre?
    dist <- pitch.class.to.numeric.pitch.class(key_centre) - pitch.class.to.numeric.pitch.class(mel_key_centre)

    if (dist != 0) {
      # then must transpose
      abs_mel <- abs_mel + dist
    }

    # check all notes in range
    if(check_all_notes_in_range(abs_mel, bottom_range, top_range)) {
      # in range
      found_melody <- TRUE
      return(list(abs_mel, meta_data))
    }
    else {
      # not in range!

      # try octave either side
      abs_mel_up <- abs_mel + 12
      abs_mel_down <- abs_mel - 12
      if(check_all_notes_in_range(abs_mel_up, bottom_range, top_range) & check_all_notes_in_range(abs_mel_down, bottom_range, top_range)) {
        # both in range, randomly select one
        snap <- sample(1:2, 1)
        if(snap == 1) {
          found_melody <- TRUE
          return(list(abs_mel_down, meta_data))
        }
        else {
          found_melody <- TRUE
          return(list(abs_mel_up, meta_data))
        }
      }
      else if (check_all_notes_in_range(abs_mel_up, bottom_range, top_range) & !check_all_notes_in_range(abs_mel_down, bottom_range, top_range)) {
        found_melody <- TRUE
        # only octave up in range, return that')
        return(list(abs_mel_up, meta_data))
      }
      else if (!check_all_notes_in_range(abs_mel_up, bottom_range, top_range) & check_all_notes_in_range(abs_mel_down, bottom_range, top_range)) {
        found_melody <- TRUE
        # only octave down in range, return that
        return(list(abs_mel_down, meta_data))
      }
      else {
        found_melody <- FALSE
        # neither is in range, try a new melody
        # try again
      }
    }
  } # end while

}



sample_melody_in_easy_key <- function(item_bank = WJD, inst, bottom_range, top_range) {
  sample_melody_in_key(item_bank = item_bank, inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = "easy")
}

sample_melody_in_hard_key <- function(item_bank = WJD, inst, bottom_range, top_range) {
  sample_melody_in_key(item_bank = item_bank, inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = "hard")
}


key_difficulty <- function(key, inst) {
  # given key and instrument, is the key considered easy or difficult
  if(key %in% hard_keys_for_inst(inst)$key) {
    return("hard")
  }
  else {
    return("easy")
  }
}

check_all_notes_in_range <- function(abs_mel, bottom_range, top_range) {
  range <- bottom_range:top_range
  all(abs_mel %in% range)
}


given_range_what_keys_can_melody_go_in <- function(melody, bottom_range = 48, top_range = 79, inst) {
  # check, starting from every note in the range, if the whole melody fits in the range,
  # if it fits, then list the key it is possible in, and give the starting note

  range <- bottom_range:top_range

  res <- data.frame(start_note = bottom_range:top_range,
                    melody = melody
                    )

  res <- res %>%
          dplyr::rowwise() %>%
          dplyr::mutate(abs_melody = paste0(itembankr::rel_to_abs_mel(rel_mel = str_mel_to_vector(melody, ","), start_note = start_note), collapse = ","),
                 in_range = all(str_mel_to_vector(abs_melody, ",") %in% range)
                 ) %>%
                dplyr::filter(in_range == TRUE)

  idxes <- sample(1:nrow(res), 10)

  for (i in idxes) {
    abs_melody <- res[i, "melody"]
    key <- get_implicit_harmonies(str_mel_to_vector(abs_melody, ","))$key
  }

  # mutate(key = get_implicit_harmonies(str_mel_to_vector(abs_melody, ","))$key) %>%
  #   rowwise %>%
  #     mutate(difficulty = key_difficulty(key, inst))

  res
}


produce_stimuli_in_range_and_key <- function(rel_melody, bottom_range = 21, top_range = 108, key) {
  # given some melodies in relative format, and a user range, produce random transpositions which fit in that range
  rel_melody <- str_mel_to_vector(rel_melody, sep = ",")
  dummy_abs_mel <- itembankr::rel_to_abs_mel(rel_melody, start_note = 1)
  mel_range <- range(dummy_abs_mel)
  span <- sum(abs(mel_range))

  gamut <- bottom_range:top_range
  gamut_clipped <- (bottom_range+span):(top_range-span)
  random_abs_mel <- 200:210  # just instantiate something out of range
  current_key <- "fail" # and same for the current key

  # some melodies aren't transposable into a given key, given the user's range, so we need a way out
  count <- 0

  while(any(!random_abs_mel %in% gamut) | current_key != key) {
    # resample until a melody is found that sits in the range
    # and is the correct tonality
    random_abs_mel_start_note <- sample(gamut, 1)
    random_abs_mel <- itembankr::rel_to_abs_mel(rel_melody, start_note = random_abs_mel_start_note)
    current_key <- get_implicit_harmonies(random_abs_mel)$key
    count <- count + 1

    if(span > top_range - bottom_range) {
      print('The span of the stimuli is greater than the range of the instrument. It is not possible to play on this instrument.')
      break
    }

    if (count == 144) {
      random_abs_mel <- "error"
      print("error!")
      break
    }
  }

  random_abs_mel
}


sample_keys_by_difficulty <- function(inst, n_easy, n_hard) {
  easy <- sample_easy_key(inst, no_to_sample = n_easy)
  hard <- sample_hard_key(inst, no_to_sample = n_hard)
  rbind(easy, hard)
}


mean_of_stimuli <- function(rel_melody) {
  if(class(rel_melody) == "character") {
    rel_melody <- itembankr::str_mel_to_vector(rel_melody, ",")
  }
  res <- round(mean(itembankr::rel_to_abs_mel(0, rel_melody)))
}

plot_mean_centred_to_range <- function(stimuli_centred_to_user_mean, user_mean_corrected_to_stimuli, user_mean_note, min_range, max_range) {

  data <- data.frame("x"=1:length(stimuli_centred_to_user_mean), "y"=stimuli_centred_to_user_mean)
  # Plot
  print(plot_gg <- data %>%
    ggplot2::ggplot( ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = user_mean_note, color = "blue") +
    ggplot2::geom_hline(yintercept = user_mean_corrected_to_stimuli, color = "red", linetype="dotted") +
    ggplot2::geom_hline(yintercept = min_range, color = "green") +
    ggplot2::geom_hline(yintercept = max_range, color = "green"))
}



#' Convert a melody from relative to absolute form by centering the mean of the stimuli on mean of the user's range.
#'
#' @param rel_melody
#' @param bottom_range
#' @param top_range
#' @param plot
#' @param range
#' @param transpose
#'
#' @return
#' @export
#'
#' @examples
rel_to_abs_mel_mean_centred <- function(rel_melody, bottom_range, top_range, plot = FALSE, range = NULL, transpose = NULL) {
  # produce a melody which is centered on the user's range.
  # NB: the "mean stimuli note" could/should be sampled from around the user's mean range i.e +/- 3 semitones

  if(class(rel_melody) == "character") {
    rel_melody <- itembankr::str_mel_to_vector(rel_melody, ",")
  }

  mean_of_stimuli <- mean_of_stimuli(rel_melody)


  user_mean_note <- mean(bottom_range:top_range)

  user_mean_corrected_to_stimuli <- round(user_mean_note - mean_of_stimuli)

  stimuli_centred_to_user_mean <- itembankr::rel_to_abs_mel(rel_melody, user_mean_corrected_to_stimuli)
  # the rel melody should be the same when converted back
  #print(diff(stimuli_centred_to_user_mean))
  #print(rel_melody)

  if(plot) {
    plot_mean_centred_to_range(stimuli_centred_to_user_mean, user_mean_corrected_to_stimuli, user_mean_note, bottom_range, top_range)
  }


  return(stimuli_centred_to_user_mean)

}


leave_arrhythmic <- function(rel_melody, bottom_range = NULL, top_range = NULL) {
  rel_melody
}



# sample_keys_by_difficulty("Piano", n_easy = 4, n_hard = 4)



## tests

#rel_to_abs_mel_mean_centred(itembankr::Berkowitz[1000, "melody"], 40, 65, TRUE)


# ra.2 <- given_range_what_keys_can_melody_go_in(melody = test_sub[1000, "melody"],
#                                              bottom_range = 48, top_range = 79, "Piano")
#

# sampled_keys <- sample_keys_by_difficulty("Alto Saxophone", 10, 10)

# rra3.2 <- given_range_what_keys_can_melody_go_in(melody = test_sub[1001, "melody"],
#                                               bottom_range = 48, top_range = 79, "Tenor Saxophone")
#
#
# ra4.2 <- given_range_what_keys_can_melody_go_in(melody = test_sub[1002, "melody"],
#                                               bottom_range = 48, top_range = 79, "Alto Saxophone")
#
#
# ra5.2 <- given_range_what_keys_can_melody_go_in(melody = test_sub[1102, "melody"],
#                                               bottom_range = 48, top_range = 79, "Piano")
#
#
# ra6.2 <- given_range_what_keys_can_melody_go_in(melody = test_sub[100, "melody"],
#                                               bottom_range = 48, top_range = 79, "Tenor Saxophone")
#
#
# ra7.2 <- given_range_what_keys_can_melody_go_in(melody = "4,5,-5,-5,3,7,-7,7,-6,6,-2,-1,1",
#                                               bottom_range = 48, top_range = 79, "Piano")
#

#
#
# sample_melody_in_hard_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)
# sample_melody_in_easy_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)
#



# sample_easy_key("Piano")
# key_rankings_for_inst("Piano")
# tt <- hard_keys_for_inst("Piano")
# tt <- hard_keys_for_inst("Alto Saxophone")
# tt <- hard_keys_for_inst("Tenor Saxophone")
#
# key_rankings_for_inst("Alto Saxophone")
# easy_keys_for_inst("Alto Saxophone")
# easy_keys_for_inst("Piano")
#
# hard_keys_for_inst("Alto Saxophone")
#
# key_rankings_for_inst("Clarinet")
# easy_keys_for_inst("Clarinet")
# hard_keys_for_inst("Clarinet")
#
# sample_easy_key("Alto Saxophone")
#
# sample_melody_in_easy_key("Tenor Saxophone",  44, 76)

# key_rankings_for_inst("Alto Saxophone")
#
# alto_range <- 58:89
# tenor_range <- 44:76
# sample_melody_in_easy_key("Alto Saxophone", 58, 89)
# sample_melody_in_hard_key("Alto Saxophone", 58, 89)
# sample_melody_in_hard_key("Tenor Saxophone", 44, 76)
# sample_melody_in_easy_key("Tenor Saxophone",  44, 76)
# hi <- sample_melody_in_easy_key("Piano", 48, 79)
#
#
# ts_hard <- hard_keys_for_inst("Tenor Saxophone")
# ts_easy <- easy_keys_for_inst("Tenor Saxophone")
# easy_keys_for_inst("Tenor Saxophone")
#
# sample_melody_in_easy_key("Piano", 48, 79)


#sample_melody_in_easy_key("Piano", 48, 79)



#test_sub <- subset_item_bank(WJD, N_range = c(3, NULL))
#test_sub[1000, "melody"]



# key_difficulty("C-maj", "Tenor Saxophone")
# key_difficulty("Ab-min", "Tenor Saxophone")
#
# hard_keys_for_inst("Piano")


###

# sample_melody_in_easy_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)
# sample_melody_in_hard_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)
#





############


#
# da1 <- sample_melody_in_key2(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "easy", length = 15)
# da2 <- sample_melody_in_key2(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "hard", length = 15)
#
# trial_char <- get_trial_characteristics(trial_df = pra, trial_no = 20)
#
# da3 <- sample_melody_in_key2(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = trial_char$difficulty, length = trial_char$melody_length)
# da4 <- sample_melody_in_key2(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = trial_char$difficulty, length = trial_char$melody_length)
