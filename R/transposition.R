
key_rankings_for_inst <- function(inst, remove_atonal = TRUE) {
  if(nchar(inst) > 4) {
    inst <- instrument_list[[inst]]
  }
  if(is.null(inst)) { # i.e., allow non WJD instruments through and pretend they are piano
    inst <- "p"
  }
  res <- dplyr::filter(key_rankings, instrument == inst) %>%
    dplyr::arrange(dplyr::desc(n))
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
                  tibble::tibble(instrument = rep("p", 4),
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
  res
}

#' Sample melody in key
#'
#' @param item_bank
#' @param inst
#' @param bottom_range
#' @param top_range
#' @param difficulty
#' @param length
#'
#' @return
#' @export
#'
#' @examples
sample_melody_in_key <- function(item_bank, inst, bottom_range, top_range, difficulty, length = NULL) {


  if (difficulty == "easy") {
    key <- sample_easy_key(inst)
  } else {
    key <- sample_hard_key(inst)
  }

  key_tonality <- key$key_tonality
  key_centre <- key$key_centre
  user_span <- top_range - bottom_range

  # sample melody

  item_bank_subset <- itembankr::subset_item_bank(item_bank, tonality = key_tonality, span_max = user_span, item_length = length)

  if(nrow(item_bank_subset) == 0) {
    item_bank_subset <- itembankr::subset_item_bank(item_bank, span_max = user_span, item_length = length)
  }

  if(nrow(item_bank_subset) == 0) {
    item_bank_subset <- itembankr::subset_item_bank(item_bank, item_length = length)
  }
  # failure for major, span == 24, length = 15

  found_melody <- FALSE
  count <- 0

  while(!found_melody) {

    count <- count + 1

    meta_data <- item_bank_subset %>% dplyr::slice_sample(n = 1)

    rel_mel <- meta_data$melody
    # now put it in a key
    key_centres <- itembankr::pitch_class_to_midi_notes(key_centre)

    key_centres_in_range <- key_centres[key_centres >= bottom_range & key_centres <= top_range]

    # first try it with the first note as being the key centre
    abs_mel <- itembankr::rel_to_abs_mel(itembankr::str_mel_to_vector(rel_mel, ","), start_note = key_centres_in_range[1])

    # check key
    mel_key <- get_implicit_harmonies(abs_mel)
    mel_key_centre <- unlist(strsplit(mel_key$key, "-"))[[1]]
    # how far away is it from being the correct tonal centre?

    dist <- itembankr::pitch_class_to_numeric_pitch_class(key_centre) - itembankr::pitch_class_to_numeric_pitch_class(mel_key_centre)

    if (dist != 0) {
      # then must transpose
      abs_mel <- abs_mel + dist
    }

    # check all notes in range
    if(check_all_notes_in_range(abs_mel, bottom_range, top_range)) {
      # in range
      found_melody <- TRUE
      meta_data$abs_melody <- paste0(abs_mel, collapse = ",")
      return(meta_data)
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
          meta_data$abs_melody <- paste0(abs_mel_down, collapse = ",")
          return(meta_data)
        }
        else {
          found_melody <- TRUE
          meta_data$abs_melody <- paste0(abs_mel_up, collapse = ",")
          return(meta_data)
        }
      }
      else if (check_all_notes_in_range(abs_mel_up, bottom_range, top_range) & !check_all_notes_in_range(abs_mel_down, bottom_range, top_range)) {
        found_melody <- TRUE
        # only octave up in range, return that')
        meta_data$abs_melody <- paste0(abs_mel_up, collapse = ",")
        return(meta_data)
      }
      else if (!check_all_notes_in_range(abs_mel_up, bottom_range, top_range) & check_all_notes_in_range(abs_mel_down, bottom_range, top_range)) {
        found_melody <- TRUE
        # only octave down in range, return that
        meta_data$abs_melody <- paste0(abs_mel_down, collapse = ",")
        return(meta_data)
      }
      else {
        if(count > 10) {
          meta_data$abs_melody <- paste0(abs_mel, collapse = ",")
          return(meta_data)
        }
        found_melody <- FALSE
        # neither is in range, try a new melody
        # try again
      }
    }
  } # end while

}

sample_melody_in_easy_key <- function(item_bank, inst, bottom_range, top_range) {
  sample_melody_in_key(item_bank = item_bank, inst = inst, bottom_range = bottom_range, top_range = top_range, difficulty = "easy")
}

sample_melody_in_hard_key <- function(item_bank, inst, bottom_range, top_range) {
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


leave_relative <- function(rel_melody, range = NULL, bottom_range = NULL, top_range = NULL, transpose = NULL) {
  rel_melody
}






## tests


# d <- WJD::WJD("main")
# item_bank_subset <- itembankr::subset_item_bank(WJD::WJD("main"),
#                                                 tonality = "major",
#                                                 span_max = 24, item_length = 11L)
# dd <- sample_melody_in_key(item_bank = WJD::WJD("phrases"),
#                      inst = "Piano",
#                      bottom_range = 48, top_range = 72,
#                      difficulty =  "easy", length = 3)


# sample_keys_by_difficulty("Piano", n_easy = 4, n_hard = 4)

#rel_to_abs_mel_mean_centred(Berkowitz::Berkowitz[1000, "melody"], 40, 65, TRUE)

# sampled_keys <- sample_keys_by_difficulty("Alto Saxophone", 10, 10)

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



#test_sub <- itembankr::subset_item_bank(WJD::WJD("phrases"), N_range = c(3, NULL))
#test_sub[1000, "melody"]



# key_difficulty("C-maj", "Tenor Saxophone")
# key_difficulty("Ab-min", "Tenor Saxophone")
#
# hard_keys_for_inst("Piano")


###

# sample_melody_in_easy_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)
# sample_melody_in_hard_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89)

# da1 <- sample_melody_in_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "easy", length = 15)
# da2 <- sample_melody_in_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "hard", length = 15)
#
# trial_char <- get_trial_characteristics(trial_df = pra, trial_no = 20)
#
# da3 <- sample_melody_in_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = trial_char$difficulty, length = trial_char$melody_length)
# da4 <- sample_melody_in_key(inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = trial_char$difficulty, length = trial_char$melody_length)




# test for perf:

# Normal WJD:
# system.time({ da1 <- sample_melody_in_key(WJD::WJD("ngram"), inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "easy", length = 15) })

# dbplyr:
# con <- musicassessr::connect_to_db()
# dbplyr_WJD <- dplyr::tbl(con, "WJD_ngram")
# system.time({ da2 <- sample_melody_in_key(dbplyr_WJD, inst = "Alto Saxophone", bottom_range = 58, top_range = 89, difficulty = "easy", length = 15) })

