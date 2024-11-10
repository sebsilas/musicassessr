

#' Item sampler (stratified sampling)
#'
#' @param item_bank
#' @param no_items
#' @param replace
#' @param shuffle
#' @param version
#'
#' @return
#' @export
#'
#' @examples
item_sampler <- function(item_bank, no_items, replace = FALSE, shuffle = TRUE, version = c("1", "2")) {

  version <- match.arg(version)

  if(version == "1") {
    res <- item_sampler_v1(item_bank, no_items, replace, shuffle)
  } else if(version == "2") {
    res <- item_sampler_v2(item_bank, no_items, replace, shuffle)
  } else {
    stop("Not a valid version")
  }

  return(res)
}


#' Item sampler materialized view
#'
#' @param db_con
#' @param no_items
#' @param table
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
item_sampler_materialized_view <- function(db_con,
                                           no_items,
                                           table = "Berkowitz_ngram_n_view",
                                           shuffle = TRUE) {

  stopifnot(
    DBI::dbIsValid(db_con),
    is.scalar.numeric(no_items),
    is.scalar.character(table),
    is.scalar.logical(shuffle)
  )


  max_val <- as.numeric(DBI::dbGetQuery(db_con, paste0("SELECT MAX (row_number) FROM ", table) )$max)

  rnd <- sample(1:max_val, size = 1)

  sampled_data <- DBI::dbGetQuery(db_con, paste0("SELECT * FROM  ", table, " WHERE row_number < ", rnd, " LIMIT ", no_items))

  # Shuffle the row order
  if(shuffle) {
    sampled_data <- sampled_data[sample(1:nrow(sampled_data)), ]
  }

  sampled_data

}

# db_con <- musicassessrdb::musicassessr_con()
# t <- item_sampler_materialized_view(db_con, 20) %>% dplyr::arrange(N)
# t <- item_sampler_materialized_view(db_con, 20)

#' Item sampler (stratified sampling) v2
#'
#' @param item_bank
#' @param no_items
#' @param replace
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
item_sampler_v2 <- function(item_bank, no_items, replace = FALSE, shuffle = TRUE) {

  stopifnot(
    is(item_bank, "tbl"),
    is.scalar.numeric(no_items),
    is.scalar.logical(replace)
  )

  # Calculate the number of unique values of N directly in SQL
  no_Ns <- item_bank %>% dplyr::summarize(no_Ns = n_distinct(N)) %>% dplyr::pull(no_Ns)
  proportion <- no_items / no_Ns

  # Sampling with index optimization and SQL-friendly methods
  sampled_data <- item_bank %>%
    dplyr::group_by(N) %>%
    dplyr::slice_sample(n = ceiling(proportion), .preserve = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::slice_sample(n = no_items) %>%
    dplyr::mutate(trial_no = dplyr::row_number()) %>%
    dplyr::collect()


  # Shuffle the row order
  if(shuffle) {
    sampled_data <- sampled_data[sample(1:nrow(sampled_data)), ]
  }

  sampled_data
}



#' Item sampler (stratified sampling)
#'
#' @param item_bank
#' @param no_items
#' @param replace
#' @param shuffle
#'
#' @return
#' @export
#'
#' @examples
item_sampler_v1 <- function(item_bank, no_items, replace = FALSE, shuffle = TRUE) {

  stopifnot(
    tibble::is_tibble(item_bank),
    is.scalar.numeric(no_items),
    is.scalar.logical(replace)
  )

  item_bank <- item_bank %>%
    dplyr::arrange(N)

  # what values are there?
  N_values <- unique(item_bank$N)
  no_of_Ns <- length(N_values)
  # given the no. of items, how many of each N will we need? let's count

  idxes <- rep(1:no_of_Ns, ceiling(no_items/no_of_Ns))
  count <- 1
  N_list <- c()

  while(count < no_items+1) {
    N_list <- c(N_list, N_values[idxes[count]])
    count <- count + 1
  }

  tabl <- as.data.frame(table(N_list))

  sample_dat <- apply(tabl, MARGIN = 1, function(x) {
    dat_subset <- item_bank[item_bank$N == as.integer(x["N_list"]), ]
    sample_i <- sample(1:nrow(dat_subset), x["Freq"], replace = replace)
    sampl <- dat_subset[sample_i, ]
  })

  res <- dplyr::bind_rows(sample_dat)

  if(shuffle) {
    res <- res[sample(1:nrow(res)), ]
  }

  res$trial_no <- 1:nrow(res)
  res
}


sample_item_characteristics <- function(var_name, item_characteristics_sampler_function, item_characteristics_pars) {
  psychTestR::code_block(function(state, ...) {

    var_name <- paste0(var_name, "_trial_characteristics")

    logging::loginfo("Calling sample_item_characteristics function with var_name: %s", var_name)

    item_chars <- item_characteristics_sampler_function(pars = item_characteristics_pars)

    psychTestR::set_global(var_name, item_chars, state)
  })
}

remove_duplicates_and_resample <- function(df, item_bank) {

  # resample until there are no more duplicates in df
  # but make sure the same N as the removed duplicate

  while(any(duplicated(df)) == TRUE) {

    duplicates <- df[duplicated(df), ]

    # remove duplicates
    df <- df[!duplicated(df), ]

    # resample
    for (n in duplicates$N) {
      N.subset <- df[df[, "N"] == n, ]
      rand.samp.i <- sample(1:nrow(N.subset), 1)
      logging::logdebug("df %s", df)
      logging::logdebug("item_bank[rand.samp.i, ] %s", item_bank[rand.samp.i, ])
      df <- rbind(df, item_bank[rand.samp.i, ])
    }

  }

  # order
  df <- df[order(df$N), ]

  df

}


item_sampler_by_N_list <- function(item_bank, no_items) {

  # take a sample to grab the shape
  sample <- item_bank[1, ]

  for (i in names(no_items)) {
    N.subset <- item_bank[item_bank[, "N"] == i, ]
    rand.samp.i <- sample(1:nrow(N.subset), as.numeric(no_items[[i]]), replace = FALSE)
    rand.samp <- N.subset[rand.samp.i, ]
    logging::logdebug("sample %s", sample)
    logging::logdebug("rand.samp %s", rand.samp)
    sample <- rbind(sample, rand.samp)
  }

  # pop off the first sample that was taken for a shape
  sample <- sample[2:nrow(sample), ]

  # remove duplicates
  #sample <- remove_duplicates_and_resample(sample, item_bank)

}



list_of_Ns_to_vector <- function(list_of_Ns) {
  # take in named list and convert it to a single vector

  res <- lapply(seq_along(list_of_Ns), function(x) rep(names(list_of_Ns)[[x]], list_of_Ns[[x]]))

  res <- as.numeric(unlist(res))
  res
}



item_sampler_simple <- function(item_bank, no_items) {
  # a simple item sampler for a list of files
  # optionally, specify a list of note numbers (N) to cap each file playback at

  # get length of item bank
  if (is.data.frame(item_bank)) {
    item_bank_length <- nrow(item_bank)
  } else {
    item_bank_length <- length(item_bank)
  }


  if(class(no_items) == "list" & is.data.frame(item_bank)) {
    stop('Item bank must be in list format e.g MIDI file list')
  }


  if(is.list(no_items)) {

    vectorNs <- list_of_Ns_to_vector(no_items)

    if(length(vectorNs) > item_bank_length) {
      stop('The number of items requested is longer than the item bank length!')
    }
    sample.temp <- sample(item_bank, length(vectorNs))
    sample <- as.list(as.numeric(paste(unlist(vectorNs))))
    names(sample) <- sample.temp
  }

  else {
    if(no_items > item_bank_length) {
      stop('The number of items requested is longer than the item bank length!')
    }

    sample <- sample(item_bank, no_items)
  }

  sample
}


item_sampler_rds <- function(item_bank, no_items) {

  if(is.list(no_items) == TRUE) {

    vectorNs <- list_of_Ns_to_vector(no_items)

    if(length(vectorNs) > length(item_bank)) {
      stop('The number of items requested is longer than the item bank length!')
    }
    else {
      sample <- sample(item_bank, length(vectorNs))
      sample <- lapply(seq_along(vectorNs), function(x) sample[[x]][1:vectorNs[x]])
    }
  }

  else {

  }
  sample
}


#' Sample from the user's range (determined at test time)
#'
#' @param no_to_sample
#'
#' @return
#' @export
#'
#' @examples
sample_from_user_range <- function(no_to_sample) {
  psychTestR::code_block(function(state, ...) {
    bottom_range <- psychTestR::get_global("bottom_range", state)
    top_range <- psychTestR::get_global("top_range", state)
    range <- bottom_range:top_range
    if(length(range) < no_to_sample) {
      sample <- sample(range, no_to_sample, replace = TRUE)
    } else {
      sample <- sample(range, no_to_sample)
    }

    psychTestR::set_global("user_range_sample", sample, state)
  })
}


#' Sample arrhythmic
#'
#' @param item_bank
#' @param num_items_arrhythmic
#' @param id
#'
#' @return
#' @export
#'
#' @examples
sample_arrhythmic <- function(item_bank, num_items_arrhythmic, id = "arrhythmic_melody") {
  psychTestR::code_block(function(state, ...) {
    span <- psychTestR::get_global("span", state)
    span_warning(span)

    if(is.null(span) | span < 10) {
      span <- 10
    }
    # sample arrhythmic
    arrhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank, span_max = span)
    if(nrow(arrhythmic_item_bank_subset) <= 1) {
      arrhythmic_item_bank_subset <- item_bank
    }
    if(nrow(arrhythmic_item_bank_subset) < num_items_arrhythmic) {
      arrhythmic_sample <- item_sampler(arrhythmic_item_bank_subset, num_items_arrhythmic, replace = TRUE)
    } else {
      arrhythmic_sample <- item_sampler(arrhythmic_item_bank_subset, num_items_arrhythmic)
    }
    psychTestR::set_global(id, arrhythmic_sample, state)
  })
}

#' Sample rhythmic
#'
#' @param item_bank
#' @param num_items_rhythmic
#' @param id
#'
#' @return
#' @export
#'
#' @examples
sample_rhythmic <- function(item_bank, num_items_rhythmic, id = "rhythmic_melody") {
  psychTestR::code_block(function(state, ...) {

    span <- psychTestR::get_global("span", state)

    span_warning(span)
    if(is.null(span) | span < 10) {
      span <- 10
    }
    # Sample rhythmic
    rhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank, span_max = span)
    if(nrow(rhythmic_item_bank_subset) <= 1) {
      rhythmic_item_bank_subset <- item_bank
    }

    rhythmic_sample <- item_sampler(rhythmic_item_bank_subset, num_items_rhythmic)

    if(nrow(rhythmic_sample) < num_items_rhythmic) {
      rhythmic_sample <- item_sampler(rhythmic_item_bank_subset, num_items_rhythmic, replace = TRUE)
    } else {
      rhythmic_sample <- item_sampler(rhythmic_item_bank_subset, num_items_rhythmic)
    }

    psychTestR::set_global(id, rhythmic_sample, state)
  })
}


#' Randomly sample num_items per dataframe
#'
#' @param item_bank
#' @param num_items
#'
#' @return
#' @export
#'
#' @examples
sample_random <- function(item_bank, num_items) {
  item_bank %>% dplyr::slice_sample(n = num_items)
}

span_warning <- function(span) {
  if(is.null(span)) {
    warning("There was no user span set. Do you need a user range page or fake_range before sampling?")
  }
}




#' Sample review
#'
#' @param num_review_items
#' @param id
#' @param rhythmic
#'
#' @return
#' @export
#'
#' @examples
sample_review <- function(num_review_items, id = "arrhythmic_melody_review", rhythmic = FALSE) {


  psychTestR::code_block(function(state, ...) {

    logging::loginfo('Sample review with id %s', id)
    logging::loginfo("num_review_items: %s", num_review_items)
    logging::logwarn("NB: this connects to the DB and should be deprecated for new select_items API approach ASAP")
    logging::logwarn("Sampling %s review trials.", num_review_items)

    # NB: Leave this debugging in in case this happens again

    cat(file=stderr(), "num_review_items", num_review_items, "\n")
    cat(file=stderr(), "rhythmic", rhythmic, "\n")

    # Sample arrhythmic
    review_sample <- musicassessrdb::get_review_trials(num_review_items, state, rhythmic)

    psychTestR::set_global(id, review_sample, state)
  })

}

get_review_trials2 <- function (no_reviews, state, rhythmic = FALSE) {

  db_con <- musicassessrdb::musicassessr_con()
  user_id <- psychTestR::get_global("user_id", state)
  current_test_id <- psychTestR::get_global("test_id", state)

  cat(file=stderr(), "user_id", user_id, "\n")
  cat(file=stderr(), "current_test_id", current_test_id, "\n")
  cat(file=stderr(), "no_reviews", no_reviews, "\n")


  user_trials <- compile_item_trials2(db_con, current_test_id, user_id = user_id)

  cat(file=stderr(), "after compile_item_trials2", "\n")
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (rhythmic) {
    logging::loginfo("Filtering to use only previously rhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(rhythmic)
  }
  else {
    logging::loginfo("Filtering to use only previously arrhythmic trials")
    user_trials <- user_trials %>% dplyr::filter(!rhythmic)
  }
  user_trials <- user_trials %>% dplyr::select(stimulus_abs_melody,
                                               stimulus_durations, item_id, rhythmic) %>% dplyr::filter(!is.na(stimulus_abs_melody) |
                                                                                                          !is.na(stimulus_durations)) %>% dplyr::slice_sample(n = no_reviews) %>%
    dplyr::collect() %>% dplyr::rowwise() %>% dplyr::mutate(melody = paste0(diff(itembankr::str_mel_to_vector(stimulus_abs_melody)),
                                                                            collapse = ",")) %>% dplyr::ungroup() %>% dplyr::rename(abs_melody = stimulus_abs_melody,
                                                                                                                                    durations = stimulus_durations) %>% dplyr::mutate(melody_no = dplyr::row_number())
  DBI::dbDisconnect(db_con)
  return(user_trials)
}

compile_item_trials2 <- function (db_con, current_test_id = NULL, session_id = NULL,
          user_id, join_item_banks_on = FALSE, filter_item_banks = NULL,
          add_trial_scores = FALSE, score_to_use = "opti3") {


  cat(file=stderr(), "compile_item_trials2", "\n")

  cat(file=stderr(), "current_test_id", current_test_id, "\n")
  cat(file=stderr(), "session_id", session_id, "\n")
  cat(file=stderr(), "user_id", user_id, "\n")
  cat(file = stderr(), "DBI::dbIsValid(db_con)", DBI::dbIsValid(db_con))



  sessions <- musicassessrdb::get_table(db_con, "sessions", collect = FALSE) %>%
    dplyr::filter(user_id %in% !!user_id)

  cat(file=stderr(), "nrow(sessions)", nrow(sessions), "\n")

  trials <- musicassessrdb::get_table(db_con, "trials", collect = FALSE) %>%
    dplyr::left_join(sessions, by = "session_id")

  cat(file=stderr(), "nrow(trials)", nrow(trials), "\n")

  user_trials <- trials %>% dplyr::filter(user_id %in% !!user_id)

  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")


  if (get_nrows(user_trials) < 1L) {
    return(user_trials)
  }
  if (!is.null(current_test_id)) {
    user_trials <- user_trials %>% dplyr::filter(test_id ==
                                                   !!current_test_id)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (get_nrows(user_trials) < 1L) {
    return(user_trials)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (!is.null(session_id)) {
    user_trials <- user_trials %>%
      dplyr::filter(session_id %in% !!session_id)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (get_nrows(user_trials) < 1L) {
    return(user_trials)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (join_item_banks_on) {
    item_banks <- user_trials %>% dplyr::pull(item_id) %>%
      get_item_bank_names(db_con = db_con)
    user_trials <- left_join_on_items(db_con, item_banks,
                                      df_with_item_ids = user_trials)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (!is.null(filter_item_banks)) {
    item_banks_table <- get_table(db_con, "item_banks", collect = TRUE)
    item_bank_ids <- item_bank_name_to_id(item_banks_table,
                                          ib_name = filter_item_banks)
    user_trials <- user_trials %>% dplyr::filter(item_bank_id %in%
                                                   item_bank_ids)
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  if (add_trial_scores) {
    trial_scores <- dplyr::tbl(db_con, "scores_trial") %>%
      dplyr::filter(measure == !!score_to_use) %>% dplyr::collect()
    user_trials <- user_trials %>% dplyr::left_join(trial_scores,
                                                    by = "trial_id")
  }
  cat(file=stderr(), "nrow(user_trials)", nrow(user_trials), "\n")

  user_trials
}
