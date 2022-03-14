#' Item sampler
#'
#' @param item_bank
#'
#' @return
#' @export
#'
#' @examples
item_sampler <- function(item_bank, no_items) {

  item_bank <- item_bank %>% dplyr::arrange(N)

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
    sample_i <- sample(1:nrow(dat_subset), x["Freq"])
    sampl <- dat_subset[sample_i, ]
  })

  res <- dplyr::bind_rows(sample_dat)

  res$trial_no <- 1:nrow(res)
  res
}


sample_item_characteristics <- function(var_name, item_characteristics_sampler_function, item_characteristics_pars) {
  psychTestR::code_block(function(state, ...) {
    print('sample_item_characteristics!')
    print(item_characteristics_pars)
    item_chars <- item_characteristics_sampler_function(pars = item_characteristics_pars)
    print(item_chars)
    print('set::')
    print(var_name)
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


sample_arrhythmic <- function(item_bank, num_items_arrhythmic, id = "arrhythmic_melody") {
  psychTestR::code_block(function(state, ...) {
    span <- psychTestR::get_global("span", state)
    # sample arrhythmic
    arrhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank, span_max = span)
    arrhythmic_sample <- musicassessr::item_sampler(arrhythmic_item_bank_subset, num_items_arrhythmic)
    psychTestR::set_global(id, arrhythmic_sample, state)
  })
}

sample_rhythmic <- function(item_bank, num_items_rhythmic, id = "rhythmic_melody") {
  psychTestR::code_block(function(state, ...) {
    span <- psychTestR::get_global("span", state)
    # sample rhythmic
    rhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = item_bank, span_max = span)
    rhythmic_sample <- musicassessr::item_sampler(rhythmic_item_bank_subset, num_items_rhythmic)
    psychTestR::set_global(id, rhythmic_sample, state)
  })
}

#rhythmic_item_bank_subset <- itembankr::subset_item_bank(item_bank = itembankr::Berkowitz("main"), item_length = c(3,7))
#rhythmic_sample <- musicassessr::item_sampler(rhythmic_item_bank_subset, 6)




#pra <- item_characteristics_sampler()


#get_trial_characteristics(trial_df = pra, trial_no = 20)

# tests


# dada <- item_sampler_rds(berkowitz.rds.abs, list("3" = 2L,
#                                                   "4" = 1L))



# doda <- item_sampler_simple(item_bank = berkowitz.midi,
#                             no_items = list("3" = 4,
#                                             "4" = 3,
#                                             "5" = 2,
#                                             "6" = 2,
#                                             "7" = 2,
#                                             "8" = 2,
#                                             "9" = 2,
#                                             "10" = 2,
#                                             "11" = 2))
#
#
#
#
#


# tests::


# test duplicate
#fake.duplicate <- rbind(user.sample, user.sample[1, ], user.sample[11, ])
#out <- remove.duplicates.and.resample(fake.duplicate, berkowitz.item.bank)



#ja <- item_sampler(DTL_1000, 23)


# item_bank_sample <- item_sampler(item_bank_sub, 20)
