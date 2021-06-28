library(readxl)
library(psych)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(corrplot)
library(Hmisc)
library(pcalg)
library(readr)
library(rjson)
library(data.table)

options(scipen = 999)

# import dependencies
source("Fantastic.R") # for some reason, FANTASTIC simply will only work when sourced from the cwd
source("simile.R")
#source("general_functions.R")


# functions

abs.to.rel.mel <- function(melody) {
  # takes an absolute melody, converts it into relative melody as a string
  rel.melody <- diff(as.numeric(as.vector(unlist(strsplit(as.character(melody), "-")))))

  # repackage as string
  str.rel.mel <- paste(rel.melody, collapse=",")

}

grab.melody.info <- function(melody, col_name) {
  #  looks up the value requested from the item bank
  # e.g, use this to find the log frequency of a given melody

  # find it in berkowitz
  res <- as.numeric(unlist(berkowitz.item.bank[berkowitz.item.bank==melody, col_name]))

}

str.mel.to.vector <- function(str_mel, sep) {
  vector_mel <- as.numeric(unlist(strsplit(str_mel, sep)))
}

get_rel_freq <- function(freq_col) {
  # the column should be a column of frequency values for each item
  res <- lapply(freq_col, function(x) x/sum(freq_col))
  cat("Sanity check, this should at to 1: ", sum(unlist(res)))
  as.numeric(res)
}

get_log_freq <- function(rel_freq_col) {
  # log freq
  as.numeric(lapply(rel_freq_col, log))
}

get_stimuli_length <- function(melody_col, sep) {
  # add melody length, should be
  res <- lapply(melody_col, function(x) length(unlist(strsplit(x, sep)))+1)
  res <- as.numeric(res)
  res
}

get_interval_entropy <- function(rel_melody_col, sep) {
  # add interval entropy
  res <- lapply(rel_melody_col, function(x) compute.entropy(str.mel.to.vector(unlist(x), sep), (phr.length.limits[2]-1)))
  res  <- as.numeric(res)
}

get_tonality <- function(melody, sep) {
  # wrap the FANTASTIC functions and add in some default durations if need be
  if (!is.na(melody)) {
    pitch <- rel_to_abs_mel(str.mel.to.vector(melody, sep))
    len <- length(pitch)
    dur16 <- rep(.25, length(pitch))
    tonality.vector <- compute.tonality.vector(pitch,dur16,make.tonal.weights(maj.vector,min.vector))
    ton.results <- compute.tonal.features(tonality.vector)
  }

  else {
    ton.results <- as.data.frame(matrix(c(NA, NA, NA, NA), nrow = 1, ncol = 4))
  }

}

# add local stepwise contour
get_step_contour <- function(melody, relative = TRUE) {
  # wrap the FANTASTIC functions and add in some default durations

  if(relative) {
    melody <- rel_to_abs_mel(melody)
  }

  if (!is.na(melody)) {
    len <- length(melody)
    dur16 <- rep(.25, length(melody))
    step.contour.vector <- step.contour(melody,dur16)
    step.contour <- compute.step.cont.feat(step.contour.vector)
  }

  else {
    ton.results <- as.data.frame(matrix(c(NA, NA, NA, NA), nrow = 1, ncol = 4))
  }
}

get_duration_measures<- function(df_row) {
  # wrap the FANTASTIC functions and add in some default durations

  if (!is.na(df_row["melody"])) {

    dur16 <- rep(.25, df_row["N"])
    d.entropy <- compute.entropy(dur16,phr.length.limits[2])
    d.ratios <- round(dur16[1:length(dur16)-1]/ dur16[2:length(dur16)],2)
    d.eq.trans <- sum(sign(d.ratios[d.ratios==1])) / length(d.ratios)

    dur.results <- as.data.frame(matrix(c(d.entropy, d.eq.trans), nrow = 1, ncol = 2))
  }

  else {
    dur.results <- as.data.frame(matrix(c(NA, NA), nrow = 1, ncol = 2))
  }
}


pattern_to_int <- function(x){
  if(length(x) > 1){
    return(lapply(x, pattern_to_int))
  }
  strsplit(gsub("\\]", "", gsub("\\[", "", x)), ",") %>% unlist() %>% as.integer()
}

#input is a vector of string, of the form [i_1, i_2,...], where the i's are integer semitone intervals.
int_ngram_difficulty <- function(pattern){
  if(length(pattern) > 1){
    return(map_dfr(pattern, int_ngram_difficulty))
  }
  #browser()
  v <- pattern_to_int(pattern)
  mean_abs_int <- mean(abs(v))
  int_range <- max(abs(v))
  l <- length(v)
  r <- rle(sign(v))
  dir_change <- length(r$values) - 1
  mean_dir_change <- (length(r$values) - 1)/(l-1)
  mean_run_length <- 1 - mean(r$lengths)/l
  int_variety <- dplyr::n_distinct(v)/l
  pitch_variety <- dplyr::n_distinct(c(0, cumsum(v)))/(l+1)
  res <- tibble(value = pattern,
         mean_int_size = mean_abs_int,
         int_range = int_range,
         dir_change = dir_change,
         mean_dir_change = mean_dir_change,
         int_variety = int_variety,
         pitch_variety = pitch_variety,
         mean_run_length = mean_run_length)
  res$mean_dir_change[res$mean_dir_change == "NaN"] <- NA
  res
}



int_to_pattern <- function (v) {
  paste0('[', paste0(v, collapse = ","), ']')
}


hist_corpus <- function(corpus) {
  ggplot(gather(corpus[, names(corpus)[!names(corpus) %in% c("melody", "mode", "ks_tonality")]]), aes(value)) +
    geom_histogram() +
    facet_wrap(~key, scales = 'free_x')

}
#int_ngram_difficulty('[-5, 1, 2, 3]')
#int_ngram_difficulty(int_to_pattern(c(-5, 1, 2, 3)))

get_melody_features <- function(df, mel_sep = "-", durationMeasures = FALSE) {

  # the in df should contain the following columns:
  # melody: a relative melody e.g 2, 2, -1, 3
  # freq: a count of the number of occurences of the dataset from which it came

  df$rel_freq <- get_rel_freq(df$freq)

  df$log_freq <- get_log_freq(df$rel_freq)

  df$N <- get_stimuli_length(df$melody, mel_sep)

  # tonality
  tonality <- bind_rows(lapply(df$melody, get_tonality, mel_sep))
  df <- cbind(df, tonality)

  # Krumhansl-Schmuckler algorithm
  ks_tonality <- lapply(df$melody, function(x) get_implicit_harmonies(rel_to_abs_mel(str.mel.to.vector(x, sep = mel_sep)))$type )
  df$ks_tonality <- unlist(ks_tonality)

  # step contour
  step_contour_df <- bind_rows(lapply(df$melody, function(x) get_step_contour(str.mel.to.vector(x, mel_sep), TRUE)))
  step_contour_df <- step_contour_df[, c("step.cont.glob.var", "step.cont.glob.dir", "step.cont.loc.var")]
  df <- bind_cols(df, step_contour_df)

  if (durationMeasures == TRUE) {
    # duration measures
    duration_df <- bind_rows(apply(df, MARGIN = 1, get_duration_measures))
    names(duration_df) <- c("d.entropy", "d.eq.trans")
    df <- bind_cols(df, duration_df)
  }

  # difficulty measures from Klaus
  difficulty_measures <- bind_rows(lapply(df$melody, function(x) int_ngram_difficulty(int_to_pattern(x))))
  df <- cbind(df, difficulty_measures)

  # calculate melody spans, to make sure melodies can be presented within a user's range
  span <- lapply(df$melody, function(x) sum(abs(range(rel_to_abs_mel(str.mel.to.vector(x, ","), start_note = 1)))) )
  df$span <- unlist(span)

  # remove NA columns with all NAs
  #all_na <- function(x) any(!is.na(x))
  #df <- df %>% select_if(all_na)

  # round all numeric columns to two decimal places
  df <- df %>% mutate_at(vars(log_freq, step.cont.glob.dir, step.cont.glob.var,
                              step.cont.loc.var, tonal.clarity, tonal.spike, tonalness, mean_int_size,
                              int_range, dir_change,mean_dir_change, int_variety, pitch_variety, mean_run_length), funs(round(., 2)))
  print(hist_corpus(df))
  df
}



