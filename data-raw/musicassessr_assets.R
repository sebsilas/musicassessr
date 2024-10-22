
library(tidyverse)
library(readxl)
library(psych)

load_all()

setwd('~/musicassessr')

# grab WJD meta info
wjd_meta <- read.csv2('data-raw/wjd_meta.csv')
# standardise namings
wjd_meta$key[wjd_meta$key=="D#-maj"] <- "Eb-maj"
wjd_meta$key[wjd_meta$key=="C#-maj"] <- "Db-maj"
wjd_meta$key[wjd_meta$key=="F#-min"] <- "Gb-min"
wjd_meta$key[wjd_meta$key=="F#-chrom"] <- "Gb-chrom"



# produce tables of all possible keys
keys_maj <- paste0(itembankr::pc_labels_flat, '-maj')
keys_min <- paste0(itembankr::pc_labels_flat, '-min')
keys_list <- c(keys_maj, keys_min)
keys_table <- tibble::tibble(key = keys_list,
                     key_centre = c(itembankr::pc_labels_flat, itembankr::pc_labels_flat),
                     key_tonality = c(rep("major", 12), rep("minor", 12))
)

# list of instruments in the WJD
instrument_list <- as.list(levels(as.factor(wjd_meta$instrument)))

# remove guitar and vibraphone (possibility of polyphony)

instrument_list[instrument_list=="vib"] <- NULL
instrument_list[instrument_list=="g"] <- NULL


# attach long name format, to allow the grabbing of the abbreviated form
names(instrument_list) <- c("Alto Saxophone", "Bass Clarinet", "Baritone Saxophone", "Clarinet", "Cornet", "Piano",
                            "Soprano Saxophone", "Trombone", "Trumpet", "Tenor Saxophone", "C Tenor Saxophone")

# attach other (non WJD, but allowed) instruments
insts_table <- read_excel('data-raw/musical_instruments.xlsx')


uniques <- insts_table$en %in% names(instrument_list)
insts_table$en[!uniques]

# add flute.. treat it like piano by giving it the same instrument code
instrument_list$Flute <- "pno"



insts_table2 <- insts_table %>% select(key, en, it, de, lv)
insts <- insts_table %>% pull(key)


# create key rankings table
key_rankings <- group_by(wjd_meta, instrument) %>% dplyr::count(key)
key_rankings$key_centre <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][1])
key_rankings$key_tonality <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][2])
key_rankings$key_tonality[key_rankings$key_tonality == "maj"] <- "major"
key_rankings$key_tonality[key_rankings$key_tonality == "min"] <- "minor"


musicassessr_dict_df <- readxl::read_excel("data-raw/musicassessr_dict.xlsx")
musicassessr_dict_df <- rbind(musicassessr_dict_df, insts_table2)

nrow(musicassessr_dict_df)

anyNA(musicassessr_dict_df)

# make sure to get all objects

load('data-raw/Berkowitz_arrhythmic_mixed_effects_model.rda')
load('data-raw/Berkowitz_rhythmic_mixed_effects_model.rda')
load('data-raw/long_note_pca.rda')
load('data-raw/long_note_agg.rda')
load('data-raw/melody_pca2_data.rda')
load('data-raw/melody_pca2.rda')


musicassessr_dict <- musicassessr::dict(additional_dict = NULL,
                          main_dict = musicassessr_dict_df)



# test the long note predict method
predict(long_note_pca2,
        data = tibble::tibble(long_note_accuracy = 300,
                              long_note_dtw_distance = 100,
                              long_note_autocorrelation_mean = .44,
                              long_note_run_test = -23,
                              long_note_no_cpts = 6,
                              long_note_beginning_of_second_cpt = 200),
        old.data = long_note_agg %>%
          select(long_note_accuracy, long_note_dtw_distance, long_note_autocorrelation_mean,
                 long_note_run_test, long_note_no_cpts, long_note_beginning_of_second_cpt)
        # you need to pass this for standardization or you will get NaNs
        # https://stackoverflow.com/questions/27534968/dimension-reduction-using-psychprincipal-does-not-work-for-smaller-data
) %>%
  as_tibble() %>%
  dplyr::rename(pca_long_note_randomness = RC1,
                pca_long_note_accuracy = RC2,
                pca_long_note_scoop = RC3)



# Only needs to be done once (or when updates happen)
# db_con <- connect_to_db()
#
# insts_table <- insts_table %>%
#   dplyr::select(en, low_note, high_note, transpose, clef) %>%
#   dplyr::rename(instrument_name = en,
#          bottom_range = low_note,
#          top_range = high_note)
#
# db_append_to_table(db_con, table = "instruments", data = insts_table, primary_key_col = "instrument_id")
#
# DBI::dbDisconnect(db_con)


# NB, run the other file for the musicassessr dict

intervals <- list(Unison = 0L, `Minor 2nd` = 1L, `Major 2nd` = 2L, `Minor 3rd` = 3L,
                  `Major 3rd` = 4L, `Perfect Fourth` = 5L, Tritone = 6L, `Perfect 5th` = 7L,
                  `Minor 6th` = 8L, `Major 6th` = 9L, `Minor 7th` = 10L, `Major 7th` = 11L,
                  Octave = 12L)

# Internal
usethis::use_data(musicassessr_dict_df, insts, insts_table, insts_table2,
                  musicassessr_dict, intervals,
                  instrument_list, key_rankings, keys_table, long_note_pca2,
                  overwrite = TRUE, internal = TRUE)

# Not internal

use_data(musicassessr_dict,
         key_rankings, instrument_list, keys_table,
         wjd_meta, insts, insts_table, insts_table2, musicassessr_dict_df,
         lm2.2, lm3.2, melody_pca2,
         melody_pca2_data, long_note_agg,
         overwrite = TRUE)

document()

credentials::set_github_pat()

install()

