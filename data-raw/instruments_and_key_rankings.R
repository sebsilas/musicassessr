library(dplyr)
library(readxl)
library(dplyr)


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





insts_table2 <- insts_table %>% select(-c(low_note, high_note, transpose, clef))
insts <- insts_table %>% pull(key)


# create key rankings table
key_rankings <- group_by(wjd_meta, instrument) %>% dplyr::count(key)
key_rankings$key_centre <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][1])
key_rankings$key_tonality <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][2])
key_rankings$key_tonality[key_rankings$key_tonality == "maj"] <- "major"
key_rankings$key_tonality[key_rankings$key_tonality == "min"] <- "minor"

use_data(key_rankings, instrument_list, keys_table, wjd_meta, insts, insts_table, insts_table2, overwrite = TRUE)

# NB, run the other file for the musicassessr dict

usethis::use_data(musicassessr_dict_df, insts, insts_table, insts_table2,
                  instrument_list, key_rankings, keys_table, overwrite = TRUE, internal = TRUE)


