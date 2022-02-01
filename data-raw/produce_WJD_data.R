
library(tibble)

# grab WJD item_bank
WJD_item_bank_df <- read_rds("www/item_banks/WJD_item_bank.RDS")

# grab WJD meta info
wjd_meta <- read.csv2('data-raw/wjd_meta.csv')
# standardise namings
wjd_meta$key[wjd_meta$key=="D#-maj"] <- "Eb-maj"
wjd_meta$key[wjd_meta$key=="C#-maj"] <- "Db-maj"

# produce tables of all possible keys
keys_maj <- paste0(pc_labels, '-maj')
keys_min <- paste0(pc_labels, '-min')
keys_list <- c(keys_maj, keys_min)
keys_table <- tibble(key = keys_list,
                     key_centre = c(pc_labels, pc_labels),
                     key_tonality = c(rep("major", 12), rep("minor", 12))
)

# list of instruments in the WJD
instrument_list <- as.list(levels(as.factor(wjd_meta$instrument)))
# attach long name format, to allow the grabbing of the abbreviated form
names(instrument_list) <- c("Alto Saxophone", "Bass Clarinet", "Bass", "Clarinet", "Cornet", "Guitar", "Piano",
                            "Soprano Saxophone", "Trombone", "Trumpet", "Tenor Saxophone", "C Tenor Saxophone", "Vibraphone")


# create key rankings table
key_rankings <- group_by(wjd_meta, instrument) %>% dplyr::count(key)
key_rankings$key_centre <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][1])
key_rankings$key_tonality <- sapply(key_rankings$key, function(x) strsplit(x, "-")[[1]][2])
key_rankings$key_tonality[key_rankings$key_tonality == "maj"] <- "major"
key_rankings$key_tonality[key_rankings$key_tonality == "min"] <- "minor"


base::save(list = c("key_rankings", "instrument_list", "keys_table", "wjd_meta"), file = "WJD_meta.RData")
