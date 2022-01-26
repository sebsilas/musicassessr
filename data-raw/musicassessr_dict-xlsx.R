## code to prepare `musicassessr_dict.xlsx` dataset goes here
library(dplyr)
library(readxl)

musicassessr_dict_df <- readxl::read_excel("data-raw/musicassessr_dict.xlsx")

insts_table <- read_excel('data-raw/musical_instruments.xlsx')

insts_table2 <- insts_table %>% select(-c(low_note, high_note, transpose, clef))


musicassessr_dict_df <- rbind(musicassessr_dict_df, insts_table2)

usethis::use_data(musicassessr_dict_df, overwrite = TRUE)

usethis::use_data(musicassessr_dict_df, insts_table, overwrite = TRUE, internal = TRUE)

# NB.. this also has to be updated at the test level (PBET, SAA etc.. to propogate there)
# i.e., you need to recreate the dicts there
