library(readxl)
library(dplyr)

insts_table <- read_excel('data-raw/musical_instruments.xlsx')

insts_table2 <- insts_table %>% select(-c(low_note, high_note))

insts <- insts_table %>% pull(key)

usethis::use_data(insts, insts_table, insts_table2, overwrite = TRUE)
usethis::use_data(insts, insts_table, insts_table2, overwrite = TRUE, internal = TRUE)

