library(readxl)
insts_table <- read_excel('data-raw/musical_instruments.xlsx')
insts <- insts_table$dict_key


usethis::use_data(insts, insts_table, overwrite = TRUE)
