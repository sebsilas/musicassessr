library(readxl)
insts_table <- read_excel('data-raw/musical_instruments.xlsx')
insts <- insts_table$dict_key

insts_dict <- lapply(insts, psychTestR::i18n)

usethis::use_data(insts, insts_table, insts_dict, overwrite = TRUE)
usethis::use_data(insts, insts_table, insts_dict, overwrite = TRUE, internal = TRUE)

