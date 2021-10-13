## code to prepare `musicassessr_dict.xlsx` dataset goes here

musicassessr_dict_df <- readxl::read_excel("data-raw/musicassessr_dict.xlsx")

musicassessr_dict_df <- rbind(musicassessr_dict_df, insts_table2)

usethis::use_data(musicassessr_dict_df, overwrite = TRUE)

usethis::use_data(musicassessr_dict_df, overwrite = TRUE, internal = TRUE)

