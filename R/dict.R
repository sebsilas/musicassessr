
#' Use musicassessr standard dictionary plus concatenate with your own
#'
#' @param additional_dict
#' @param main_dict
#'
#' @return
#' @export
#'
#' @examples
dict <- function(additional_dict = NULL, main_dict = musicassessr::musicassessr_dict_df) {
  if(all(names(main_dict) ==  names(additional_dict))) {
    dict_df <- rbind(main_dict, additional_dict)
    dict <- psychTestR::i18n_dict$new(dict_df)
  } else {
    mar_names <- paste0(names(main_dict), collapse = " ")
    stop(paste0("Your dictionary dataframe must have the same column names as musicassessr's: ", mar_names))
  }
  dict
}


# dictionary functions

dict_key_to_translations <- function(key) {
  cols <- names(musicassessr::musicassessr_dict_df)[!names(musicassessr::musicassessr_dict_df) %in% "key"]
  as.vector(unlist(musicassessr::musicassessr_dict_df[musicassessr::musicassessr_dict_df["key"] == key, cols]))
}

#' Translate an item from musicassessr dictionary
#'
#' @param non_english_translation
#' @param language
#'
#' @return
#' @export
#'
#' @examples
translate_from_dict <- function(non_english_translation, language) {

  lang_sym <- rlang::sym(language)

  musicassessr::insts_table %>%
    dplyr::filter(!! lang_sym == !! non_english_translation) %>%
    dplyr::pull(en) %>%
    as.character()

}

