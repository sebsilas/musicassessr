#' Use musicassessr standard dictionary plus concatenate with your own
#'
#' @param additional_dict
#'
#' @return
#' @export
#'
#' @examples
dict <- function(additional_dict) {
  warning('using musicassessr::dict')
  if(all(names(musicassessr_dict_df) ==  names(additional_dict))) {
    dict_df <- rbind(musicassessr_dict_df, additional_dict)
    dict <- psychTestR::i18n_dict$new(dict_df)
  }
  else {
    mar_names <- paste0(names(musicassessr_dict_df), collapse = " ")
    stop(paste0("Your dictionary dataframe must have the same column names as musicassessr's: ", mar_names))
  }
  dict
}

# dictionary functions

dict_key_to_translations <- function(key) {
  cols <- names(musicassessr_dict_df)[!names(musicassessr_dict_df) %in% "key"]
  as.vector(unlist(musicassessr_dict_df[musicassessr_dict_df["key"] == key, cols]))
}

translate_from_dict <- function(non_english_translation, language) {
  as.character(musicassessr_dict_df[musicassessr_dict_df[, language] == non_english_translation, "en"])
}

