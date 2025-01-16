
# Dictionary functions

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

  translation <- musicassessr::musicassessr_dict_df %>%
    dplyr::filter(!! lang_sym == !! non_english_translation) %>%
    dplyr::pull(en) %>%
    as.character()

  if(length(translation) > 1L) {
    translation <- translation[1]
  }

  return(translation)

}

