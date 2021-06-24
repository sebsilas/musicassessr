#' String melody to vector
#'
#' Takes a melody, written as a string of numbers (e.g MIDI) notes separated by a character (e.g a comma) and returns it as a numeric vector
#'
#' @param mel_str character
#' @param sep character
#'
#' @return integer
#' @export
#'
#' @examples
#' melody_str_to_vector(mel_str = "60, 62, 64, 65", sep = ",")
melody_str_to_vector <- function(mel_str, sep) {
  vector_mel <- as.numeric(unlist(strsplit(mel_str, sep)))
  vector_mel
}
