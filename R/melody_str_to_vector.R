melody_str_to_vector <- function(mel_str, sep) {
  vector_mel <- as.numeric(unlist(strsplit(mel_str, sep)))
  vector_mel
}
