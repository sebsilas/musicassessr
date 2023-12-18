

#' Melconv
#'
#' @param file_name
#' @param return_notes_and_durs
#'
#' @return
#' @export
#'
#' @examples
melconv <- function(file_name, return_notes_and_durs = TRUE) {

  file_name <- normalizePath(file_name)

  melconv_location <- Sys.getenv("MELCONV_LOCATION")
  melconv_out <- Sys.getenv("MELCONV_OUT")

  # Use melconv

  melconv_res <- system2(command = melconv_location,
           args = c('-f midi',
                    paste0('-i ', file_name),
                    paste0('-o ', melconv_out)),
           stdout = TRUE, stderr = FALSE)

  if(any(grepl("offending", melconv_res)) && return_notes_and_durs) {
    res <- tibble::tibble(onset = NA, durations = NA, note = NA, midi_file = file_name, N = NA)
  } else {
    n <- paste0(melconv_out, basename(file_name))
    res <- itembankr::midi_file_to_notes_and_durations(n)
  }
  # tuneR::readMidi(res)
  res

}



#' Melconv from a pyin result
#'
#' @param pyin_res
#'
#' @return
#' @export
#'
#' @examples
melconv_from_pyin_res <- function(pyin_res) {

  #pyin_res <- pyin(audio_file)

  # sort the problematic file format

  # f <- readr::read_csv(audio_file, col_names = c('onset_s','duration_s', 'pitch_hz')) %>%
  #   dplyr::select(onset_s, pitch_hz, duration_s)

  pyin_res <- pyin_res %>% dplyr::select(onset, freq, dur)

  new_file <- paste0('/srv/shiny-server/files/csv/', paste0(sample(1:9, 20, replace = TRUE), collapse = ""), '.csv')

  # write out
  write.table(pyin_res, file = new_file, row.names = FALSE, col.names = FALSE, sep=",")

  melconv(new_file)

}
