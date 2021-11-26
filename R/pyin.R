
#' pyin
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#' @param type
#'
#' @return
#' @export
#'
#' @examples
pyin <- function(file_name, transform_file = NULL,
                 normalise = FALSE, hidePrint = TRUE, type = "notes") {

  if(type == "pitch_track") {
    vamp_cmd <- "vamp:pyin:pyin:smoothedpitchtrack"
  } else if(type == "notes") {
    vamp_cmd <- "vamp:pyin:pyin:notes"
  } else {
    stop("Unknown type")
  }

  if(is.null(transform_file)) {
    args <- c("-d",
              vamp_cmd,
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }

  cmd <- ifelse(musicassessr_state == "production", "/opt/sonic-annotator/sonic-annotator", "/Users/sebsilas/sonic-annotator")

  print(cmd)

  if(hidePrint) {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE)
  }

  if(length(sa_out) == 0) {
    if(type == "notes") {
      res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
    } else {
      res <- tibble::tibble(onset = NA, freq = NA, file_name = file_name)
    }
  } else {
    res <- read.csv(text = sa_out, header = FALSE)

    if(type == "notes") {
      res <- res %>%
        dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
        dplyr::mutate(
          onset = round(onset, 2),
          dur = round(dur, 2),
          freq = round(freq, 2),
          note = round(hrep::freq_to_midi(freq)))
    } else {
      res <- res %>%
        dplyr::rename(onset = V2, freq = V3) %>%
        dplyr::mutate(
          onset = round(onset, 2),
          freq = round(freq, 2))
    }

    file_name <- res$V1[[1]]

    res <- res %>% dplyr::select(-V1)

    res <- tibble::tibble(file_name, res)
  }
}



#pyin('/Users/sebsilas/true.wav')
