


#' Download files from the ec2 instance (in batches)
#'
#' @param file_name
#' @param absolute_url
#' @param download_location
#' @param batch_size
#'
#' @return
#' @export
#'
#' @examples
download_files_from_ec2 <- function (file_name, absolute_url, download_location, batch_size = 100) {

  url <- paste0(absolute_url, "/files/", file_name)
  dl_location <- paste0(download_location, file_name)

  df <- data.frame(url = url, dl_location = dl_location)

  audio_files_to_download_bins <- split(df, (as.numeric(rownames(df))-1) %/% batch_size)

  purrr::map(audio_files_to_download_bins, download_batch_from_ec2)

}


download_batch_from_ec2 <- function(df) {
  purrr::pmap(df, function(url, dl_location) {
    if(RCurl::url.exists(url)) {
      download.file(url, dl_location)
    } else {
      warning(paste0(url, ' does not exist.'))
      NA
    }

  })
}



