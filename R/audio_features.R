


#' @title Extract audio features from an audio file
#' @description **DEPRECATED**: This function has been removed.
#'   Use `itembankr::extract_audio_features()` instead.
#' @param audio_file_path Path to the audio file
#' @param reencode_audio_file_to_lame_mp3 Logical; whether to re-encode the file to MP3
#' @param reencode_audio_file_to_lame_mp3_verbose Logical; verbose re-encoding output
#' @return None — this function errors immediately
#' @export
#' @examples
#' \dontrun{
#' extract_audio_features("path/to/audio.wav") # will throw an error
#' }
extract_audio_features <- function(audio_file_path,
                                   reencode_audio_file_to_lame_mp3 = FALSE,
                                   reencode_audio_file_to_lame_mp3_verbose = FALSE) {
  lifecycle::deprecate_stop(
    when = "2.18.0",
    what = "extract_audio_features()",
    with = "itembankr::extract_audio_features()"
  )
}
