

add_silence_to_audio_file <- function(old_file = "/Users/sebsilas/true.wav",
                                     new_file = "/Users/sebsilas/truewithsilence.wav",
                                     no_seconds_silence_beginning = 0,
                                     no_seconds_silence_end = 0,
                                     sox_location = system.file("bin/sox", package = "musicassessr") ) {

  cmd <- paste0(sox_location, " ",  old_file, " ", new_file, " pad ", no_seconds_silence_beginning, " ", no_seconds_silence_end)

  logging::loginfo("Running sox cmd: %s", cmd)
  system(cmd)
}


# t <- add_silence_to_audio_file(no_seconds_silence_beginning = 4)
# /Users/sebsilas/musicassessr/inst/bin/sox /Users/sebsilas/true.wav /Users/sebsilas/truewithsilence.wav pad 4 0
