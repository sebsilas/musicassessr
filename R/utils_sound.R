

add_silence_to_audio_file <- function(old_file = "/Users/sebsilas/true.wav",
                                      new_file = "/Users/sebsilas/truewithsilence.wav",
                                      no_seconds_silence_beginning = 0,
                                      no_seconds_silence_end = 0,
                                      sox_location = if(vampr::get_os() == "linux") "/usr/bin/sox" else system.file("bin/sox", package = "musicassessr") ) {

  cmd <- paste0(sox_location, " ",  old_file, " ", new_file, " pad ", no_seconds_silence_beginning, " ", no_seconds_silence_end)

  logging::loginfo("Running sox cmd: %s", cmd)

  res <- tryCatch({
    system(cmd)
  }, error = function(err) {
    logging::logerror(err)
    "ERROR"
  })

  return(res)
}


# t <- add_silence_to_audio_file(old_file = "Seb.record_audio_page.17-10-2023--11-26--20.wav",
#                                new_file = "Seb.record_audio_page.17-10-2023--11-26--20_new.wav",
#                                sox_location = '/usr/bin/sox', no_seconds_silence_beginning = 1)
