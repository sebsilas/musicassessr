
get_correct_sonic_annotator_location_musicassessr <- function(state) {
  # within the context of a musicassessr test, get the correct pyin cmd
  musicassessr_state <- ifelse(exists("musicassessr_state"), musicassessr_state, "production")

  if(musicassessr_state == "production") {
    "/opt/sonic-annotator/sonic-annotator"
  } else {
    psychTestR::get_global("sonic_annotator_local_location", state)
  }
}

get_correct_app_dir <- function(state) {
  # musicassessr_state should be a global variable
  musicassessr_state <- ifelse(exists("musicassessr_state"), musicassessr_state, "production")

  ifelse(musicassessr_state == "production",
         '/srv/shiny-server/files/',
         psychTestR::get_global("local_app_file_dir", state))
}

