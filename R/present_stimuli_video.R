present_stimuli_video <- function(video_url, ...) {
  vid <- shiny::tags$div(htmltools::HTML(paste0("<video controls width=\"640px\", height=\"350px\">
  <source muted=\"false\", src=\"", video_url,"\" type = \"video/mp4\">
  Sorry, your browser doesn't support embedded videos.
  </video>")))
  vid
}


present_stimuli_audio <- function(audio_url, ...) {
  shiny::tags$audio(src = audio_url, type = "audio/mp3", controls = "true")
}


