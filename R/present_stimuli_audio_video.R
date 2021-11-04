present_stimuli_video <- function(video_url, ...) {
  vid <- shiny::tags$div(htmltools::HTML(paste0("<video controls width=\"640px\", height=\"350px\">
  <source muted=\"false\", src=\"", video_url,"\" type = \"video/mp4\">
  Sorry, your browser doesn't support embedded videos.
  </video>")))
  vid
}


present_stimuli_audio <- function(audio_url, hideOnPlay = FALSE, page_type = 'null',
                                  record_audio_method = 'aws_pyin', stop_button_text = "Stop",
                                  answer_meta_data = 0, ...) {

  if(page_type == "record_audio_page") {
    page_type <- record_audio_method
  }


  shiny::tags$div(
    shiny::tags$audio(src = audio_url, type = "audio/mp3", controls = "true", id = "player"),
    shiny::tags$br(),
    shiny::tags$br(),
    if(hideOnPlay) {
      shiny::tags$script(paste0('
        var player = document.getElementById("player");
        if(typeof player !== "undefined") {
            console.log(\'in this if\');
            player.addEventListener("play", function () {
            hideAudioFilePlayer();
            var audio_duration = player.duration * 1000; // to ms
            setTimeout(function(){  recordAndStop(ms = null, showStop = true, hidePlay = false, id = null, type = \"', record_audio_method, '\", stop_button_text = \"', stop_button_text, '\") }, audio_duration);
        }); }'))
    }
  )
}
