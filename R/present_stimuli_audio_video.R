present_stimuli_video <- function(video_url, ...) {
  vid <- shiny::tags$div(htmltools::HTML(paste0("<video controls width=\"640px\", height=\"350px\">
  <source muted=\"false\", src=\"", video_url,"\" type = \"video/mp4\">
  Sorry, your browser doesn't support embedded videos.
  </video>")))
  vid
}


present_stimuli_audio <- function(audio_url,
                                  hideOnPlay = FALSE,
                                  stop_button_text = "Stop",
                                  page_type = 'null',
                                  answer_meta_data = data.frame(),
                                  volume = 1,
                                  audio_playback_as_single_play_button = FALSE,
                                  auto_next_page = FALSE,
                                  trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"), ...) {

  if(page_type == "record_audio_page") {
    on_finish <- paste0('recordAndStop(ms = null, showStop = true, hidePlay = false, id = null, type = \"record_audio_page\", stop_button_text = \"', stop_button_text, '\");')
  } else {
    on_finish <- '0'
  }

  shiny::tags$div(
    shiny::tags$audio(src = audio_url, type = "audio/mp3",
                      controls = "true", id = "player",
                      oncanplay="hide_spinner();"),
    shiny::tags$br(),
    shiny::tags$br(),
    if(audio_playback_as_single_play_button) {
      shiny::tags$div(
        shiny::tags$script(paste0('var player = document.getElementById("player");
                                 player.style.display = \"none\";')),
        shiny::tags$button("Play", id = "playButton", onclick = "player.play();hidePlayButton();", class="btn btn-default"))
    },
    if(volume != 1) {
      shiny::tags$script(paste0('var player = document.getElementById("player");
                                 player.volume = ', volume, ';'))
    },

    # Trigger at end of stimulus
    shiny::tags$script(paste0(
      'var player = document.getElementById("player");
      if(typeof player !== "undefined") {
        console.log(\'in this if\');
          player.addEventListener("play", function () {
          hideAudioFilePlayer();
          var audio_duration = player.duration * 1000; // to ms
          setTimeout(', trigger_end_of_stimulus_fun, ', audio_duration);
      })
      }')),

    if(hideOnPlay) {
      shiny::tags$script(paste0('
        function hide_spinner(){
                      spinner=document.getElementsByClassName(\"hollow-dots-spinner\");
                      if(typeof spinner[0] !== "undefined") {
                        spinner[0].style.display = "none";
                      }
          }
        spinner = document.getElementsByClassName("hollow-dots-spinner");
        if(typeof spinner[0] !== "undefined") {
          spinner[0].style.display = "block";
        }
        var player = document.getElementById("player");
        if(typeof player !== "undefined") {
            console.log(\'in this if\');
            player.addEventListener("play", function () {
            hideAudioFilePlayer();
            var audio_duration = player.duration * 1000; // to ms
            setTimeout(function() {', on_finish, '

        }); audio_duration})
        }'))
    }
  )
}
