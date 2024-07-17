

present_stimuli_audio <- function(audio_url,
                                  hideOnPlay = FALSE,
                                  stop_button_text = psychTestR::i18n("Stop"),
                                  page_type = 'null',
                                  answer_meta_data = data.frame(),
                                  volume = 1,
                                  audio_playback_as_single_play_button = FALSE,
                                  trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
                                  trigger_start_of_stimulus_fun = wrap_js_fun_body("function() { console.log('Stimulus started!');"), ...) {

  if(is.na(trigger_start_of_stimulus_fun)) {
    trigger_start_of_stimulus_fun <- wrap_js_fun_body("console.log('Stimulus started!');")
  }

  if(is.na(trigger_end_of_stimulus_fun)) {
    trigger_end_of_stimulus_fun <- wrap_js_fun_body("console.log('Stimulus finished!');")
  }

  shiny::tags$div(
    shiny::tags$audio(src = audio_url, type = "audio/mp3", controls = "true", id = "player", oncanplay="hide_spinner();"),
    # Volume controls
    if(volume != 1) {
      shiny::tags$script(paste0('document.getElementById("player").volume = ', volume, ';'))
    },
    shiny::tags$br(),
    shiny::tags$br(),
    shiny::tags$script("var audioPlayerUserPaused = false;"),
    if(audio_playback_as_single_play_button) {
      shiny::tags$div(
        shiny::tags$script(shiny::HTML(paste0('document.getElementById("player").style.display = "none";'))),
        shiny::tags$button(psychTestR::i18n("Play"), id = "playButton", class="btn btn-default"),
        shiny::tags$script('document.getElementById("playButton").addEventListener("click", function() { document.getElementById("player").play(); hidePlayButton(); })'),
        shiny::tags$script(paste0('document.getElementById("playButton").addEventListener("click", ', trigger_start_of_stimulus_fun, ');'))
        )
    }
    ,
    # Trigger at end of stimulus
    shiny::tags$script(shiny::HTML(paste0(
      'var player = document.getElementById("player");
      if(typeof player !== "undefined") {
          player.addEventListener("play", function () {
          var audio_duration = player.duration * 1000; // to ms
          console.log("audio_duration: " + audio_duration);
          setTimeout(', trigger_end_of_stimulus_fun, ', audio_duration);
      })
      }'))),
  )
}



