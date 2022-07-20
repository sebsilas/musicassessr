

#' Include musicassessr scripts in a webpage
#'
#' @return
#' @export
#'
#' @examples
include_musicassessr_js <- function(visual_notation = FALSE, record_audio = TRUE) {
  htmltools::tagList(
    lapply(musicassessr::musicassessr_js(visual_notation = visual_notation,
                                         record_audio = record_audio), function(x) {
                                           if(base::startsWith(x, "http")) {
                                             htmltools::tags$script(src = x)
                                           } else {
                                             htmltools::includeScript(x)
                                           }
                                         })
  )
}


#' musicassessr scripts
#'
#' @param state
#' @param visual_notation
#' @param midi_file_playback
#' @param record_audio
#' @param app_name
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js <- function(musicassessr_aws = FALSE,
                            visual_notation = FALSE,
                            midi_file_playback = FALSE,
                            record_audio = TRUE,
                            app_name = character()) {

  if(record_audio) {
    extra_js_id <- record_audio_setup(app_name, musicassessr_aws)
  }

  # TODO add midi_input argument. This would make importing https://cdn.jsdelivr.net/npm/webmidi@2.5.1 and getMIDIin.js optional
  c(
    if(record_audio) get_musicassessr_state_js_script(musicassessr_aws),
    "https://cdn.rawgit.com/mattdiamond/Recorderjs/08e7abd9/dist/recorder.js",
    "https://www.midijs.net/lib/midi.js",
    if(midi_file_playback) "https://unpkg.com/@tonejs/midi", # only required for midi file playback
    system.file("www/js/modernizr-custom.js", package = "musicassessr"),
    "https://unpkg.com/tone@13.8.25/build/Tone.js",
    system.file("www/js/Tonejs-Instruments.js", package = "musicassessr"),
    if(visual_notation) "https://cdn.jsdelivr.net/npm/opensheetmusicdisplay@0.7.6/build/opensheetmusicdisplay.min.js", # only required when with visual notation
    'https://unpkg.com/tone-rhythm@2.0.0/dist/tone-rhythm.min.js',
    system.file("www/spinner/spin.js", package = "musicassessr"),
    system.file("www/js/musicassessr.js", package = "musicassessr"),
    "https://cdn.jsdelivr.net/npm/webmidi@2.5.1",
    system.file("www/js/getMIDIin.js", package = "musicassessr"),
    if(record_audio & musicassessr_aws) paste0("tmp/", extra_js_id)
  )
}



get_musicassessr_state_js_script <- function(musicassessr_aws = FALSE) {

  if(musicassessr_aws) {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {

    system2(command = "npx", args = "kill-port 3000")

    system2(command = "node", args = 'node/app.js', wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}

record_audio_setup <- function(app_name, musicassessr_aws) {

  DIRS_TO_CREATE <- c('tmp', 'www', 'www/audio')

  purrr::map(DIRS_TO_CREATE, create_dir_if_doesnt_exist)

  if(musicassessr_aws) {
    js_to_write <- paste0('const shiny_app_name = \"', app_name, '\";')

    extra_js_id <- paste0("extra_", stringr::str_replace_all(app_name, "/", "_"), ".js")

    if(!file.exists(extra_js_id)) {
      write(js_to_write, file = paste0('tmp/', extra_js_id))
    }

  } else {
    if(!dir.exists('node')) {
      R.utils::copyDirectory(system.file('node', package = 'musicassessr'), 'node')
    }
    extra_js_id <- NA
  }

  extra_js_id
}



create_dir_if_doesnt_exist <- function(dir) {
  if(!dir.exists(dir)) {
    R.utils::mkdirs(dir)
  }
}
