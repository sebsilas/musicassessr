


#' Include musicassessr scripts in a webpage
#'
#' @param app_name
#' @param visual_notation
#' @param record_audio
#' @param midi_input
#'
#' @return
#' @export
#'
#' @examples
include_musicassessr_js <- function(app_name = "", visual_notation = FALSE, record_audio = TRUE, midi_input = FALSE) {
  htmltools::tagList(
    lapply(musicassessr_js(app_name = app_name,
                           visual_notation = visual_notation,
                           midi_input = midi_input,
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
#' @param app_name
#' @param musicassessr_aws
#' @param visual_notation
#' @param midi_file_playback
#' @param record_audio
#' @param midi_input
#'
#' @return
#' @export
#'
#' @examples
musicassessr_js <- function(app_name,
                            musicassessr_aws = FALSE,
                            visual_notation = FALSE,
                            midi_file_playback = FALSE,
                            record_audio = TRUE,
                            midi_input = FALSE) {

  if(!record_audio) {
    app_name <- ""
  } # This allows us to both make sure that users using musicassessr provide an app name (most use cases) by default, but allow through cases where record audio isn't used (like with the PDT)

  stopifnot(
    is.scalar.logical(musicassessr_aws),
    is.scalar.logical(visual_notation),
    is.scalar.logical(midi_file_playback),
    is.scalar.logical(record_audio),
    is.scalar.character(app_name),
    is.scalar.logical(midi_input)
  )

  if(musicassessr_aws) create_dir_if_doesnt_exist('tmp')

  if(record_audio) {
    logging::loginfo(paste0('The current app directory is ', getwd(), '. Is that correct?'))
    shiny_app_js_id <- record_audio_setup(app_name, musicassessr_aws)
  }

  c(
    if(record_audio) get_musicassessr_state_js_script(musicassessr_aws),
    "https://cdn.jsdelivr.net/gh/mattdiamond/Recorderjs@08e7abd9/dist/recorder.js",
    "https://www.midijs.net/lib/midi.js",
    if(midi_file_playback) "https://unpkg.com/@tonejs/midi", # only required for midi file playback
    system.file("www/js/modernizr-custom.js", package = "musicassessr"),
    "https://unpkg.com/tone@13.8.25/build/Tone.js",
    system.file("www/js/Tonejs-Instruments.js", package = "musicassessr"),
    if(visual_notation) "https://cdn.jsdelivr.net/npm/opensheetmusicdisplay@0.7.6/build/opensheetmusicdisplay.min.js", # only required when with visual notation
    'https://unpkg.com/tone-rhythm@2.0.0/dist/tone-rhythm.min.js',
    system.file("www/spinner/spin.js", package = "musicassessr"),
    system.file("www/js/musicassessr.js", package = "musicassessr"),
    if(midi_input) "https://cdn.jsdelivr.net/npm/webmidi@2.5.1",
    if(midi_input) system.file("www/js/getMIDIin.js", package = "musicassessr"),
    if(record_audio & musicassessr_aws) paste0("tmp/", shiny_app_js_id),
    "https://sdk.amazonaws.com/js/aws-sdk-2.585.0.min.js"
  )
}



get_musicassessr_state_js_script <- function(musicassessr_aws = FALSE) {

  if(musicassessr_aws) {
    system.file("www/js/musicassessr_production.js", package = "musicassessr")
  } else {

    if(check_port()) {
      system2(command = "npx", args = "kill-port 3000", wait = TRUE)
    }

    system2(command = "node", args = 'node/app.js', wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}

record_audio_setup <- function(app_name, musicassessr_aws) {

  DIRS_TO_CREATE <- c('www', 'www/audio')

  purrr::map(DIRS_TO_CREATE, create_dir_if_doesnt_exist)

  if(musicassessr_aws) {
    js_to_write <- paste0('const shiny_app_name = \"', app_name, '\";')

    shiny_app_js_id <- paste0("shiny_app_", stringr::str_replace_all(app_name, "/", "_"), ".js")

    if(!file.exists(shiny_app_js_id)) {
      write(js_to_write, file = paste0('tmp/', shiny_app_js_id))
    }

  } else {
    if(!dir.exists('node')) {
      R.utils::copyDirectory(system.file('node', package = 'musicassessr'), 'node')
    }
    shiny_app_js_id <- NA
  }

  shiny_app_js_id
}



create_dir_if_doesnt_exist <- function(dir) {
  if(!dir.exists(dir)) {
    R.utils::mkdirs(dir)
  }
}

check_port <- function(port = 3000) {
  # Is something running on port xx?
  os <- get_os()
  if(os %in% c("osx", "linux" )) {
    res <- check_port_mac()
  } else if (os == "windows") {
    res <- check_port_windows()
  } else {
    stop("OS not known.")
  }
  res
}


check_port_mac <- function(port = 3000) {
  res <- system2("lsof", args = c(paste0("-i :", port)), stdout = TRUE, stderr = TRUE, wait = TRUE)
  if(is.null(attributes(res)$status)) {
    TRUE
  } else {
    FALSE
  }
}


check_port_windows <- function(port = 3000) {
  res <- system(command = sprintf('netstat -na', port), intern = T, show.output.on.console = F)

  res <- res[5:length(res)] %>%
    stringr::str_split("[ ]+") %>%
    purrr::map_dfr(function(x){
      local_addr <- x[[3]]
      remote_addr <- x[[4]]
      local_port <- x[[3]] %>% stringr::str_split(":") %>% purrr::pluck(1) %>% dplyr::last()
      remote_port <- x[[4]] %>% stringr::str_split(":") %>% purrr::pluck(1) %>% dplyr::last()
      tibble::tibble(protocol = x[[2]],
             local_addr = local_addr,
             local_port = local_port,
             remote_addr = remote_addr,
             remote_port = remote_port,
             status = x[[5]])
    } )
  res <- nrow(res %>% dplyr::filter(local_port == as.character(port))) > 0
  if(res) {
    TRUE
  } else {
    FALSE
  }
}
