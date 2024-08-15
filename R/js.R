


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
#' @param asynchronous_api_mode
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
                            asynchronous_api_mode = FALSE,
                            visual_notation = FALSE,
                            midi_file_playback = FALSE,
                            record_audio = TRUE,
                            midi_input = FALSE) {

  stopifnot(
    is.scalar.logical(asynchronous_api_mode),
    is.scalar.logical(visual_notation),
    is.scalar.logical(midi_file_playback),
    is.scalar.logical(record_audio),
    is.scalar.character(app_name),
    is.scalar.logical(midi_input)
  )

  if(record_audio) {
    logging::loginfo(paste0('The current app directory is ', getwd(), '. Is that correct?'))
    shiny_app_id <- record_audio_setup(asynchronous_api_mode, app_name)
  } else {
    shiny_app_id <- NULL
  }

  c(
    if(record_audio) get_musicassessr_state_js_script(asynchronous_api_mode),
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
    "https://sdk.amazonaws.com/js/aws-sdk-2.585.0.min.js",
    paste0('www/', shiny_app_js_id)
  )
}



get_musicassessr_state_js_script <- function(asynchronous_api_mode = FALSE) {

  if(!asynchronous_api_mode && ! on_musicassessr_aws() ) {

    if(check_port()) {
      system2(command = "npx", args = "kill-port 3000", wait = TRUE)
    }

    system2(command = "node", args = 'node/app.js', wait = FALSE)

    system.file("www/js/musicassessr_test.js", package = "musicassessr")
  }

}

record_audio_setup <- function(asynchronous_api_mode, app_name) {

  if(!asynchronous_api_mode && ! on_musicassessr_aws() ) {
    if(!dir.exists('node')) {
      R.utils::copyDirectory(system.file('node', package = 'musicassessr'), 'node')
    }
  }

  if( on_musicassessr_aws() ) {

    DIRS_TO_CREATE <- c('www', 'www/audio')

    purrr::map(DIRS_TO_CREATE, create_dir_if_doesnt_exist)

    js_to_write <- paste0('const shiny_app_name = \"', app_name, '\";')

    shiny_app_js_id <- paste0("shiny_app_", stringr::str_replace_all(app_name, "/", "_"), ".js")

    if(!file.exists(shiny_app_js_id)) {
      write(js_to_write, file = paste0('www/', shiny_app_js_id))
    }

  } else {
    shiny_app_js_id <- NULL
  }

  return(shiny_app_js_id)
}


on_musicassessr_aws <- function() {
  file.exists('/srv/shiny-server/musicassessr-aws-check.txt')
}


create_dir_if_doesnt_exist <- function(dir) {

  tryCatch({
    if (!dir.exists(dir)) {
      dir.create(dir_path, recursive = TRUE, mode = "0755")
    }
    Sys.chmod(dir_path, mode = "0755")
  }, error = function(e) {
    message("Failed to create directory: ", e)
  })

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
    str_split("[ ]+") %>%
    map_dfr(function(x){
      local_addr <- x[[3]]
      remote_addr <- x[[4]]
      local_port <- x[[3]] %>% str_split(":") %>% pluck(1) %>% last()
      remote_port <- x[[4]] %>% str_split(":") %>% pluck(1) %>% last()
      tibble(protocol = x[[2]],
             local_addr = local_addr,
             local_port = local_port,
             remote_addr = remote_addr,
             remote_port = remote_port,
             status = x[[5]])
    } )
  res <- nrow(res %>% filter(local_port == as.character(port))) > 0
  if(res) {
    TRUE
  } else {
    FALSE
  }
}
