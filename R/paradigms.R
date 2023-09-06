

#' Choose a melody paradigm
#'
#' @param paradigm_type
#' @param page_type
#' @param call_and_response_end
#' @param stop_recording_after_x_seconds
#'
#' @return
#' @export
#'
#' @examples
paradigm <- function(paradigm_type = c("call_and_response", "simultaneous_recall"),
                     page_type = c("record_audio_page", "record_midi_page"),
                     call_and_response_end = c("manual", "auto"),
                     stop_recording_after_x_seconds = NULL) {

  # call_and_response_end if "manual", user clicks stop, if "auto" - automatically triggered

  paradigm_type <- match.arg(paradigm_type)
  page_type <- match.arg(page_type)
  call_and_response_end <- match.arg(call_and_response_end)

  stopifnot(
    paradigm_type %in% c("simultaneous_recall", "call_and_response"),
    page_type %in% c("record_audio_page", "record_midi_page"),
    call_and_response_end %in% c("manual", "auto"),
    is.null.or(stop_recording_after_x_seconds, is.scalar.numeric)
  )

  if(paradigm_type == "simultaneous_recall") {
    trigger_start_of_stimulus_fun <- record_triggers(record = "start", page_type = page_type, show_stop = FALSE)
    trigger_end_of_stimulus_fun <- record_triggers(record = "stop", page_type = page_type)
  } else if(paradigm_type == "call_and_response") {
    trigger_start_of_stimulus_fun <- NA
      if(call_and_response_end == "manual") {
        trigger_end_of_stimulus_fun <- record_triggers(record = "start", page_type = page_type)
      } else if(call_and_response_end == "auto") {
        trigger_end_of_stimulus_fun <- record_triggers(record = "stop", page_type = page_type, stop_recording_after_x_seconds = stop_recording_after_x_seconds)
      } else {
        stop('call_and_response_end not understood')
      }
  } else {
    stop("Unknown paradigm_type")
  }

  list(trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
       trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun)

}



record_triggers <- function(record = c("start", "stop"),
                            page_type = c("record_audio_page", "record_midi_page"),
                            stop_recording_after_x_seconds = NULL,
                            show_stop = TRUE) {


  record <- match.arg(record)
  page_type <- match.arg(page_type)


  stopifnot(record %in% c("start", "stop"),
            page_type %in% c("record_audio_page", "record_midi_page"),
            is.null.or(stop_recording_after_x_seconds, is.scalar.numeric),
            is.scalar.logical(show_stop))

  show_stop <- if(show_stop) "true" else "false"

  funs <- if(record == "start") {
    paste0("startRecording('", page_type, "'); recordUpdateUI('", page_type, "', ", show_stop, "); hidePlayButton();")
  } else if(record == "stop") {
    paste0("stopRecording('", page_type, "'); ")
  } else ""

  stop_recording_after <- if(is.null(stop_recording_after_x_seconds)) NULL else {
    paste0("setTimeout(() => { stopRecording('", page_type, "') }, ", stop_recording_after_x_seconds * 1000, ");")
  }

  funs <- paste0(funs, stop_recording_after)

  wrap_js_fun_body(funs)

}



wrap_js_fun_body <- function(js_code) {

  stopifnot(is.scalar.character(js_code))

  shiny::HTML(paste0('function() { ', js_code, '}'))
}

# wrap_js_fun_body("console.log('Stimulus finished!');")
