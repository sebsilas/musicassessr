

#' Choose a melody paradigm
#'
#' @param paradigm_type
#' @param page_type
#' @param call_and_response_end
#' @param stop_recording_after_x_seconds
#' @param instantiate_midi
#' @param midi_device
#' @param mute_midi_playback
#' @param attempts_left
#' @param stimuli_type
#' @param feedback
#' @param asynchronous_api_mode
#' @param simultaneous_recall_show_stop
#'
#' @return
#' @export
paradigm <- function(paradigm_type = c("call_and_response", "simultaneous_recall"),
                     page_type = c("record_audio_page", "record_midi_page"),
                     call_and_response_end = c("manual", "auto"),
                     stop_recording_after_x_seconds = NULL,
                     instantiate_midi = FALSE,
                     midi_device = NULL,
                     mute_midi_playback = FALSE,
                     attempts_left = 1L,
                     stimuli_type = c("melody", "audio"),
                     feedback = FALSE,
                     asynchronous_api_mode = FALSE,
                     simultaneous_recall_show_stop = FALSE) {


  # call_and_response_end if "manual", user clicks stop, if "auto" - automatically triggered

  paradigm_type <- match.arg(paradigm_type)
  page_type <- match.arg(page_type)
  call_and_response_end <- match.arg(call_and_response_end)

  stopifnot(
    paradigm_type %in% c("simultaneous_recall", "call_and_response"),
    page_type %in% c("record_audio_page", "record_midi_page"),
    call_and_response_end %in% c("manual", "auto"),
    is.null.or(stop_recording_after_x_seconds, is.scalar.numeric),
    is.scalar.logical(instantiate_midi),
    is.null.or(midi_device, is.scalar.character),
    is.numeric(attempts_left),
    stimuli_type %in% c("audio", "melody"),
    is.scalar.logical(feedback),
    is.scalar.logical(asynchronous_api_mode)
  )

  if(paradigm_type == "simultaneous_recall") {

    trigger_start_of_stimulus_fun <- record_triggers(record = "start",
                                                     page_type = page_type,
                                                     show_stop = simultaneous_recall_show_stop,
                                                     midi_device = midi_device,
                                                     instantiate_midi = instantiate_midi,
                                                     mute_midi_playback = mute_midi_playback,
                                                     stimuli_type = stimuli_type,
                                                     asynchronous_api_mode = asynchronous_api_mode,
                                                     feedback = feedback)

    trigger_end_of_stimulus_fun <- record_triggers(record = "stop", page_type = page_type, stimuli_type = stimuli_type, asynchronous_api_mode = asynchronous_api_mode, feedback = feedback)

  } else if(paradigm_type == "call_and_response") {
    trigger_start_of_stimulus_fun <- wrap_js_fun_body("showListenImage();")
    if(call_and_response_end == "manual") {
      trigger_end_of_stimulus_fun <- record_triggers(record = "start", page_type = page_type, attempts_left = attempts_left, stimuli_type = stimuli_type, asynchronous_api_mode = asynchronous_api_mode, feedback = feedback)
    } else if(call_and_response_end == "auto") {
      trigger_end_of_stimulus_fun <- record_triggers(record = "start", page_type = page_type, stop_recording_after_x_seconds = stop_recording_after_x_seconds, attempts_left = attempts_left, stimuli_type = stimuli_type, asynchronous_api_mode = asynchronous_api_mode, feedback = feedback)
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
                            show_stop = TRUE,
                            instantiate_midi = FALSE,
                            midi_device = NULL,
                            mute_midi_playback = FALSE,
                            attempts_left = 1L,
                            show_sheet_music = FALSE,
                            sheet_music_id = 'sheet_music',
                            stimuli_type = c("melody", "audio"),
                            feedback = FALSE,
                            asynchronous_api_mode = FALSE,
                            trigger_next_page = NULL) {

  record <- match.arg(record)
  page_type <- match.arg(page_type)
  stimuli_type <- match.arg(stimuli_type)


  stopifnot(record %in% c("start", "stop"),
            page_type %in% c("record_audio_page", "record_midi_page"),
            is.null.or(stop_recording_after_x_seconds, is.scalar.numeric),
            is.scalar.logical(show_stop),
            is.scalar.logical(instantiate_midi),
            is.null.or(midi_device, is.scalar.character),
            is.scalar.logical(mute_midi_playback),
            is.numeric(attempts_left),
            is.scalar.logical(show_sheet_music),
            is.scalar.character(sheet_music_id),
            stimuli_type %in% c("audio", "melody"),
            is.scalar.logical(feedback),
            is.scalar.logical(asynchronous_api_mode)
          )

  show_stop <- TRUE_to_js_true(show_stop)
  trigger_next_page <- TRUE_to_js_true(!(attempts_left > 1L))

  if(stimuli_type == "audio") {

    funs <- if(record == "start") {
      paste0("startRecording('", page_type, "'); recordUpdateUI('", page_type, "', ", show_stop, ", true, true, ", trigger_next_page, ", ", TRUE_to_js_true(show_sheet_music), ", '", sheet_music_id, "'); ")
    } else if(record == "stop") {
      if(feedback && asynchronous_api_mode) {
        paste0("stopRecording('", page_type, "', ", trigger_next_page, ");")
      } else {
        paste0("stopRecording('", page_type, "', ", trigger_next_page, "); show_happy_with_response_message();")
      }
    } else ""


  } else {

    if(instantiate_midi && is.null(midi_device)) {
      stop("If instantiate_midi is TRUE, midi_device must be a character string")
    }

    funs <- if(record == "start") {
      paste0("startRecording('", page_type, "'); recordUpdateUI('", page_type, "', ", show_stop, ", true, true, ", trigger_next_page, ", ", TRUE_to_js_true(show_sheet_music), ", '", sheet_music_id, "'); ")
    } else if(record == "stop") {
      paste0("stopRecording('", page_type, "', ", trigger_next_page, "); ")
    } else ""

    stop_recording_after <- if(is.null(stop_recording_after_x_seconds)) NULL else {
      paste0("setTimeout(() => { stopRecording('", page_type, "') }, ", stop_recording_after_x_seconds * 1000, ");")
    }

    if(instantiate_midi) {
      instantiate_midi_fun <- paste0('instantiateMIDI(\"',midi_device,'\", false, ', TRUE_to_js_true(mute_midi_playback), ');')
      funs <- paste0(funs, instantiate_midi_fun)
    }

    funs <- paste0(funs, stop_recording_after)

  }

  wrap_js_fun_body(funs)

}



#' Helper to wrap some JS code in a function
#'
#' @param js_code
#'
#' @return
#' @export
#'
#' @examples
wrap_js_fun_body <- function(js_code) {

  stopifnot(is.scalar.character(js_code))

  shiny::HTML(paste0('function() { ', js_code, '}'))
}

# wrap_js_fun_body("console.log('Stimulus finished!');")
