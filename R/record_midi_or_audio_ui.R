
# common ui template for midi and audio pages
record_midi_or_audio_ui <- function(body = "",
                                    label = "record_audio_page",
                                    stimuli = NULL,
                                    stimuli_reactive = FALSE,
                                    page_text = " ",
                                    page_title = " ",
                                    page_type = "record_audio_page",
                                    interactive = FALSE,
                                    get_answer,
                                    answer_meta_data = tibble::tibble(),
                                    record_button_text = psychTestR::i18n("Record"),
                                    stop_button_text = psychTestR::i18n("Stop"),
                                    record_duration = NULL,
                                    on_complete = NULL,
                                    save_answer = TRUE,
                                    page_text_first = TRUE,
                                    happy_with_response =  FALSE,
                                    attempts_left = 1L,
                                    max_goes_forced = FALSE,
                                    autoInstantiate = TRUE,
                                    midi_device = " ",
                                    max_goes = 1L,
                                    show_progress = FALSE,
                                    melody_no = 0,
                                    total_no_melodies = 0,
                                    volume_meter = FALSE,
                                    volume_meter_type = 'default',
                                    show_sheet_music_after_record = FALSE,
                                    show_record_button = TRUE,
                                    reactive_melody_no = FALSE,
                                    mute_midi_playback = FALSE,
                                    db_vars = NULL,
                                    lyrics = NULL,
                                    feedback = FALSE,
                                    asynchronous_api_mode = FALSE, ...) {


  if(max_goes > 1L) {
    happy_with_response <- TRUE
  }


  if(is.character(page_text)) {
    page_text <- shiny::tags$p(page_text)
  }

  attempt <- max_goes - attempts_left

  section_progress <- if(reactive_melody_no) paste0(psychTestR::i18n("Section_Progress"), ': ', melody_no) else paste0(psychTestR::i18n("Section_Progress"), ': ', melody_no, "/", total_no_melodies)

  if(is.list(db_vars)) {
    db_vars$page_label <- label
  }

  psychTestR::page(ui = shiny::tags$div(

      # We force this to keep turning on to make sure it doesn't get unset e.g., if there is a reload
      if(asynchronous_api_mode) turn_on_upload_to_s3_mode(log = TRUE),

      # Set attempts
      shiny::tags$script(
        shiny::HTML(paste0('Shiny.setInputValue("attempt", ', jsonlite::toJSON(attempt), ');
                           console.log(\"This is a ', page_type, '\");'))
      ),

      if(page_type == "record_midi_page") autoInstantiateMidi(autoInstantiate, midi_device, interactive, mute_midi_playback),

      send_page_label_to_js(label),

      shiny::tags$script(set_answer_meta_data(answer_meta_data)),

      # Set JS vars for musicassessrdb
      if(!is.scalar.null(db_vars)) set_answer_meta_data_for_db_as_js_vars(db_vars),

      if(show_progress) shiny::tags$h4(section_progress),

      if(attempt > 0) shiny::tags$h4(paste0(psychTestR::i18n("Attempt"), " ", attempt, "/", max_goes)),

      shiny::tags$h2(id = "trial_page_title", page_title),

      if(page_text_first) shiny::tags$div(id = "trial_page_text", page_text),

      shiny::tags$div(id = "bodyArea", body),

      if(!is.null(lyrics)) {
        shiny::tags$div(id = "lyrics",
          shiny::tags$h3(psychTestR::i18n("Lyrics")),
          convert_to_html_paragraphs(lyrics)
        )
      },

      shiny::tags$img(id = "listenImg",
                      src = "https://musicassessr.com/assets/img/ear2.png",
                      height = 150, width = 150, style = "display: none;"),

      if(is.null(stimuli)) shiny::tags$script("trigger_next_page = true;") else shiny::tags$div(id = "stimuliArea", stimuli),

      if(volume_meter) shiny::tags$div(volume_meter(volume_meter_type, start_hidden = TRUE), shiny::includeScript(path=system.file("www/js/microphone_signal_test.js", package = "musicassessr"))),

      present_record_button(show_record_button, page_type, record_button_text, stop_button_text, show_sheet_music_after_record, stop_recording_automatically_after_ms = record_duration * 1000),

      happy_with_response_message(happy_with_response, attempts_left, max_goes_forced, max_goes),

      if(is_function_or_true(feedback) && asynchronous_api_mode) feedback_melodic_production_async_ui(),

      if(!page_text_first) page_text,

      shiny::tags$script(htmltools::HTML(paste0('apiUrl = "', Sys.getenv("ENDPOINT_URL"), '\"')))
  ),
  label = label,
  get_answer = get_answer,
  save_answer = save_answer,
  on_complete = on_complete
  )

}


set_answer_meta_data_for_db_as_js_vars <- function(db_vars) {

  if(is.null(db_vars$onset)) {
    db_vars$onset <- FALSE
  }

  if(is.null(db_vars$pyin_type)) {
    db_vars$pyin_type <- "notes"
  }

  # Replace nulls with NAs
  db_vars <- purrr::map(db_vars, replace_nulls)

  # Leave the is.nulls() above before the stopifnot, so that e.g., onset is created, if it doesn't already exist

  print(setdiff(db_var_names, names(db_vars)))

  stopifnot(length(setdiff(db_var_names, names(db_vars))) == 0)

  additional <- if(is.scalar.character(db_vars$additional)) db_vars$additional else jsonlite::toJSON(db_vars$additional, auto_unbox = TRUE)

  db_vars$additional <- additional

  js_holder <- db_var_names %>%
    purrr::map(function(name) {
      htmltools::HTML(paste0("db_", name, " = \'", db_vars[[name]], "\';"))
    })


  scr <- htmltools::HTML(paste0(js_holder))

  shiny::tags$script(scr)

}

db_var_names <- c("stimuli",
  "stimuli_durations",
  "trial_time_started",
  "instrument",
  "attempt",
  "item_id",
  "display_modality",
  "phase",
  "rhythmic",
  "session_id",
  "test_id",
  "onset",
  "new_items_id",
  "review_items_id",
  "user_id",
  "feedback",
  "feedback_type",
  "trial_paradigm",
  "additional",
  "melody_block_paradigm",
  "file_type",
  "noise_filename",
  "page_label",
  "module",
  "pyin_type"
  )



#' Create a template for db_vars
#'
#' @param init_with_time_started
#' @param trial_time_started
#' @param trial_paradigm
#' @param onset
#' @param pyin_type
#' @param attempt
#' @param item_id
#' @param page_label
#' @param display_modality
#' @param phase
#' @param feedback
#' @param additional
#' @param test_id
#' @param session_id
#' @param instrument
#' @param melody_block_paradigm
#' @param module
#' @param rhythmic
#' @param user_id
#'
#' @returns
#' @export
#'
#' @examples
create_db_vars_template <- function(init_with_time_started = TRUE,
                                    trial_time_started = Sys.time(),
                                    trial_paradigm = "call_and_response",
                                    onset = FALSE,
                                    pyin_type = "notes",
                                    attempt = 1L,
                                    item_id = NULL,
                                    page_label = NULL,
                                    display_modality = "auditory",
                                    phase = "test",
                                    feedback = FALSE,
                                    additional = list(),
                                    test_id = 1L,
                                    session_id = NULL,
                                    instrument = NULL,
                                    melody_block_paradigm = "NA",
                                    module = "NA",
                                    rhythmic = TRUE,
                                    user_id = NULL,
                                    feedback_type = "opti3") {

  empty_obj <- setNames(as.list(rep(NA, length(db_var_names))), db_var_names)
  empty_obj$trial_time_started <- trial_time_started
  empty_obj$trial_paradigm <- trial_paradigm
  empty_obj$onset <- onset
  empty_obj$pyin_type <- pyin_type
  empty_obj$attempt <- attempt
  empty_obj$item_id <- item_id
  empty_obj$page_label <- page_label
  empty_obj$display_modality <- display_modality
  empty_obj$phase <- phase
  empty_obj$feedback <- feedback
  empty_obj$additional <- additional
  empty_obj$test_id <- test_id
  empty_obj$session_id <- session_id
  empty_obj$instrument <- instrument
  empty_obj$melody_block_paradigm <- melody_block_paradigm
  empty_obj$module <- module
  empty_obj$rhythmic <- rhythmic
  empty_obj$user_id <- user_id
  empty_obj$feedback_type <- feedback_type

  return(empty_obj)
}

send_page_label_to_js <- function(label) {
  shiny::tags$script(
    paste0('page_label = \"', label, '\";
           Shiny.setInputValue("page_label", page_label);'
           )
    )
}


loading <- function() {
  htmltools::HTML('
  <div id="hollowDotsSpinner" class="hollow-dots-spinner" :style="spinnerStyle;">
    <div class="dot"></div>
      <div class="dot"></div>
        <div class="dot"></div>
          </div>
    <div id="loading"></div>
    ')
}


return_correct_attempts_left <- function(attempts_left, max_goes_forced = FALSE) {

  if(max_goes_forced) {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1_max_goes_forced")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2_max_goes_forced")
  } else {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2")
  }

  if(attempts_left == 0L) {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("no_more_attempts_next")),
                    shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else if (attempts_left == 1L) {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(shiny::HTML(psychTestR::i18n("happy_with_response_message"))),
                    shiny::tags$p(psychTestR::i18n("attempts_remaining_1")),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = "Try Again", label = "Try Again", onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else if (is.infinite(attempts_left)) {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("happy_with_response_message")),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = "Try Again", label = "Try Again", onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else {
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("were_you_happy")),
                    if(!is.infinite(attempts_left)) shiny::tags$p(paste0(psychTestR::i18n("You_have"), ' ', " ", attempts_left, ' ', psychTestR::i18n("attempts_remaining_if_like"))),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = 'Try Again', label = 'Try Again', onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = 'Continue', label = 'Continue', onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  }
}


happy_with_response_message <- function(happy_with_response_message, attempts_left, max_goes_forced = FALSE, max_goes = 1) {

  if(max_goes == 1) {
    happy_with_response_message <- FALSE
  }

  if(happy_with_response_message) {
    shiny::tags$div(
      return_correct_attempts_left(attempts_left, max_goes_forced),
      shiny::tags$script('show_happy_with_response = true;')
    )
  } else {
    shiny::tags$div(
      shiny::tags$script('show_happy_with_response = false;')
    )
  }
}



present_record_button <- function(show_record_button,
                                  page_type,
                                  record_button_text =  psychTestR::i18n("Record"),
                                  stop_button_text = psychTestR::i18n("Stop"),
                                  show_sheet_music_after_record = FALSE,
                                  sheet_music_id = "sheet_music",
                                  stop_recording_automatically_after_ms = NULL) {

  stopifnot(
    is.null.or(stop_recording_automatically_after_ms, is.numeric)
  )

  if(length(stop_recording_automatically_after_ms) == 0)  {
    stop_recording_automatically_after_ms <- "null"
    show_stop <-  TRUE_to_js_true(TRUE)
  } else {
    show_stop <-  TRUE_to_js_true(FALSE)
  }

  shiny::tags$div(id = "button_area",
                  shiny::tags$script(paste0("stop_button_text = \"", stop_button_text, "\"")),
                  shiny::tags$button(record_button_text, id = "recordButton", class = "btn btn-default action-button", style = if(show_record_button) "visibility: visible;" else "visibility: hidden"),
                  shiny::tags$button(stop_button_text, id = "stopButton", class = "btn btn-default action-button", style = "visibility: hidden;"),
                  shiny::tags$script(shiny::HTML(paste0('document.getElementById("recordButton").addEventListener("click", function() {
                            startRecording(type = \"', page_type, '\", ', stop_recording_automatically_after_ms, ');
                            recordUpdateUI(type = \"', page_type, '\", ', show_stop, ');',
                                                        if(show_sheet_music_after_record) paste0("showSheetMusic('", sheet_music_id, "');") else "",
                                                        '});'))),
  )

}


