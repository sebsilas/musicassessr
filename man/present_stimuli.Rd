% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/present_stimuli.R
\name{present_stimuli}
\alias{present_stimuli}
\title{Present stimuli}
\usage{
present_stimuli(
  stimuli,
  stimuli_type,
  display_modality,
  page_type = character(),
  page_text = " ",
  page_title = " ",
  slide_length = numeric(),
  answer_meta_data = character(),
  get_answer = function(input, ...) {
    
    stop("Specify a proper get_answer function if you're using present_stimuli with page_type")

    },
  save_answer = TRUE,
  stimuli_reactive = FALSE,
  midi_device = " ",
  page_label = "x",
  button_text = psychTestR::i18n("Next"),
  play_button_text = psychTestR::i18n("Play"),
  note_length = 0.5,
  sound = "piano",
  asChord = FALSE,
  ascending = TRUE,
  start_note = 0L,
  end_note = "end",
  durations = numeric(),
  choices = character(),
  user_rating = FALSE,
  page_text_first = TRUE,
  happy_with_response = FALSE,
  attempts_left = 1L,
  visual_music_notation_id = "sheet_music",
  play_button_id = "playButton",
  button_area_id = "button_area",
  hideOnPlay = FALSE,
  record_immediately = FALSE,
  max_goes_forced = FALSE,
  transpose_visual_notation = 0L,
  clef = "auto",
  volume = 1,
  audio_playback_as_single_play_button = FALSE,
  max_goes = 1L,
  melody_no = 0L,
  total_no_melodies = 0L,
  show_progress = FALSE,
  sheet_music_start_hidden = FALSE,
  sound_only_first_melody_note = FALSE,
  sheet_music_id = "sheet_music",
  give_first_melody_note = FALSE,
  slider_value = 5,
  slider_min = 1,
  slider_max = 10,
  octave = 4,
  volume_meter = FALSE,
  volume_meter_type = "default",
  show_sheet_music_after_record = FALSE,
  interactive = FALSE,
  show_record_button = FALSE,
  trigger_start_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus started!');"),
  trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
  first_note_message = psychTestR::i18n("first_note_is"),
  transposed_message = psychTestR::i18n("transposed"),
  play_first_note_button_text = psychTestR::i18n("play_first_note"),
  reactive_melody_no = FALSE,
  mute_midi_playback = FALSE,
  db_vars = NULL,
  lowest_reading_note = NA,
  highest_reading_note = NA,
  lyrics = NULL,
  feedback = FALSE,
  asynchronous_api_mode = FALSE,
  key = NULL,
  on_complete = NULL,
  ...
)
}
\arguments{
\item{trigger_start_of_stimulus_fun}{A string of an anonymous Javascript function (with body) to trigger when the stimulus begins.}

\item{trigger_end_of_stimulus_fun}{A string of an anonymous Javascript function (with body) to trigger when the stimulus is completed.}

\item{mute_midi_playback}{Should MIDI audio feedback be muted on record_midi_pages?}

\item{db_vars}{Vars for the DB as a named list.}

\item{on_complete}{}
}
\description{
Present stimuli
}
