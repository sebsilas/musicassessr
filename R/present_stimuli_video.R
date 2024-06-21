


#' Video melodic production page
#'
#' @param video_file
#' @param melody_no
#' @param total_no_melodies
#'
#' @return
#' @export
#'
#' @examples
video_melodic_production_page <- function(video_file,
                                          melody_no,
                                          total_no_melodies) {
  musicassessr::present_stimuli(
    stimuli = video_file,
    stimuli_type = "video",
    display_modality = "visual",
    page_title = "Play the melody by ear",
    page_text = shiny::tags$div(
      shiny::tags$p("Play the melody by ear then click Stop when you are finished."),
      video_stimulus_event_listener(),
      set_melodic_stimuli("test", "test"),
    ),
    page_type = "record_audio_page",
    get_answer = musicassessr::get_answer_add_trial_and_compute_trial_scores_s3,
    hideOnPlay = TRUE,
    page_label = video_file,
    #answer_meta_data = tb_row,
    trigger_start_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall")$trigger_end_of_stimulus_fun,
    trigger_end_of_stimulus_fun = musicassessr::paradigm(paradigm_type = "simultaneous_recall")$trigger_end_of_stimulus_fun,
    #db_vars = db_vars,
    happy_with_response = TRUE,
    #attempts_left = attempts_left,
    #max_goes = max_goes,
    total_no_melodies = total_no_melodies,
    show_progress = TRUE,
    melody_no = melody_no
  )
}


present_stimuli_video <- function(video_url, ...) {
  vid <- shiny::tags$div(htmltools::HTML(
    paste0(
      '<video id = \"stimulus_video\" controls width=\"640px\", height=\"350px\">
    <source muted=\"false\", src=\"',
      video_url,
      '\" type = \"video/mp4\">
    Sorry, your browser doesn\'t support embedded videos.
    </video>'
    )
  ))

  vid
}


video_stimulus_event_listener <- function() {
  shiny::tags$script(
    htmltools::HTML(
      "
    const video = document.getElementById('stimulus_video');

  video.addEventListener('play', function() {
    // Hide the controls
    video.controls = false;

    // Trigger a function when the video starts playing
    onVideoPlay();
  });

  video.addEventListener('ended', function() {
    // Trigger a function when the video ends
    onVideoEnd();
  });

  function onVideoPlay() {

    console.log('Video started playing!');

    startRecording();
    recordUpdateUI('record_audio_page', false, false, true, false, false);

  }

  function onVideoEnd() {
    console.log('Video ended!')

     stopRecording();

  }"
    )
  )
}
