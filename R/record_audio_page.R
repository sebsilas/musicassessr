
tidy_freqs <- function(freqs) {
  freqs.wo.null <- as.numeric(unlist(lapply(freqs, function(x) ifelse(is.null(x), 0, x) )))
  notes <- lapply(freqs.wo.null, function(x) ifelse(is.na(x) | x== 0, NA, round(freq_to_midi(as.numeric(x)))) )

  unlist(notes)
}



deploy_crepe <- function(method, crepe_stats = FALSE) {

  if (crepe_stats == TRUE & method == "crepe") {

    tags$div(shiny::tags$canvas(id = "activation"),
        tags$div(id="output",
                 br(),
                 tags$p('Status: ', tags$span(id="status")), tags$br(),
                 tags$p('Estimated Pitch: ', tags$span(id="estimated-pitch")),
                 tags$br(),
                 tags$p('Voicing Confidence: ', tags$span(id="voicing-confidence")),
                 tags$p('Your sample rate is', tags$span(id="srate"), ' Hz.')))
  }
  else {
    tags$div()
  }
}


deploy_aws_pyin <- function(method, crepe_stats = FALSE, show_aws_controls = TRUE, stop_button_text = "Stop") {

  if (method == "aws_pyin") {
    # NB: remove style attribute from pauseButton and/or recordingsList to show pause button or recordings respectively
    tags$div(htmltools::HTML(paste0('<div id="controls">
  	 <button id="recordButton">Record</button>
  	 <button id="pauseButton" disabled style="display: none;">Pause</button>
  	 <button id="stopButton" disabled>',stop_button_text, '</button>
    </div>
    <div id="formats" style="display: none;">Format: start recording to see sample rate</div>
  	<p style="display: none;"><strong>Recordings:</strong></p>
  	<ol id="recordingsList" style="display: none;"></ol>
        <div id="loading" style="display: none;"></div>
        <div id="csv_file" style="display: none;"></div>')), show_aws_buttons(show_aws_controls))
  }
  else {
    div()
  }
}



plot.note.data <- function(notes, onsets, quantized_notes) {

  # create df
  data <- data.frame(onsets = onsets,
                     note = unlist(notes),
                     quantized = quantized_notes
  )

  # Plot
  ggplot(data, aes(onsets) ) +
    #geom_line(aes(y = note, colour = "red"), alpha = 0.5) +
    geom_line(aes(y = quantized, colour = "blue"), alpha = 0.5, size = 3) +
    geom_point(aes(y = note, colour = "red"), shape=21, color="black", fill="#69b3a2", size=1) +
    theme_ipsum() +
    ggtitle("pitches")



}

show_aws_buttons <- function(show_aws_controls) {

  if(show_aws_controls == FALSE) {
    aws_controls <- tags$script('var controls = document.getElementById("controls");
                                controls.style.visibility = \'hidden\'; // start hidden
                                console.log("hide controls");')
  }
  else {
    aws_controls <- tags$script('')
  }

  aws_controls
}



record_audio_page <- function(body = NULL, label = "record_audio_page", stimuli = " ", stimuli_reactive = FALSE, page_text = " ", page_title = " ", interactive = FALSE,
                              note_no = "max", show_record_button = FALSE, get_answer = get_answer_store_async_builder(page_id = "record_audio_page"), transpose = 0, answer_meta_data = 0,
                              method = c("aws_pyin", "crepe"), show_aws_controls = FALSE, crepe_stats = FALSE,
                              button_text = "Record", stop_button_text, ...) {


  #note_no_js_script <- set.note.no(stimuli, note_no)

    psychTestR::page(ui = tags$div(

      tags$head(

        tags$script('console.log(\"this is an audio page\");'),
        tags$script(paste0('console.log(\"method is: ',method,'\");')),
        tags$script(set_answer_meta_data(answer_meta_data)),
        HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">'),
        includeCSS("static-website-s3/style.css")

      ),
      tags$body(
        tags$script('var confidences = [];
                    var user_response_frequencies = [];
                    var timecodes = [];
                    var answer_meta_data;
                    '
                    ),
        tags$h2(page_title),
        tags$p(page_text),
        tags$div(body),
        reactive_stimuli(stimuli_function = stimuli_function,
                         stimuli_reactive = stimuli_reactive,
                         prepared_stimuli = abs_mel),

        present_record_button(show_record_button, type = method, button_text = button_text),

        tags$div(id ="container",
                 deploy_aws_pyin(method = method, show_aws_controls = show_aws_controls, stop_button_text),
                 deploy_crepe(method),
        )
      )
    ),
    label = label,
    get_answer = get_answer,
    save_answer = TRUE
    )
}
