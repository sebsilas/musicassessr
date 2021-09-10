
# feedback

feedback_melodic_production <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {
    # pitch classes
    present_stimuli(stimuli = answer$user_response_note,
                    stimuli_type = "midi_notes",
                    display_modality = "both",
                    page_title = "Feedback: ",
                    page_text = shiny::tags$div(shiny::tags$p(paste0("Similarity was ", answer$similarity)),
                                    shiny::tags$p(paste0("No correct: ", answer$no_correct)),
                                    shiny::tags$p(paste0("Number of errors: ", answer$no_errors)),
                                    shiny::tags$p(paste0("Accuracy (error by note events): ", answer$accuracy)),
                                    shiny::tags$p(paste0("Time taken: ", answer$trial_length, " seconds.")),
                                    shiny::tags$p(plot)))


  })
}

feedback_long_tone <- function() {
  ##
}

plot_note_data <- function(notes, onsets, quantized_notes) {

  # create df
  data <- data.frame(onsets = onsets,
                     note = unlist(notes),
                     quantized = quantized_notes)

  # Plot
  plot <- ggplot2::ggplot(data, aes(onsets) ) +
          #geom_line(aes(y = note, colour = "red"), alpha = 0.5) +
          geom_line(aes(y = quantized, colour = "blue"), alpha = 0.5, size = 3) +
          geom_point(aes(y = note, colour = "red"), shape=21, color="black", fill="#69b3a2", size=1) +
          theme_ipsum() +
          ggtitle("pitches")

  rendered_plot <- renderPlot({ plot }, width = 500)
}



add_feedback <- function(items, feedback, after = 2) {
  if(is.null(feedback) | !is.function(feedback)) {
    unlist(items)
  } else {
    res <- insert.every.other.pos.in.list(items, feedback())
    res <- lapply(res, function(x) if(is.list(x)) unlist(x))
    unlist(res)
  }
}
