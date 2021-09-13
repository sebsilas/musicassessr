
# feedback

feedback_melodic_production <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {

    amd <- answer$answer_meta_data
    answer$answer_meta_data <- NULL
    d_names <- names(answer)[!names(answer) == "answer_meta_data"]
    amd_names <- names(amd)

    if(is.na(answer$result)) {
      psychTestR::one_button_page("Unfortunately a valid response was not recorded.")
    } else {
      # plot
      plot <- feedback_mel_plot(answer$onsets_noteon, answer$user_response_note, answer$errors_boolean_octaves_allowed, answer$stimuli)

      tab <- shiny::renderTable({
        # nb, duplicate code, make functions
        d <- lapply(1:length(answer), function(x) {
          if(length(answer[[x]]) > 1) {
          paste0(answer[[x]], collapse = ",")
        } else {
          answer[[x]]
          }
        })

        d <- base::t(as.data.frame(d))
        row.names(d) <- d_names
        d
      }, rownames = TRUE, colnames = FALSE, width = "50%")

      tab2 <- shiny::renderTable({
        amd <- lapply(1:length(amd), function(x) {
          if(length(amd[[x]]) > 1) {
            paste0(amd[[x]], collapse = ",")
          } else {
            amd[[x]]
          }
        })
        amd <- base::t(as.data.frame(amd))
        row.names(amd) <- amd_names
        amd
      }, rownames = TRUE, colnames = FALSE, width = "50%")


      present_stimuli(answer$user_response_note,
                      stimuli_type = "midi_notes",
                      display_modality = "both",
                      page_title = "Your Response",
                      page_type = 'one_button_page',
                      page_text = shiny::tags$div(shiny::tags$p(plot), tags$h3('Response Data'), tab, tags$h3('Stimuli Info'), tab2),
                      page_text_first = FALSE
                      )
    }
  })
}

feedback_long_tone <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {

    # plot
    plot <- feedback_long_note_plot(answer$onset, answer$freq, answer$stimuli)

    answer$stimuli <- NULL
    answer$onset <- NULL
    answer$freq <- NULL

    tab <- shiny::renderTable({
      d_names <- names(answer)
      d <- base::t(as.data.frame(answer))
      row.names(d) <- d_names
      d }, rownames = TRUE, colnames = FALSE, width = "50%")

    psychTestR::one_button_page(
      shiny::tags$div(shiny::tags$p(plot), tags$h3('Response Data'), tab)
    )
  })
}

feedback_mel_plot <- function(onsets, pitch_plot, error_plot, stimuli) {

  # create df
  prod.df <- tibble::tibble("onset" = c(0, onsets),
                            "pitch" = c(NA, pitch_plot),
                            "error" = c(NA, as.numeric(error_plot)))

  prod.df$error <- as.factor(prod.df$error)

  target.notes.other.octaves <- unlist(lapply(stimuli, function(x) get_all_octaves_in_gamut (x, midi.gamut.min, midi.gamut.max)))

  plot <- plot_prod(prod.df, stimuli, target.notes.other.octaves, pitchOctaveIndependent = FALSE)

  rendered_plot <- shiny::renderPlot({ plot }, width = 500)
}


feedback_long_note_plot <- function(onsets, freqs, stimuli) {

  stimuli <- hrep::midi_to_freq(stimuli)

  # create df
  prod_df <- tibble::tibble("onset" = c(0, onsets),
                            "freq" = c(NA, freqs))


  plot <- ggplot2::ggplot(prod_df, ggplot2::aes(x=onset, y=freq)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                   axis.ticks.x=ggplot2::element_blank()) +
    ggplot2::geom_hline(yintercept = stimuli, color = magma.colors[3], size = 4, alpha = 0.7) +
    ggplot2::geom_line( color=magma.colors[5])

  rendered_plot <- shiny::renderPlot({ plot }, width = 500)
}



add_feedback <- function(items, feedback, after = 2) {
  if(is.null(feedback) | !is.function(feedback)) {
    unlist(items)
  } else {
    res <- insert.every.other.pos.in.list(items, feedback(), n = after)
    res <- lapply(res, function(x) {
      if(is.list(x)) {
        unlist(x)
      } else { x } })
    print('added_feedback')
    print(res)
    unlist(res)
  }
}

# tl <- psychTestR::join(
#   lapply(LETTERS, psychTestR::one_button_page)
# )
#
# tl2 <- add_feedback(tl, function() 'feedback_melodic_production', after = 4)

