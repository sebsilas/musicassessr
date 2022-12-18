
#' Simple melodic production feedback
#'
#' @return
#' @export
#'
#' @examples
feedback_melodic_production_simple <- function() {
  feedback_melodic_production(melody_dtw = FALSE, answer_meta_data = FALSE)
}


feedback_melodic_production <- function(melody_dtw = TRUE, answer_meta_data = TRUE) {

  psychTestR::reactive_page(function(state, answer, ...) {

    if(is.null(answer$error)) {

      # plots
      plot <- feedback_mel_plot(answer$onsets_noteon, answer$user_response_note, answer$errors_boolean_octaves_allowed, answer$stimuli)


      if(melody_dtw) {
        melody_dtw_plot <- plot_dtw_melody(answer$stimuli, answer$stimuli_durations, answer$pyin_pitch_track)
        melody_dtw_head <- "Melody DTW Plot"
      } else {
        melody_dtw_plot <- " "
        melody_dtw_head <- " "
      }

      if(answer_meta_data) {
        stimuli_info <- 'Stimuli Info'
      } else {
        stimuli_info <- " "
      }

      # get then remove necessary vars
      amd <- answer$answer_meta_data

      if(is.character(amd)) {
        amd <- rjson::fromJSON(amd)
      }

      answer$answer_meta_data <- NULL
      answer$pyin_pitch_track <- NULL

      # produce scores table
      scores_tab <- list_to_shiny_table(answer)

      # make meta data table
      if(answer_meta_data & is.data.frame(amd)) {
        t_names <- names(amd)
        amd <- amd %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% base::t() %>% as.data.frame()
        row.names(amd) <- t_names
        answer_meta_data_tab <- shiny_table(amd)
      } else if (answer_meta_data & is.list(answer_meta_data) &! is.data.frame(answer_meta_data)) {
        print('else iff')
        answer_meta_data_tab <- list_to_shiny_table(amd)
      } else {
        answer_meta_data_tab <- " "
      }

      present_stimuli(answer$user_response_note,
                      stimuli_type = "midi_notes",
                      display_modality = "both",
                      page_title = "Your Response",
                      page_type = 'one_button_page',
                      page_text = shiny::tags$div(shiny::tags$p(plot),
                                                  shiny::tags$h3(melody_dtw_head),
                                                  shiny::tags$p(melody_dtw_plot),
                                                  shiny::tags$h3('Response Data'),
                                                  scores_tab,
                                                  shiny::tags$h3(stimuli_info),
                                                  answer_meta_data_tab
                                                  ),
                      page_text_first = FALSE,
                      play_button_id = "playButtonFeedback",
                      button_area_id = "buttonArea3")

    } else {
      psychTestR::one_button_page("Unfortunately a valid response was not recorded.")
    }
  })
}

shiny_table <- function(content, rownames = TRUE, colnames = FALSE) {
  shiny::renderTable({
    content
  }, rownames = rownames, colnames = colnames, width = "50%")
}

list_to_shiny_table <- function(l, rownames = TRUE, colnames = FALSE) {

  l <- l[!is.na(l) & lengths(l) > 0]
  l_names <- names(l)
  l <- lapply(1:length(l), function(x) {

    if (length(l[[x]]) > 1) {
      paste0(l[[x]], collapse = ",")
    } else {
      l[[x]]
    }
  })
  l <- l[!is.na(l)]
  r <- as.data.frame(base::t(as.data.frame(l)))
  row.names(r) <- l_names
  r <- r %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  shiny_table(r)

}





feedback_long_tone <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {

    if(is.na(answer$onset) | is.null(answer$onset) |
      is.na(answer$freq) | is.null(answer$freq)) {
      plot <- "Sorry, but we cannot provide feedback for this trial. Did you sing?"
  } else {
    # plot
    plot <- feedback_long_note_plot(answer$onset, answer$freq, answer$stimuli)
  }

    answer$stimuli <- NULL
    answer$onset <- NULL
    answer$freq <- NULL

    tab <- list_to_shiny_table(answer)


    psychTestR::one_button_page(
      shiny::tags$div(shiny::tags$p(plot),
                      tags$h3('Response Data'),
                      tab))

  })
}

feedback_mel_plot <- function(onsets, pitch_plot, error_plot, stimuli) {

  # create df
  prod.df <- tibble::tibble("onset" = c(0, onsets),
                            "pitch" = c(NA, pitch_plot),
                            "error" = factor(c(NA, as.numeric(error_plot))))

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



#' Helper for adding a feedback function to a timeline of pages
#'
#' @param items
#' @param feedback
#' @param after
#'
#' @return
#' @export
#'
#' @examples
add_feedback <- function(items, feedback, after = 2) {
  if(is.null(feedback) | !is.function(feedback)) {
    unlist(items)
  } else {
    res <- insert_item_into_every_other_n_position_in_list(items, feedback(), n = after)
    res <- lapply(res, function(x) {
      if(is.list(x)) {
        unlist(x)
      } else { x } })
    unlist(res)
  }
}

# tl <- psychTestR::join(
#   lapply(LETTERS, psychTestR::one_button_page)
# )
#
# tl2 <- add_feedback(tl, function() 'feedback_melodic_production', after = 4)



