
#' Simple melodic production feedback
#'
#' @return
#' @export
#'
#' @examples
feedback_melodic_production_simple <- function() {
  feedback_melodic_production(melody_dtw = FALSE, answer_meta_data = FALSE)
}


#' Feedback for melodic production pages
#'
#' @param melody_dtw
#' @param answer_meta_data
#'
#' @return
#' @export
#'
#' @examples
feedback_melodic_production <- function(melody_dtw = TRUE, answer_meta_data = TRUE) {

  psychTestR::reactive_page(function(state, answer, ...) {

    if(!answer$error) {

      # Plots
      plot <- feedback_mel_plot(answer$onsets_noteon,
                                answer$user_response_note,
                                answer$errors_boolean_octaves_allowed,
                                answer$stimuli)


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
      answer$production <- NULL

      # Produce scores table
      scores_tab <- list_to_shiny_table(answer)

      # Make meta data table
      if(answer_meta_data & is.data.frame(amd)) {
        t_names <- names(amd)
        amd <- amd %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% base::t() %>% as.data.frame()
        row.names(amd) <- t_names
        answer_meta_data_tab <- shiny_table(amd)
      } else if (answer_meta_data & is.list(answer_meta_data) &! is.data.frame(answer_meta_data)) {
        answer_meta_data_tab <- list_to_shiny_table(amd)
      } else {
        answer_meta_data_tab <- " "
      }

      present_stimuli(stimuli = answer$user_response_note,
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
                                                  answer_meta_data_tab),
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

    if(is.numeric(l[[x]])) {
      l[[x]]<- round(l[[x]], 2)
    }

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

    if(is.scalar.na(answer$onset) | is.scalar.null(answer$onset) |
       is.scalar.na(answer$freq) | is.scalar.null(answer$freq)) {
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
  target.notes.other.octaves <- as.integer(sort(as.vector(unlist(get_all_octaves_in_gamut(stimuli)))))

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

    res <- lapply(res, function(x) { if(is.list(x)) unlist(x) else x })

    unlist(res)
  }
}


#' Helper for adding a feedback function to a timeline of pages with a progress bar
#'
#' @param items
#' @param feedback
#' @param after
#'
#' @return
#' @export
#'
#' @examples
add_feedback_with_progress <- function(items, feedback, after = 2) {
  if(is.null(feedback) | !is.function(feedback)) {
    unlist(items)
  } else {

    res <- insert_item_into_every_other_n_position_in_list_with_proportion(items, feedback, n = after)

    res <- lapply(res, function(x) { if(is.list(x)) unlist(x) else x })

    unlist(res)
  }
}



display_rhythm_production_feedback <- function(feedback, res) {

  if(feedback && !is.null(res$user_bpm) && !is.na(res$user_bpm)) {
    shiny::showNotification(paste0("BPM: ", round(res$user_bpm, 2)))
  }

  if(feedback && !is.null(res$rhythfuzz) && !is.na(res$rhythfuzz)) {
    shiny::showNotification(paste0("Rhythfuzz: ", round(res$rhythfuzz, 2)))
  }

  if(feedback && !is.null(res$precision) && !is.na(res$precision)) {
    shiny::showNotification(paste0("Precision: ", round(res$precision, 2)))
  }

  if(feedback && !is.null(res$accuracy) && !is.na(res$accuracy)) {
    shiny::showNotification(paste0("Accuracy: ", round(res$accuracy, 2)))
  }

  if(feedback && !is.null(res$dtw_distance) && !is.na(res$dtw_distance)) {
    shiny::showNotification(paste0("DTW Distance: ", round(res$dtw_distance, 2)))
  }

  if(feedback && !is.null(res$tam_distance) && !is.na(res$tam_distance)) {
    shiny::showNotification(paste0("TAM Distance: ", round(res$tam_distance, 2)))
  }
}


#' Feedback as an image
#'
#' @param image
#' @param height
#' @param width
#' @param text
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
feedback_image <- function(image, height = NULL, width = NULL, text = "Well done!", progress = NULL) {

  stopifnot(
    is.null.or(progress, is.scalar.numeric)
  )


  if(is.null(height) || is.null(width)) {
    img <- shiny::tags$img(src = image)
  } else {
    img <- shiny::tags$img(src = image, height = height, width = width)
  }

  ui <- shiny::tags$div(
             if(!is.null(progress)) progress_bar(progress),
             img,
             shiny::tags$h3(text)
             )

  psychTestR::one_button_page(ui)
}



feedback_melodic_production_async <- function() {

  psychTestR::reactive_page(function(state, answer, ...) {

    filename <- answer$wav_file

    logging::loginfo("filename: %s", filename)

    ui <- feedback_melodic_production_async_ui(filename)

    psychTestR::one_button_page(ui)

  })

}

feedback_melodic_production_async_ui <- function(filename, poll_ms = 5000) {

  ui <- shiny::tags$div(
    # CSS
    tags$style(shiny::HTML("
    .loader {
      border: 8px solid #f3f3f3;
      border-top: 8px solid #A5C54D;
      border-radius: 50%;
      width: 60px;
      height: 60px;
      animation: spin 2s linear infinite;
      display: block;
      margin: auto;
    }
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    #data-container {
      text-align: center;
      margin-top: 20px;
    }
  ")),

    # UI
    shiny::tags$div(id = "loader",
        shiny::tags$div(class = "loader"),
        shiny::tags$p("Good try! Let's see how you did...")),
        shiny::tags$img(src = 'https://musicassessr.com/assets/img/bird.png', height = 150, width = 160, style = 'margin: 20px 0 20px 0;'),
    shiny::tags$div(id = "data-container"),

    # Javascript
    shiny::tags$script(shiny::HTML(paste0("
    const apiUrl = 'https://api.songbird.training/v2/get-job-status';

  async function fetchData() {

    console.log('Fetching feedback for  ", filename, "');

    const payload = {
      filename: \'", filename, "\'
    };

    try {
      const response = await fetch(apiUrl, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(payload)
      });
      if (!response.ok) {
        throw new Error('Network response was not ok ' + response.statusText);
      }
      const data = await response.json();

      // Check if the status is 'FINISHED'
      if (data.status === 'FINISHED') {
        console.log('Job is finished. Stopping polling.');
        const message = JSON.parse(data.message);
        const score = message.opti3.opti3;
        displayScore(score);
        stopPolling();
        hideLoader();
      }
    } catch (error) {
      console.error('Fetch error:', error);
    }
  }

  function displayScore(score) {
    const container = document.getElementById('data-container');
    console.log('opti3: ', + score);
    score = benevolentOpti3(score);
    container.innerHTML = `<p>Well done! </p> <img src = 'https://musicassessr.com/assets/img/bird.png', height = 150, width = 160 style = 'margin: 20px 0 20px 0;' /> <p>Your score was ${score}!</p>`;
  }

  function benevolentOpti3(score) {
    // Apply a quadratic transformation
    let benevolentScore = Math.sqrt(score);

    // Scale to the range 1 to 10
    let scaledScore = 1 + benevolentScore * 9;

    // Round up
    scaledScore = Math.ceil(scaledScore);

    return scaledScore;
  }


  function showLoader() {
    document.getElementById('loader').style.display = 'block';
  }

  function hideLoader() {
    document.getElementById('loader').style.display = 'none';
  }

  const pollingInterval = ", poll_ms, "; // 5 seconds

  const intervalId = setInterval(fetchData, pollingInterval);

  fetchData();

  function stopPolling() {
    clearInterval(intervalId);
  }
  ")
   ))
  )
}

