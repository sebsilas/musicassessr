# references
# https://yihui.shinyapps.io/DT-radio/


path.to.audio <- 'all_audio_both_data_collections/'

magma.colors <- c("#aadb37", "#ff523b", "#daf571", "#efffb0", "#A5B3AF", "#163F50")

# functions
# graph production data

plot_prod_df <- function(df, row_number, errorsOctaveAllowed = TRUE, pitchOctaveIndependent = TRUE) {

  if(length(row_number) < 1) {
    row_number <- 1
  }
  if(row_number < 1) {
    row_number <- 1
  }

  row <- df %>% dplyr::slice(row_number)

  if (errorsOctaveAllowed) {
    # plot the errors allowed variable or not?
    error_plot <- as.numeric(as.logical(unlist(strsplit(row %>% dplyr::pull(errors_boolean_octaves_allowed), ","))))
  } else {
    error_plot <- as.numeric(as.logical(unlist(strsplit(row %>% dplyr::pull(errors_boolean_octaves_allowed), ","))))
  }

  stimuli <- row %>% dplyr::pull(stimuli)
  target.notes <- itembankr::str_mel_to_vector(stimuli)
  pitch_plot <- itembankr::str_mel_to_vector(row %>% dplyr::pull(user_response_note))

  if (!pitchOctaveIndependent) {
    # get target notes
    # find them in other octaves
    target.notes.other.octaves <- unlist(lapply(target.notes, function(x) get_all_octaves_in_gamut (x, midi.gamut.min, midi.gamut.max)))
    #cum.errors <- row %>% dplyr::pull(cum.errors)
    #cum.correct <- row %>% dplyr::pull(cum.correct)
  }
  # given a row number and a df, plot the prod df for that trial
  # create df for plotting
    onsets <- itembankr::str_mel_to_vector(row %>% dplyr::pull(onsets_noteon)) /1000
    # get relevant production data
    prod.df <- tibble::tibble("onset" = c(0, onsets),
                          "pitch" = c(NA, pitch_plot),
                          "error" = c(NA, error_plot)#,
                          #"cum.errors" = c(0, cum.errors),
                          #"cum.correct" = c(0, cum.correct)
    )
    prod.df$error <- as.factor(prod.df$error)

    plot_prod(prod.df, target.notes, target.notes.other.octaves,  pitchOctaveIndependent)

}

plot_prod <- function(prod_df, target_notes, target_notes_other_octaves, pitchOctaveIndependent) {

  ylim_min <- min(prod_df$pitch, na.rm = TRUE) - 2
  ylim_max <- max(prod_df$pitch, na.rm = TRUE) + 2

  ggplot2::ggplot(prod_df, ggplot2::aes(x=onset, y=pitch, color = error)) +
    ggplot2::ylim(ylim_min, ylim_max) +
    { if (pitchOctaveIndependent) ggplot2::scale_y_discrete(breaks=c("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab")) } +
    { if (!pitchOctaveIndependent) ggplot2::geom_hline(yintercept = target_notes_other_octaves, color = magma.colors[4], size = 2) } +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                   axis.ticks.x=ggplot2::element_blank()) +
    ggplot2::geom_hline(yintercept = target_notes, color = magma.colors[3], size = 4, alpha = 0.7) +
    ggplot2::geom_line( color=magma.colors[5]) +
    ggplot2::geom_point(size=3) +
    ggplot2::scale_color_manual(values=c("0" = magma.colors[1], "1" = magma.colors[2]))

}



### end functions

# main


results_explorer <- function(results_df = PBET_calibration_results,
                            errorsOctaveAllowed = TRUE,
                            pitchOctaveIndependent = TRUE) {

  contin_cols <- results_df %>% dplyr::select(where(is.numeric))

  ui <- shiny::basicPage(

    musicassessr_js_scripts("A", "B", "C", "D", "E"),

    shiny::tags$h2("Trials Explorer"),

    shiny::checkboxInput('playbackwithStimuli', 'Playback with Stimuli?', value = FALSE),

    shiny::actionButton("playSelectedTrial", "Play Selected Trial"),

    shiny::actionButton('saveDisqualifiedTrials', 'Save Disqualified Trials'),

    shiny::plotOutput('histograms'),

    htmlOutput('melodyNotation'),

    shiny::htmlOutput('audio'),

    shiny::plotOutput('trialPlot'),

    DT::dataTableOutput("trials")

  )

  server <- function(input, output) {


    # hide some columns (although leave them in the dataframe for plotting)
    columns2hide <- c("prod")

    output$trials <- DT::renderDataTable(results_df, selection = 'single', escape = FALSE,
                                         options = list(searching = TRUE, pageLength = 20,
                                                        columnDefs = list(list(visible=FALSE, targets=match(columns2hide, colnames(results_df))))
                                         ), callback = DT::JS("table.rows().every(function(i, tab, row) {
            var $this = $(this.node());
            $this.attr('id', this.data()[0]);
            $this.addClass('shiny-input-radiogroup');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
    )


    output$histograms <- renderPlot({
      ggplot2::ggplot(tidyr::gather(contin_cols), ggplot2::aes(value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(~key, scales = 'free_x')
    })

    output$trialPlot <- renderPlot({

      if (is.null(input$trials_rows_selected)) {
        plot.new()
      }
      else {
        plot_prod_df(df = results_df, row_number = input$trials_rows_selected,
                     errorsOctaveAllowed = errorsOctaveAllowed, pitchOctaveIndependent = pitchOctaveIndependent)
      }
    })

    observeEvent(input$playSelectedTrial, {

      stimuli_selected <- input$trials_rows_selected

      if (is.null(stimuli_selected)) {
        trial_no <- 1
      }
      else {
        play_trial_wav(results_df, stimuli_selected, input$playbackwithStimuli)
      }

    })

    output$melodyNotation <- renderUI({

      if (is.null(input$trials_rows_selected)) {
        print("nothing selected")
      }
      else {
        stimuli <- results_df %>% dplyr::slice(input$trials_rows_selected) %>% dplyr::pull(stimuli)
        stimuli <- itembankr::str_mel_to_vector(stimuli, sep = ",")
        musicassessr::present_stimuli_midi_notes_both(stimuli = stimuli, note_length = 0.5)
      }
    })

    output$audio <- renderUI({
      row <- input$trials_rows_selected
      if(length(row) < 1) {
        row <- 1
      }
      if(row < 1) {
        row <- 1
      }

      file_name <- results_df %>% dplyr::slice(row) %>% dplyr::pull(key)

      if (is.null(file_name) | is.na(file_name)) {
        shiny::tags$p("No corresponding audio")
      }
      else {
        shiny::tags$audio(src = paste0('musicassessr-assets/PBET_calibration_audio/', file_name), type="audio/wav", controls = TRUE)
      }
    })

  }

  shiny::shinyApp(ui, server)
}

download_results_from_S3 <- function() {
  # used for downloading results:

  #keys <- PBET_calibration_results$key
  #keys <- keys[!is.na(keys)]
  #
  #
  # s3sync(bucket = "shinny-app-source-41630", direction = "download")

  # file_list <- list.files("PBET_calibration_audio")
  # files_to_delete <- file_list[!file_list %in% keys]
  # files_to_delete <- paste0("PBET_calibration_audio/", files_to_delete)
  # unlink(files_to_delete)
}

# need internet
#results_explorer(PBET_calibration_results)
#results_explorer(errorsOctaveAllowed = T, pitchOctaveIndependent = T)
