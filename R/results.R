
tidy_melodies <- function(melody_results) {

  melody_results <- lapply(melody_results, function(x) {
    lapply(x, function(y) {
      if(is.list(y)) {
        lapply(y, as.character)
      } else {
        if(length(y) > 1) {
          paste0(y, collapse = ",")
        } else {
          as.character(y)
        }
      }
    })
  })

  melody_results <- lapply(melody_results, unlist, recursive = FALSE)

  dplyr::bind_rows(melody_results)

}



present_scores <- function(res) {

  # long tones
  long_tones <- as.data.frame(lapply(res$MST.long_note_trials$long_tone_, paste0, collapse = ","))

  long_tone_summary <- ress %>%
    dplyr::select(note_accuracy, note_precision, dtw_distance) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
        dplyr::summarise(mean_note_accuracy = mean(note_accuracy, na.rm = TRUE),
                         note_precision = mean(note_precision, na.rm = TRUE),
                         mean_dtw_distance = mean(note_precision, na.rm = TRUE))

  # arrhythmic
  arrhythmic_melodies <- tidy_melodies(res$MST.arrhythmic_melodies)

  arrhythmic_melody_summary <- arrhythmic_melodies %>% dplyr::select(
  correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
   correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
    similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))

  # rhythmic
  rhythmic_melodies <- tidy_melodies(res$MST.rhythmic_melodies)

  rhythmic_melody_summary <- rhythmic_melodies %>% dplyr::select(
    correct_by_note_events_log_normal, correct_by_note_events_octaves_allowed,
    correct_by_note_events_octaves_allowed_log_normal, accuracy, accuracy_octaves_allowed,
    similarity, note_precision, mean_cents_deviation_from_nearest_stimuli_pitch, mean_cents_deviation_from_nearest_midi_pitch) %>%
      dplyr::mutate_if(is.character,as.numeric) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))

  list("long_note" = long_tone_summary,
       "arrhythmic" = arrhythmic_melody_summary,
       "rhythmic" = rhythmic_melody_summary
       )

}

#' Present Final Results
#'
#' @return
#' @export
#'
#' @examples
final_results <- function() {

  psychTestR::reactive_page(function(state, ...) {
    res <- psychTestR::get_results(state)
    processed_results <- present_scores(res)

  psychTestR::one_button_page(

    shiny::tags$div(

      shiny::tags$h2('Long Note Scores'),

      shiny::renderTable({
        processed_results$long_note
      }, width = "50%"),

      shiny::tags$h2('Arrhythmic Melody Scores'),

      shiny::renderTable({
        processed_results$arrhythmic
      }, width = "50%"),

      shiny::tags$h2('Rhythmic Melody Scores'),

      shiny::renderTable({
        processed_results$rhythmic
      }, width = "50%")

    )
  )

  })
}

#res <- readRDS('/Users/sebsilas/Downloads/results.rds')
#ress <- present_scores(res)


