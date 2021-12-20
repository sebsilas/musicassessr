# since the MAST logic is static, pre-pare everything:

get_MAST_high <- function() {
  list.files('inst/www/MAST21_high/')
}

get_MAST_low <- function() {
  list.files('inst/www/MAST21_low/')
}



#' Deploy a block of the MAST21 stimuli
#'
#' @param label
#' @param item_bank
#' @param num_items
#' @param num_examples
#' @param feedback
#' @param get_answer
#' @param sound
#' @param page_text
#' @param page_title_melody
#' @param page_title_long_note
#' @param page_text_long_note
#' @param instruction_text
#'
#' @return
#' @export
#'
#' @examples
MAST21_trials <- function(label = "MAST21", item_bank, num_items, num_examples = NULL, feedback = FALSE,
                          get_answer = musicassessr::get_answer_pyin,
                          sound = "piano",
                          page_text = "Click below to hear the melody. Sing back the melody. Click Stop when finished.",
                          page_title_melody = "Please sing the melody",
                          page_title_long_note = "Sing the note with a \"Dooo\" sound.",
                          page_text_long_note = shiny::tags$p("Please sing the note ", shiny::tags$strong("after"), "you hear it, then click Stop."),
                          instruction_text = "Now you will hear melodies with rhythms. Please try and sing the melodies with the correct rhythm.") {


  long_notes_3 <- purrr::map(1:nrow(MAST_long_notes), function(x) {
    melody <- MAST_long_notes %>% dplyr::slice(x) %>% dplyr::pull(octave_3)
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page", save_answer = TRUE,
                    page_title = page_title_long_note, page_text = page_text_long_note,
                    get_answer = get_answer_pyin_long_note, page_label = paste0("MAST21_", x))
  })

  long_notes_3 <- insert.every.other.pos.in.list(long_notes_3, psychTestR::elt_save_results_to_disk(complete = FALSE))


  long_notes_4 <- purrr::map(1:nrow(MAST_long_notes), function(x) {
    melody <- MAST_long_notes %>% dplyr::slice(x) %>% dplyr::pull(octave_4)
    present_stimuli(stimuli = melody, stimuli_type = "midi_notes",
                    display_modality = "auditory", sound = sound,
                    note_length = 2, page_type = "record_audio_page", save_answer = TRUE,
                    page_title = page_title_long_note, page_text = page_text_long_note,
                    get_answer = get_answer_pyin_long_note, page_label = paste0("MAST21_", x))
  })

  long_notes_4 <- insert.every.other.pos.in.list(long_notes_4, psychTestR::elt_save_results_to_disk(complete = FALSE))


  melodies_octave_3 <- lapply(1:nrow(MAST_melodies), function(x) {
    row <- MAST_melodies %>% dplyr::slice(x)
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row$octave_3), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound, save_answer = TRUE,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row$durations),
                    page_text = page_text, page_title = page_title_melody,
                    get_answer = get_answer_pyin, page_label = paste0("MAST21_", x+4))
  })

  melodies_octave_3 <- insert.every.other.pos.in.list(melodies_octave_3, psychTestR::elt_save_results_to_disk(complete = FALSE))


  melodies_octave_4 <- lapply(1:nrow(MAST_melodies), function(x) {
    row <- MAST_melodies %>% dplyr::slice(x)
    present_stimuli(stimuli = itembankr::str_mel_to_vector(row$octave_4), stimuli_type = "midi_notes",
                    display_modality = "auditory", auto_next_page = TRUE, sound = sound, save_answer = TRUE,
                    page_type = "record_audio_page", durations = itembankr::str_mel_to_vector(row$durations),
                    page_text = page_text, page_title = page_title_melody,
                    get_answer = get_answer_pyin, page_label = paste0("MAST21_", x+4))
  })

  melodies_octave_4 <- insert.every.other.pos.in.list(melodies_octave_4, psychTestR::elt_save_results_to_disk(complete = FALSE))



  if(feedback & !is.function(feedback)) {
    feedback <- feedback_melodic_production
  }

  psychTestR::module(label,
                     c(
                       # instructions

                       # long notes 1-4

                       psychTestR::conditional(
                         test = function(state, ...) {
                           range <- psychTestR::get_global("range", state)
                           range == "Baritone" | range == "Bass" | range == "Tenor"
                         },
                         logic = long_notes_3
                       ),

                       psychTestR::conditional(
                         test = function(state, ...) {
                           range <- psychTestR::get_global("range", state)
                           range == "Alto" | range == "Soprano"
                         },
                         logic = long_notes_4
                       ),


                       psychTestR::conditional(
                         test = function(state, ...) {
                           range <- psychTestR::get_global("range", state)
                           range == "Baritone" | range == "Bass" | range == "Tenor"
                         },
                         logic = melodies_octave_3
                       ),

                       psychTestR::conditional(
                         test = function(state, ...) {
                           range <- psychTestR::get_global("range", state)
                           range == "Alto" | range == "Soprano"
                         },
                         logic = melodies_octave_4
                       )
                     )
  )
}


F3 <- 53
F4 <- 65


MAST_long_notes <- tibble::tibble(
  melody = c("F", "B", "E", "C"),
  octave_3 = c(53, 59, 52, 48),
  octave_4 = c(65, 71, 64, 60)
)


MAST_melodies <- itembankr::MAST21("phrases")

MAST_melodies$octave_3 <- apply(MAST_melodies, MARGIN = 1, function(row) {
  transpose <- ifelse(is.na(row['transpose']), 0, as.integer(row['transpose']))
  paste0(itembankr::rel_to_abs_mel(rel_mel = itembankr::str_mel_to_vector(row['melody']), start_note = F3) + transpose, collapse = ",")
})

MAST_melodies$octave_4 <- apply(MAST_melodies, MARGIN = 1, function(row) {
  transpose <- ifelse(is.na(row['transpose']), 0, as.integer(row['transpose']))
  paste0(itembankr::rel_to_abs_mel(rel_mel = itembankr::str_mel_to_vector(row['melody']), start_note = F4) + transpose, collapse = ",")
})





MAST21_daah <- MAST21_trials(label = "MAST21_daah", sound = "piano",
                         page_title_long_note = "Please sing back the note with a \"Daah\" sound.",
                         page_title_melody = "Please sing back the melody with a \"Daah\" sound.")



MAST21_dooo <- MAST21_trials(label = "MAST21_dooo", sound = "piano",
                             page_title_long_note = "Please sing back the note with a \"Dooo\" sound.",
                             page_title_melody = "Please sing back the melody with a \"Dooo\" sound.")




usethis::use_data(MAST21_daah,
                  MAST21_dooo,
                  overwrite = TRUE, internal = TRUE)


