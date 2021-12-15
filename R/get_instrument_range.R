
get_note_until_satisfied_loop <- function(prompt_text, var_name, page_type,
                                          button_text = "Record", show_musical_notation = FALSE) {


  stopifnot(is.character(prompt_text) | class(prompt_text) == "shiny.tag",
            is.character(var_name), is.character(page_type), is.character(button_text),
            is.logical(show_musical_notation))

  c(
    # set the user satisfied state to false

    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("user_satisfied", "No", state)
      psychTestR::set_global(var_name, NA, state)
    }),

    # keep in loop until the participant confirms the note is correct
    psychTestR::while_loop(test = function(state, ...) {
      user_satisfied <- psychTestR::get_global("user_satisfied", state)
      note <- psychTestR::get_global(var_name, state)
      user_satisfied %in% dict_key_to_translations("No") | is.na(note) },
      logic = list(
        # logic page 1, get new note
        midi_or_audio(page_type, prompt_text, var_name),
        # logic page 2, was everything ok with this note?
        check_note_ok(var_name, page_type, show_musical_notation),
        psychTestR::code_block(function(state, answer, ...) {
          psychTestR::set_global("user_satisfied", answer, state)
        })
      )
    )
  )
}



#' Get Instrument Range Pages
#'
#' @param type
#' @param get_range
#' @param show_musical_notation
#'
#' @return
#' @export
#'
#' @examples
get_instrument_range_pages <- function(type, get_range, show_musical_notation = FALSE) {

  # a short multi-page protocol to get the user's frequency range

  stopifnot(is.character(type), is.logical(get_range), is.logical(show_musical_notation))

  if(get_range == "test" | get_range == FALSE) {
    fake_range()
  } else {
    if (type == "microphone") {

      get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation)

    } else if(type == "midi_keyboard") {

      get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation)

    } else {
      midi_or_audio_reactive(show_musical_notation = show_musical_notation)
    }
  }


}


midi_or_audio_reactive <- function(show_musical_notation = FALSE) {
  c(
    # is MIDI?
    psychTestR::conditional(test = function(state, ...) {
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "MIDI"
    },
    logic = get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation)),

    psychTestR::conditional(test = function(state, ...){
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "Microphone"
    },
    logic = get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation))
  )
}

range_explanation_page <- function() {
  psychTestR::one_button_page(shiny::tags$p(style = "text-align: left;", "We will now find your approximate voice range.
                              You will be asked first to sing your lowest comfortable note.
                              The computer will then analyse this note and it will be played back to you.
                              You will be asked to decide if this is a good match to your lowest note.
                              Sometimes the computer can make a large error, and you will know when that happens, and you will have an opportunity to sing your lowest note again, and give the computer another chance."))
}

get_note_until_satisfied_loop_audio <- function(show_musical_notation = FALSE) {


  c(
    range_explanation_page(),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), psychTestR::i18n("get_range_low_note")), var_name = "bottom_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), psychTestR::i18n("get_range_high_note")), var_name = "top_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation),
    present_range(show_musical_notation)
  )
}

get_note_until_satisfied_loop_midi <- function(show_musical_notation = FALSE) {
  c(
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    present_range(show_musical_notation)
  )
}


check_note_ok <- function(var_name, page_type, show_musical_notation = FALSE) {


  stopifnot(is.character(var_name), is.character(page_type), is.logical(show_musical_notation))

  psychTestR::reactive_page(function(answer, state, ...) {
    if(page_type == "record_audio_page") {
      note <- answer$user_response
    } else {
      note <- answer$note
    }


    if(is.na(note)) {
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p(psychTestR::i18n("nothing_entered")),
        shiny::tags$p("Perhaps you are too quiet, or you have a noisy computer fan.")))
    } else {

      if(show_musical_notation) {
        stimuli_type <- "both"
      } else {
        stimuli_type <- "auditory"
      }


      psychTestR::set_global(var_name, note, state)
      present_stimuli(stimuli = note,
                      stimuli_type = "midi_notes",
                      display_modality = stimuli_type,
                      page_text = psychTestR::i18n("correct_note_message"),
                      page_type = "NAFC_page",
                      choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                      label = var_name,
                      play_button_text = psychTestR::i18n("Play")
      )
    }
  })
}

determine_span <- function(highest_user_note, lowest_user_note, adjust_range) {
  # ideally we want to have a span of at least an octave
  # if the user performs the range test properly the span is simply highest_user_note - lowest_user_note
  # however, if they don't perform the range test well, try and determine a sensible range
  span <- highest_user_note - lowest_user_note

  if(adjust_range) {
    if(span < 12) {
      highest_user_note <- highest_user_note + (6 - span)
      lowest_user_note <- lowest_user_note - (6 - span)
      span <- highest_user_note - lowest_user_note
    }
  }

  list("span" = span,
      "highest_user_note" = highest_user_note,
      "lowest_user_note" = lowest_user_note)

}

present_range <- function(show_musical_notation = FALSE, adjust_range = FALSE) {

  stopifnot(is.logical(show_musical_notation), is.logical(adjust_range))
  if(adjust_range) warning("Adjusting range")

  psychTestR::reactive_page(function(state, ...) {
    lowest_user_note <- psychTestR::get_global("bottom_range", state)
    highest_user_note <- psychTestR::get_global("top_range", state)

    span_result <- determine_span(highest_user_note, lowest_user_note, adjust_range)

    psychTestR::set_global("span", span_result$span, state)

    if(adjust_range) {
      range <- c(span_result$lowest_user_note - 3, span_result$highest_user_note + 3)
    } else {
      range <- c(span_result$lowest_user_note, span_result$highest_user_note)
    }


    if(show_musical_notation) {
      stimuli_type <- "both"
    } else {
      stimuli_type <- "auditory"
    }


    present_stimuli(stimuli = range,
                    stimuli_type = "midi_notes",
                    display_modality = stimuli_type,
                    page_text = shiny::tags$div(
                      shiny::tags$p("You can click below to hear your range."),
                      shiny::tags$p("Please note, we may have changed the range to be slightly higher or lower than you actually sang.")
                      ),
                    page_type = "one_button_page",
                    button_text = psychTestR::i18n("Next"))
  })
}

midi_or_audio <- function(type, prompt_text, var_name) {

  if (type == "record_audio_page") {

    musicassessr::record_audio_page(page_text = prompt_text,
                                    label = var_name,
                                    get_answer = get_answer_average_frequency_ff("round"),
                                    show_record_button = TRUE,
                                    show_aws_controls = FALSE,
                                    auto_next_page = TRUE,
                                    button_text = psychTestR::i18n("Record"),
                                    stop_button_text = psychTestR::i18n("Stop"))

  }
  else {
    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(page_text = prompt_text,
                       label = var_name,
                       get_answer = get_answer_midi_note_mode,
                       show_record_button = TRUE,
                       midi_device = midi_device,
                       button_text = psychTestR::i18n("Record"),
                       stop_button_text = psychTestR::i18n("Stop"))
    })
  }
}


