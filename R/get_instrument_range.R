


#' Get Instrument Range Pages
#'
#' @param input_type
#' @param show_musical_notation
#' @param adjust_range
#' @param test_type
#' @param concise_wording
#'
#' @return
#' @export
#'
#' @examples
get_instrument_range_pages <- function(input_type,
                                       show_musical_notation = FALSE,
                                       adjust_range = FALSE,
                                       test_type = c("voice", "instrument"),
                                       concise_wording = FALSE) {


  # A short multi-page protocol to get the user's frequency range

  stopifnot(is.scalar.character(input_type),
            is.logical(show_musical_notation),
            is.scalar.character(test_type),
            is.scalar.logical(concise_wording))

  if (input_type == "microphone") {
    get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type, concise_wording = concise_wording)
  } else if(input_type == "midi_keyboard") {
    get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation, adjust_range = adjust_range)
  } else {
    midi_or_audio_reactive(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type)
  }


}

get_note_until_satisfied_loop <- function(prompt_text, var_name, page_type,
                                          button_text = psychTestR::i18n("Record"), show_musical_notation = FALSE) {


  stopifnot(is.character(prompt_text) | class(prompt_text) == "shiny.tag",
            is.character(var_name), is.character(page_type), is.character(button_text),
            is.logical(show_musical_notation))

  psychTestR::join(
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








midi_or_audio_reactive <- function(show_musical_notation = FALSE, adjust_range = FALSE, test_type = c("voice", "instrument")) {
  c(
    # is MIDI?
    psychTestR::conditional(test = function(state, ...) {
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "MIDI"
    },
    logic = get_note_until_satisfied_loop_midi(show_musical_notation = show_musical_notation, adjust_range = adjust_range)),

    psychTestR::conditional(test = function(state, ...){
      response_type <- psychTestR::get_global("response_type", state)
      response_type == "Microphone"
    },
    logic = get_note_until_satisfied_loop_audio(show_musical_notation = show_musical_notation, adjust_range = adjust_range, test_type = test_type))
  )
}

range_explanation_page <- function(test_type = c("voice", "instrument"), concise_wording = FALSE) {

  if(test_type == "voice") {
    if(concise_wording) {
      text <- psychTestR::i18n("range_explanation_voice_concise")
    } else {
      text <- psychTestR::i18n("range_explanation_voice")
    }
  } else {
    text <- psychTestR::i18n("range_explanation_instrument")
  }
  psychTestR::one_button_page(text, button_text = psychTestR::i18n("Next"))
}

get_note_until_satisfied_loop_audio <- function(show_musical_notation = FALSE, adjust_range = FALSE, test_type = c("voice", "instrument"), concise_wording = FALSE) {

  if(test_type == "voice") {
    low_note_text <- psychTestR::i18n("get_range_low_note_voice")
    high_note_text <- psychTestR::i18n("get_range_high_note_voice")

  } else {
    low_note_text <- psychTestR::i18n("get_range_low_note")
    high_note_text <- psychTestR::i18n("get_range_high_note")
  }

  psychTestR::join(
    range_explanation_page(test_type, concise_wording),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), low_note_text), var_name = "bottom_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation),
    get_note_until_satisfied_loop(prompt_text = shiny::tags$div(shiny::tags$h2(psychTestR::i18n("Range_Test")), high_note_text), var_name = "top_range", page_type = "record_audio_page", show_musical_notation = show_musical_notation),
    present_range(show_musical_notation, adjust_range, test_type)
  )
}

get_note_until_satisfied_loop_midi <- function(show_musical_notation = FALSE, adjust_range = FALSE) {
  c(
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_low_note"), var_name = "bottom_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    get_note_until_satisfied_loop(prompt_text = psychTestR::i18n("get_range_midi_high_note"), var_name = "top_range", page_type = "record_midi_page", show_musical_notation = show_musical_notation),
    present_range(show_musical_notation, adjust_range)
  )
}


check_note_ok <- function(var_name, page_type, show_musical_notation = FALSE) {




  stopifnot(is.character(var_name), is.character(page_type), is.logical(show_musical_notation))

  psychTestR::reactive_page(function(answer, state, ...) {

    transpose <- psychTestR::get_global("transpose_visual_notation", state)
    transpose <- if(is.null(transpose)) 0L else transpose
    clef <- psychTestR::get_global("clef", state)
    clef <- if(is.null(clef)) "auto" else clef

    if(transpose != 0) {
      transposed_note_message <- psychTestR::i18n("transposed")
    } else {
      transposed_note_message <- " "
    }

    if(page_type == "record_audio_page") {
      note <- answer$user_response
    } else {
      note <- answer$note
    }

    if(is.na(note)) {
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$p(psychTestR::i18n("nothing_entered")),
        shiny::tags$p(psychTestR::i18n("audio_error_suggestion"))))
    } else {

      musical_notation <- present_musical_notation_range_page(show_musical_notation, note, transpose, var_name, transposed_note_message)

      psychTestR::set_global(var_name, note, state)


        present_stimuli(stimuli = note,
                        stimuli_type = "midi_notes",
                        display_modality = "auditory",
                        page_text = shiny::tags$div(psychTestR::i18n("correct_note_message"), musical_notation),
                        page_type = "NAFC_page",
                        choices = c(psychTestR::i18n("Yes"), psychTestR::i18n("No")),
                        label = var_name,
                        play_button_text = psychTestR::i18n("Play"),
                        clef = clef)


    }
  })
}

present_musical_notation_range_page <- function(show_musical_notation, note, transpose, var_name, transposed_note_message) {


  if(show_musical_notation) {
    musical_notation <- present_stimuli(stimuli = note + transpose,
                                        stimuli_type = "midi_notes",
                                        display_modality = "visual",
                                        label = var_name,
                                        play_button_text = psychTestR::i18n("Play"))

    shiny::tags$div(musical_notation, shiny::tags$p(transposed_note_message))

  } else {
    musical_notation <- " "
  }
}

determine_span <- function(highest_user_note, lowest_user_note, adjust_range) {
  # ideally we want to have a span of at least an octave
  # if the user performs the range test properly the span is simply highest_user_note - lowest_user_note
  # however, if they don't perform the range test well, try and determine a sensible range

  highest_user_note <- max(highest_user_note, lowest_user_note)
  lowest_user_note <- min(highest_user_note, lowest_user_note)

  span <- highest_user_note - lowest_user_note

  if(adjust_range) {
    if(span < 12) {
      m <- round(mean(lowest_user_note:highest_user_note))
      highest_user_note <- m + 6
      lowest_user_note <- m - 6
      span <- 12
    }
  }

  list("span" = span,
       "highest_user_note" = highest_user_note,
       "lowest_user_note" = lowest_user_note)

}


present_range <- function(show_musical_notation = FALSE, adjust_range = FALSE, test_type = c("voice", "instrument")) {

  stopifnot(is.scalar.logical(show_musical_notation), is.scalar.logical(adjust_range))

  if(adjust_range) warning("Adjusting range")

  if(match.arg(test_type) == "voice") {
    range_adjust_message <- psychTestR::i18n("range_adjust_message_voice")
  } else {
    range_adjust_message <- psychTestR::i18n("range_adjust_message")
  }

  psychTestR::reactive_page(function(state, ...) {

    lowest_user_note <- psychTestR::get_global("bottom_range", state)
    highest_user_note <- psychTestR::get_global("top_range", state)

    span_result <- determine_span(highest_user_note, lowest_user_note, adjust_range)

    range <- c(span_result$lowest_user_note, span_result$highest_user_note)

    psychTestR::set_global("span", span_result$span, state)
    psychTestR::set_global("top_range", span_result$highest_user_note, state)
    psychTestR::set_global("bottom_range", span_result$lowest_user_note, state)


    page <- present_stimuli(stimuli = range,
                    stimuli_type = "midi_notes",
                    display_modality = both_or_auditory(show_musical_notation),
                    page_text = shiny::tags$div(
                      shiny::tags$p(psychTestR::i18n("hear_range")),
                      shiny::tags$p(range_adjust_message)
                    ),
                    page_type = "one_button_page",
                    button_text = psychTestR::i18n("Next"))


    page
  })
}


both_or_auditory <- function(both) {
  if(both) {
    "both"
  } else {
    "auditory"
  }
}


midi_or_audio <- function(type, prompt_text, var_name) {

  if (type == "record_audio_page") {

    record_audio_page(page_text = prompt_text,
                      label = var_name,
                      get_answer = get_answer_average_frequency_ff("round"),
                      button_text = psychTestR::i18n("Record"),
                      stop_button_text = psychTestR::i18n("Stop"))

  } else {
    psychTestR::reactive_page(function(state, ...) {

      midi_device <- psychTestR::get_global("midi_device", state)

      if(is.null(midi_device)) { shiny::showNotification(psychTestR::i18n("no_midi_device_selected")) }

      record_midi_page(page_text = prompt_text,
                       label = var_name,
                       get_answer = get_answer_midi_note_mode,
                       midi_device = midi_device,
                       button_text = psychTestR::i18n("Record"),
                       stop_button_text = psychTestR::i18n("Stop"))
    })
  }
}


#' A page to identify a user's singing range by asking them to sing Happy Birthday
#'
#' @param feedback
#' @param label
#' @param text
#'
#' @return
#' @export
#'
#' @examples
sing_happy_birthday_page <- function(feedback = FALSE, label = "sing_hbd", text = psychTestR::i18n("sing_hbd")) {

  page <- record_audio_page(label = label,
                            page_text = text,
                            get_answer = musicassessr::get_answer_simple_pyin_summary)

  if(feedback) {
    psychTestR::join(
      page,
      psychTestR::reactive_page(function(state, answer, ...) {
        psychTestR::one_button_page(
          shiny::tags$div(
            shiny::tags$h1("Output"),
            shiny::tags$p(paste0('Min: ', answer$Min.)),
            shiny::tags$p(paste0('Max: ', answer$Max.)),
            shiny::tags$p(paste0('Mean: ', answer$Mean)),
            shiny::tags$p(paste0('Median: ', answer$Median))
          )
        )
      })
    )
  } else {
    page
  }
}



