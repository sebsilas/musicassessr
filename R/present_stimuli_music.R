

# midi notes


present_stimuli_midi_notes_auditory <- function(stimuli, note_length = 0.5, sound = "piano",
                                                page_type = 'null', play_button_text = "Play",
                                                stop_button_text = "Stop",
                                                record_audio_method = "aws_pyin",
                                                asChord = FALSE, dur_list = 'null', auto_next_page = FALSE,
                                                play_button_id = "playButton", button_area_id = "button_area", ...) {

  if(page_type == "record_audio_page") {
    page_type <- record_audio_method
  }

  if(dur_list != 'null') {
    dur_list <- rjson::toJSON(dur_list)
  }

  if(sound == "tone") {
    js.script <- paste0('playTone(',stimuli,', ', note_length,', this.id, \'tone\');')
  } else {

    if (length(stimuli) == 1 & is.character(stimuli) == FALSE) {
      melody.for.js <- hrep::midi_to_freq(stimuli-12) # there is a bug where the piano plays up an octave
      js.script <- sprintf("triggerNote(\"%s\", %s, %s);", sound, melody.for.js, note_length)
    }
    else {
      melody.for.js <- rjson::toJSON(stimuli)
      js.script <- paste0("playSeq(",melody.for.js,", true, this.id, \'",sound,"\', \"", page_type, "\", \"", stop_button_text, "\", ", dur_list, ");")
    }
  }


  shiny::tags$div(
    # send stimuli to js
    shiny::tags$script(paste0('var stimuli = ', rjson::toJSON(stimuli), ';
                       Shiny.setInputValue("stimuli", JSON.stringify(stimuli));
                       ')),
    shiny::tags$div(id=button_area_id,
                    shiny::tags$button(play_button_text, id = play_button_id, onclick=js.script, class="btn btn-default")
    ))

}


#' Present MIDI notes as musical notation
#'
#' @param stimuli
#' @param note_length
#' @param asChord
#' @param ascending
#'
#' @return
#' @export
#'
#' @examples
present_stimuli_midi_notes_visual <- function(stimuli, note_length, asChord = FALSE, ascending, id = "sheet_music", present_div = TRUE) {

  if (stimuli == "interactive") {
    res <- shiny::tags$div(
      shiny::tags$div(id=id),
    )
  }

  else {
    xml <- wrap.xml.template(type = "midi_notes", notes = stimuli, asChord = asChord)
    res <- open.music.display.wrapper(xml, id, present_div)
  }
  res

}

#' Present midi notes in both visual and auditory modalities
#'
#' @param stimuli
#' @param note_length
#' @param sound
#' @param asChord
#' @param play_button_text
#' @param ascending
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
present_stimuli_midi_notes_both <- function(stimuli, note_length, sound = "piano", asChord = FALSE, play_button_text = "Play",
                                            ascending = TRUE, visual_music_notation_id = "sheet_music",
                                            play_button_id = "playButton", button_area_id = "button_area", ...) {

  return_stimuli_auditory <- present_stimuli_midi_notes_auditory(stimuli = stimuli, note_length = note_length,
                                                                 sound = sound, play_button_text = play_button_text,
                                                                 play_button_id = play_button_id, button_area_id = button_area_id, ...)
  return_stimuli_visual <- present_stimuli_midi_notes_visual(stimuli = stimuli, note_length = note_length, asChord = asChord, ascending = ascending, id = visual_music_notation_id)

  shiny::tags$div(return_stimuli_auditory, return_stimuli_visual)
}

present_stimuli_midi_notes <- function(stimuli, display_modality, note_length, sound = 'piano', asChord = FALSE, ascending, play_button_text = "Play",
                                       record_audio_method = "aws_pyin",  dur_list = 'null', auto_next_page = FALSE,
                                       visual_music_notation_id = "sheet_music", play_button_id = "playButton",
                                       button_area_id = "button_area", ...) {

  if (display_modality == "auditory") {
    return_stimuli <- present_stimuli_midi_notes_auditory(stimuli = stimuli, note_length = note_length, sound = sound,
                                                          play_button_text = play_button_text,
                                                          record_audio_method =  record_audio_method, dur_list = dur_list,
                                                          auto_next_page = auto_next_page, play_button_id = play_button_id,
                                                          button_area_id = button_area_id, ...)

  }
  else if (display_modality == "visual") {
    return_stimuli <- present_stimuli_midi_notes_visual(stimuli = stimuli,
                                                        note_length = note_length,
                                                        asChord = asChord,
                                                        ascending = ascending, auto_next_page = auto_next_page,
                                                        id = visual_music_notation_id)
  }
  else {
    return_stimuli <- present_stimuli_midi_notes_both(stimuli = stimuli, note_length = note_length, sound = sound,
                                                      asChord = asChord, ascending = ascending,
                                                      play_button_text = play_button_text,
                                                      record_audio_method = record_audio_method,
                                                      visual_music_notation_id = visual_music_notation_id,
                                                      play_button_id = play_button_id, button_area_id = button_area_id, ...)
  }

  return_stimuli
}


# scientific music notation

present_stimuli_scientific_music_notation_visual <- function(stimuli, asChord = FALSE) {

  xml <- wrap.xml.template(type = "scientific_music_notation", notes = stimuli, asChord = asChord)

  open.music.display.wrapper(xml)

}

present_stimuli_scientific_music_notation_auditory <- function(stimuli, note_length, sound) {

  if(class(stimuli) == "list") {
    stimuli_rhythms <- stimuli[["rhythms"]]
    stimuli_pitches <- stimuli[["scientific_music_notation"]]
  }

  else if(is.null(stimuli_pitches)) {
      stimuli_pitches <- rep("C4", length(stimuli_rhythms))
  }

  else {
    stimuli_pitches <- stimuli
  }

    # return page
      shiny::tags$div(
        play.notes.html.wrapper(stimuli_pitches, stimuli_rhythms)
      )
}

present_stimuli_scientific_music_notation <- function(stimuli, display_modality, note_length = 0.5, sound = "piano") {

  if (display_modality == "auditory") {
    return_stimuli <- present_stimuli_scientific_music_notation_auditory(stimuli = stimuli, note_length = note_length, sound = sound)
  }
  else {
    return_stimuli <- present_stimuli_scientific_music_notation_visual(stimuli = stimuli)
  }

  return_stimuli
}



# pitch classes

present_stimuli_pitch_classes_visual <- function(stimuli, octave = 4, asChord = FALSE) {

  xml <- wrap.xml.template(type = "pitch_classes", notes = stimuli, octave = octave, asChord = asChord)

  # deploy over music display wrapper
  open.music.display.wrapper(xml)

}

present_stimuli_pitch_classes_auditory <- function(stimuli, octave) {

}


present_stimuli_pitch_classes <- function(stimuli, display_modality, octave = 4, ...) {

  if(display_modality == "visual") {
    return_stimuli <- present_stimuli_pitch_classes_visual(stimuli = stimuli,
                                                           octave = octave, ...)

  } else {
    return_stimuli <- present_stimuli_pitch_classes_auditory(stimuli = stimuli,
                                                             octave = octave, ...)
  }
}


# present rhythms (i.e non-pitched stimuli)

present_stimuli_rhythms <- function(stimuli_rhythms) {
  # https://developer.aliyun.com/mirror/npm/package/tone-rhythm

  # set dummy pitch
  stimuli_pitches <- rep("C4", length(stimuli_rhythms))

  # return page
    div(
    # load scripts
    # wrap html
    play.notes.html.wrapper(stimuli_pitches, stimuli_rhythms)
  )
}

# file presentation functions

# .mid file (only auditory currently)



grab.stimuli.number.from.file.path <- function(file_path) {
  d <- strsplit(file_path, ".", fixed = TRUE)[[1]][1]
  d <- strsplit(d, "/", fixed = TRUE)[[1]]
  d <- d[length(d)]
  num <- as.numeric(paste0(stringr::str_extract_all(d, "[0-9]")[[1]], collapse = ""))
}


present_stimuli_midi_file <- function(stimuli, display_modality, button_text = "Play", transpose = 0, start_note = 1, end_note = "end", bpm = 85, ...) {

  if(end_note == "end") {
    end_note <- '\"end\"'
  }
  # 0 indexing for JS (but not needed for end_note)
  start_note <- start_note-1

  if(display_modality == "auditory") {

    shiny::tags$div(

      #shiny::tags$script(paste0('var stimuli = ', rjson::toJSON(stimuli_for_js))),

      shiny::tags$div(id="button_area",
                      shiny::tags$button(button_text, id="playButton", class="btn btn-default",
                                         onclick=shiny::HTML(paste0("playMidiFileAndRecordAfter(\"",stimuli,"\", true, ",start_note,",",end_note,", true, this.id, ",transpose,", 'piano', ", bpm, ");")))
      ),
    shiny::tags$br()
    )


  }
  else {
    stop('Only support for auditory presentation of midi files currently')
  }

}


# .xml file (only visual curently)
present_stimuli_music_xml_file <- function(stimuli, display_modality) {

  if(display_modality == "visual") {

    open.music.display.wrapper(stimuli)

  }
  else {
    stop('Only support for visual presentation of musicxml files currently')
  }

}



display_previous_answer_music_notation_pitch_class <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {

    # grab response from previous trial
    note_no <- answer[[5]] # this has to be before the next line
    stimuli <- answer[[1]][1:note_no]
    user_response <- answer[[2]]
    user_response_timecodes <- round(answer[[4]]/1000, 2)
    stimuli_durations <- answer[[11]]

    # calculate some other info
    trial_length <- user_response_timecodes[length(user_response_timecodes)]
    no_correct <- sum(as.numeric(user_response %in% stimuli))
    no_errors <- length(user_response) - no_correct


    if(length(user_response) < 3) {
      similarity <- "Not enough notes"
      ng <- "Not enough notes"
    }
    else {

      similarity <- opti3(pitch_vec1 = stimuli,
                          dur_vec1 = stimuli_durations,
                          pitch_vec2 = user_response,
                          #dur_vec2 = rep(.25, length(user_response)) # arrhythmic
                          dur_vec2 = user_response_timecodes) # rhythmic
      ## NB!!! need to get the actual onsets of the stimuli ^^^^

      # for arrhythmic?
      ng <- ngrukkon(stimuli, user_response)

    }



    if (no_errors == 0 & no_correct == length(stimuli)) {
      accuracy <- 1
    }
    else {
      accuracy <- no_errors/length(user_response)
    }


    if(!is.null(answer$plot)) {
      plot <- renderPlot({ answer$plot }, width = 500)
    }
    else {
      plot <- " "
    }

    if(!is.null(answer$rms_plot)) {
      rms.plot <- shiny::renderPlot({ answer$rms_plot }, width = 500)
    }
    else {
      rms.plot <- " "
    }

    # pitch classes
    present_stimuli(stimuli = user_response,
                    stimuli_type = "midi_notes",
                    display_modality = "auditory",
                    page_title = "Feedback: ",
                    page_text = div(tags$p(paste0("Similarity was ", similarity)),
                                    tags$p(paste0("No correct: ", no_correct)),
                                    tags$p(paste0("Number of errors: ", no_errors)),
                                    tags$p(paste0("Accuracy (error by note events): ", accuracy)), # add then subtract 1 to stop possibility of dividing 0
                                    tags$p(paste0("Time taken: ", trial_length, " seconds.")),
                                    tags$p(plot),
                                    tags$p(rms.plot)
                    )
    )


  })
}


display_previous_answer_music_notation_pitch_class_aws <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {


    user_response <- answer$user_pitch

    # pitch classes
    present_stimuli(stimuli = user_response,
                    stimuli_type = "midi_notes",
                    display_modality = "both",
                    page_title = "Feedback: ",
                    page_text = tags$p("You played: ")
                    )

  })
}






display_previous_answer_music_notation_pitch_class2 <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {

    print('display_previous_answer_music_notation_pitch_class2!!')
    print(answer)

    stimuli <- answer$stimuli
    user_response <- answer$user_response_notes
    user_response_timecodes <- 1:length(user_response)

    # calculate some other info
    trial_length <- user_response_timecodes[length(user_response_timecodes)]
    no_correct <- sum(as.numeric(user_response %in% stimuli))
    no_errors <- length(user_response) - no_correct


    if(length(user_response) < 3) {
      similarity <- "Not enough notes"
      ng <- "Not enough notes"
    }
    else {

      # similarity <- opti3(pitch_vec1 = stimuli,
      #                     dur_vec1 = stimuli_durations,
      #                     pitch_vec2 = user_response,
      #                     #dur_vec2 = rep(.25, length(user_response)) # arrhythmic
      #                     dur_vec2 = user_response_timecodes) # rhythmic
      ## NB!!! need to get the actual onsets of the stimuli ^^^^

      # for arrhythmic?
      ng <- ngrukkon(stimuli, user_response)

    }



    if (no_errors == 0 & no_correct == length(stimuli)) {
      accuracy <- 1
    }
    else {
      accuracy <- no_errors/length(user_response)
    }


    # if(!is.null(answer$plot)) {
    #   plot <- renderPlot({ answer$plot }, width = 500)
    # }
    # else {
    #   plot <- " "
    # }

    # pitch classes
    present_stimuli(stimuli = user_response,
                    stimuli_type = "midi_notes",
                    display_modality = "both",
                    page_title = "Feedback: ",
                    page_text = div(tags$p(paste0("Similarity was ", ng)),
                                    tags$p(paste0("No correct: ", no_correct)),
                                    tags$p(paste0("Number of errors: ", no_errors)),
                                    tags$p(paste0("Accuracy (error by note events): ", accuracy)), # add then subtract 1 to stop possibility of dividing 0
                                    tags$p(paste0("Time taken: ", trial_length, " seconds."))
                                    #tags$p(plot),
                    )
    )


  })
}


display_previous_answer_music_notation_pitch_class_aws <- function() {
  # since this uses the pitch class present stimuli type, this will return in a "presentable" octave
  psychTestR::reactive_page(function(state, answer, ...) {


    user_response <- answer$user_pitch

    # pitch classes
    present_stimuli(stimuli = user_response,
                    stimuli_type = "midi_notes",
                    display_modality = "both",
                    page_title = "Feedback: ",
                    page_text = tags$p("You played: ")
    )

  })
}



