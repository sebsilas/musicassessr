



# Midi Notes

present_stimuli_midi_notes_auditory <- function(stimuli,
                                                note_length = 0.5,
                                                sound = "piano",
                                                page_type = 'null',
                                                play_button_text = "Play",
                                                stop_button_text = "Stop",
                                                asChord = FALSE,
                                                durations = numeric(),
                                                play_button_id = "playButton",
                                                button_area_id = "button_area",
                                                transpose_visual_notation = 0L,
                                                clef = "auto",
                                                sound_only_first_melody_note = FALSE,
                                                give_first_melody_note = FALSE,
                                                trigger_start_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus started!');"),
                                                trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
                                                first_note_message = psychTestR::i18n("first_note_is"),
                                                transposed_message = psychTestR::i18n("transposed"),
                                                play_first_note_button_text = psychTestR::i18n("play_first_note"),
                                                lowest_reading_note = NA, ...) {

  durations <- sort_durations(durations, note_length, stimuli)

  if(sound_only_first_melody_note) {
    stimuli <- stimuli[1]
    durations <- durations[1]
  }


  trigger_start_of_stimulus_fun <- NA_to_js_null(trigger_start_of_stimulus_fun)
  trigger_end_of_stimulus_fun <- NA_to_js_null(trigger_end_of_stimulus_fun)

  js_script <- paste0("playSeq(\'", play_button_id, "\', ", rjson::toJSON(stimuli),", ", rjson::toJSON(durations), ', \"', sound, '\", ', trigger_start_of_stimulus_fun, ', ', trigger_end_of_stimulus_fun, ')')

  play_button <- shiny::tags$button(play_button_text, id = play_button_id, onclick = js_script, class="btn btn-default")

  shiny::tags$div(
    # Should the first note be shown/played?
    show_first_melody_note(give_first_melody_note, stimuli, transpose_visual_notation, clef = clef, first_note_message = first_note_message,
                           transposed_message = transposed_message, play_first_note_button_text = play_first_note_button_text, lowest_reading_note = lowest_reading_note),
    set_melodic_stimuli(stimuli, durations),
    shiny::tags$div(id = button_area_id, play_button),
    shiny::tags$br()
  )

}

#' Set melodic stimuli
#'
#' @param stimuli
#' @param durations
#'
#' @return
#' @export
#'
#' @examples
set_melodic_stimuli <- function(stimuli, durations) {

  # Send stimuli to JS
  shiny::tags$script(
    htmltools::HTML(paste0('var stimuli = ', rjson::toJSON(stimuli), ';
                       Shiny.setInputValue("stimuli", JSON.stringify(stimuli));
                       var stimuli_durations = ', rjson::toJSON(durations), ';
                       Shiny.setInputValue("stimuli_durations", JSON.stringify(stimuli_durations));
                       ')
    )
  )
}






#' Present MIDI notes as musical notation
#'
#' @param stimuli
#' @param note_length
#' @param asChord
#' @param ascending
#' @param id
#' @param present_div
#' @param clef
#' @param transpose_visual_notation
#' @param audio_play_button_id
#' @param sheet_music_start_hidden
#' @param durations
#'
#' @return
#' @export
#'
#' @examples
present_stimuli_midi_notes_visual <- function(stimuli,
                                              note_length,
                                              asChord = FALSE,
                                              ascending, id = "sheet_music",
                                              present_div = TRUE,
                                              clef = "auto",
                                              transpose_visual_notation = 0L,
                                              audio_play_button_id = "playButton",
                                              sheet_music_start_hidden = FALSE,
                                              durations = NULL) {

  if(transpose_visual_notation != 0) {
    stimuli <- stimuli + transpose_visual_notation
  }


  if(identical(stimuli, "interactive")) {
    res <- shiny::tags$div(
      shiny::tags$div(id=id),
    )
  } else {
    xml <- wrap.xml.template(type = "midi_notes", notes = stimuli, asChord = asChord, clef = clef)
    res <- shiny::tags$div(open.music.display.wrapper(xml, id, present_div, sheet_music_start_hidden))
  }

  shiny::tags$div(
    res,
    set_melodic_stimuli(stimuli, durations)
  )

}



#' Present midi notes in both visual and auditory modalities
#'
#' @param stimuli
#' @param note_length
#' @param sound
#' @param asChord
#' @param play_button_text
#' @param ascending
#' @param visual_music_notation_id
#' @param play_button_id
#' @param button_area_id
#' @param sheet_music_start_hidden
#' @param sound_only_first_melody_note
#' @param sheet_music_id
#' @param page_type
#' @param durations
#' @param trigger_start_of_stimulus_fun
#' @param trigger_end_of_stimulus_fun
#' @param clef
#' @param first_note_message
#' @param transposed_message
#' @param play_first_note_button_text
#'
#' @return
#' @export
#'
#' @examples
present_stimuli_midi_notes_both <- function(stimuli, note_length = 0.5, sound = "piano", asChord = FALSE, play_button_text = "Play",
                                            ascending = TRUE, visual_music_notation_id = "sheet_music",
                                            play_button_id = "playButton", button_area_id = "button_area",
                                            sheet_music_start_hidden = FALSE, sound_only_first_melody_note = FALSE,
                                            sheet_music_id = 'sheet_music',
                                            page_type = 'null', durations = NULL,
                                            trigger_start_of_stimulus_fun  = wrap_js_fun_body("console.log('Stimulus started!');"),
                                            trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
                                            clef = "auto",
                                            first_note_message = psychTestR::i18n("first_note_is"),
                                            transposed_message = psychTestR::i18n("transposed"),
                                            play_first_note_button_text = psychTestR::i18n("play_first_note")) {



  return_stimuli_auditory <- present_stimuli_midi_notes_auditory(stimuli = stimuli, note_length = note_length,
                                                                 sound = sound, play_button_text = play_button_text,
                                                                 play_button_id = play_button_id, button_area_id = button_area_id,
                                                                 sound_only_first_melody_note = sound_only_first_melody_note,
                                                                 sheet_music_id = sheet_music_id,
                                                                 page_type = page_type, durations = durations,
                                                                 trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                                                 trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                                                 first_note_message = first_note_message,
                                                                 transposed_message = transposed_message, clef = clef,
                                                                 play_first_note_button_text = play_first_note_button_text)

  return_stimuli_visual <- present_stimuli_midi_notes_visual(stimuli = stimuli, note_length = note_length, asChord = asChord, ascending = ascending,
                                                             id = visual_music_notation_id, sheet_music_start_hidden = sheet_music_start_hidden, clef = clef)

  shiny::tags$div(return_stimuli_auditory, return_stimuli_visual)
}

present_stimuli_midi_notes <- function(stimuli,
                                       display_modality,
                                       note_length, sound = 'piano', asChord = FALSE, ascending, play_button_text = "Play",
                                       durations = NULL,
                                       visual_music_notation_id = "sheet_music", play_button_id = "playButton",
                                       button_area_id = "button_area", record_immediately = FALSE,
                                       transpose_visual_notation = 0L,
                                       sheet_music_start_hidden = FALSE,
                                       sound_only_first_melody_note = FALSE,
                                       sheet_music_id = 'sheet_music',
                                       page_type = 'null',
                                       clef = 'auto',
                                       give_first_melody_note = FALSE,
                                       trigger_start_of_stimulus_fun  = wrap_js_fun_body("console.log('Stimulus started!');"),
                                       trigger_end_of_stimulus_fun = wrap_js_fun_body("console.log('Stimulus finished!');"),
                                       first_note_message = psychTestR::i18n("first_note_is"),
                                       transposed_message = psychTestR::i18n("transposed"),
                                       play_first_note_button_text = psychTestR::i18n("play_first_note"),
                                       lowest_reading_note = NA, ...) {


  if (display_modality == "auditory") {
    return_stimuli <- present_stimuli_midi_notes_auditory(stimuli = stimuli, note_length = note_length, sound = sound,
                                                          play_button_text = play_button_text,
                                                          durations = durations,
                                                          play_button_id = play_button_id,
                                                          button_area_id = button_area_id, record_immediately = record_immediately,
                                                          transpose_visual_notation = transpose_visual_notation,
                                                          sound_only_first_melody_note = sound_only_first_melody_note,
                                                          page_type = page_type,
                                                          give_first_melody_note = give_first_melody_note,
                                                          trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                                          trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                                          first_note_message = first_note_message,
                                                          transposed_message = transposed_message,
                                                          play_first_note_button_text = play_first_note_button_text, clef = clef,
                                                          lowest_reading_note = lowest_reading_note, ...)

  } else if (display_modality == "visual") {
    return_stimuli <- present_stimuli_midi_notes_visual(stimuli = stimuli,
                                                        note_length = note_length,
                                                        asChord = asChord,
                                                        ascending = ascending,
                                                        id = visual_music_notation_id,
                                                        sheet_music_start_hidden = sheet_music_start_hidden,
                                                        durations = durations,
                                                        clef = clef,
                                                        transpose_visual_notation = transpose_visual_notation, ...)
  } else {
    return_stimuli <- present_stimuli_midi_notes_both(stimuli = stimuli, note_length = note_length, sound = sound,
                                                      asChord = asChord, ascending = ascending,
                                                      play_button_text = play_button_text, visual_music_notation_id = visual_music_notation_id,
                                                      play_button_id = play_button_id, button_area_id = button_area_id,
                                                      sheet_music_start_hidden = sheet_music_start_hidden,
                                                      sound_only_first_melody_note = sound_only_first_melody_note,
                                                      sheet_music_id = sheet_music_id,
                                                      page_type = page_type,
                                                      trigger_start_of_stimulus_fun = trigger_start_of_stimulus_fun,
                                                      trigger_end_of_stimulus_fun = trigger_end_of_stimulus_fun,
                                                      clef = clef,
                                                      first_note_message = first_note_message,
                                                      transposed_message = transposed_message,
                                                      play_first_note_button_text = play_first_note_button_text, ...)
  }

  return_stimuli
}


# Scientific music notation

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

present_stimuli_pitch_classes_visual <- function(stimuli, octave = 4L, asChord = FALSE) {

  xml <- wrap.xml.template(type = "pitch_classes", notes = stimuli, octave = octave, asChord = asChord)

  # deploy over music display wrapper
  open.music.display.wrapper(xml)

}

present_stimuli_pitch_classes_auditory <- function(stimuli, octave) {

}


present_stimuli_pitch_classes <- function(stimuli, display_modality, octave = 4L, ...) {

  if(display_modality == "visual") {
    return_stimuli <- present_stimuli_pitch_classes_visual(stimuli = stimuli, octave = octave, ...)
  } else {
    return_stimuli <- present_stimuli_pitch_classes_auditory(stimuli = stimuli, octave = octave, ...)
  }
}


# present rhythms (i.e non-pitched stimuli)

present_stimuli_rhythms <- function(stimuli_rhythms, ...) {
  # https://developer.aliyun.com/mirror/npm/package/tone-rhythm

  # set dummy pitch
  stimuli_pitches <- rep("C4", length(stimuli_rhythms))

  # return page
  shiny::tags$div(
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
                                         onclick=shiny::HTML(paste0("playMidiFile(\"",stimuli,"\", true, ",start_note,",",end_note,", true, this.id, ",transpose,", 'piano', ", bpm, ");")))
      ),
      shiny::tags$br()
    )


  }
  else {
    stop('Only support for auditory presentation of midi files currently')
  }

}


# .xml file (only visual curently)
present_stimuli_music_xml_file <- function(stimuli,
                                           display_modality,
                                           sound_only_first_melody_note = FALSE,
                                           sheet_music_start_hidden = FALSE,
                                           page_type = 'null') {

  if(display_modality %in% c("visual", "both")) {
    shiny::tags$div(
      open.music.display.wrapper(stimuli, sheet_music_start_hidden = sheet_music_start_hidden)
    )
  } else {
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
                          onset_vec1 = stimuli_durations,
                          pitch_vec2 = user_response,
                          #onset_vec2 = rep(.25, length(user_response)) # arrhythmic
                          onset_vec2 = user_response_timecodes) # rhythmic
      ## NB!!! need to get the actual onsets of the stimuli ^^^^

      # for arrhythmic?
      ng <- ngrukkon_safe(diff(stimuli), diff(user_response))

    }

    if (no_errors == 0 & no_correct == length(stimuli)) {
      accuracy <- 1
    } else {
      accuracy <- no_errors/length(user_response)
    }


    if(!is.null(answer$plot)) {
      plot <- renderPlot({ answer$plot }, width = 500)
    }  else {
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
      # for arrhythmic?
      ng <- ngrukkon_safe(diff(stimuli), diff(user_response))

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



wrap.xml.template <- function(notes, clef = "auto", asChord = FALSE, type = "midi_notes", octave = 4L) {

  mean_notes <- get_mean_of_notes(notes, type, octave)

  notes <- format.notes(type = type, notes = notes, asChord = asChord, octave = octave)

  if (clef == "treble") {
    clef <- "<sign>G</sign><line>2</line>"
  } else if (clef == "alto") {
    clef <- "<sign>C</sign><line>3</line>"
  } else if (clef == "bass") {
    clef <- "<sign>F</sign><line>4</line>"
  } else {
    clef <- choose_clef_from_mean(mean_notes)
  }

  res <- htmltools::HTML(paste0('<?xml version="1.0" encoding="UTF-8" standalone="no"?>
  <!DOCTYPE score-partwise PUBLIC
      "-//Recordare//DTD MusicXML 3.0 Partwise//EN"
      "http://www.musicxml.org/dtds/partwise.dtd">
  <score-partwise version="3.0">
    <part-list>
      <score-part id="P1">
        <part-name>Music</part-name>
      </score-part>
    </part-list>
    <part id="P1">
      <measure number="1">
        <attributes>
          <divisions>1</divisions>
          <key>
            <fifths>0</fifths>
          </key>
          <time>
            <beats>4</beats>
            <beat-type>4</beat-type>
          </time>
          <clef>',
                                clef,
                                '</clef>
        </attributes>', notes, '</measure>
    </part>
  </score-partwise>'))
}


format.accidentals.for.music.xml <- function(pitch_class_string){
  # take pitch class string, determine if sharp or flat
  # if so, return appropriate <alter> music xml element (-1 for flat, 1 for sharp)
  # if not, return empty string
  # also return the pitch class with the flat removed

  last_char <- itembankr::get_last_char_of_string(pitch_class_string)

  if (last_char == "b") {
    alter.text <- '<alter>-1</alter>'
    pitch.class <- itembankr::remove_last_char_of_string(pitch_class_string)
  }

  else if (last_char == "#") {
    alter.text <- '<alter>1</alter>'
    pitch.class <- itembankr::remove_last_char_of_string(pitch_class_string)
  }
  else {
    alter.text <- ''
    pitch.class <- pitch_class_string
  }

  list(alter.text, pitch.class)
}


format.notes.scientific_music_notation <- function(notes, asChord = FALSE) {

  res <- ""

  for(i in seq_along(notes)) {

    note <- itembankr::remove_last_char_of_string(notes[i])
    octave <- itembankr::get_last_char_of_string(notes[i])
    alter <- format.accidentals.for.music.xml(note)[[1]] # alters specifies if not sharp or flat
    note.without.sharp.or.flat <- format.accidentals.for.music.xml(note)[[2]]

    # https://www.musicxml.com/tutorial/the-midi-compatible-part/pitch/

    if (i == 1) {
      res <- paste0(res, '<note>
        <pitch>
        <step>', note.without.sharp.or.flat, '</step>',
                    alter,
                    '<octave>', octave, '</octave>
        </pitch>
        <duration>4</duration>
        <type>whole</type>
        </note>')

    }


    else {

      res <- paste0(res, '<note>',
                    ifelse(asChord, '<chord/>', ' '), # format as chord
                    '<pitch>
                <step>', note.without.sharp.or.flat, '</step>',
                    alter,
                    '<octave>', octave, '</octave>
                </pitch>
                <duration>4</duration>
                <type>whole</type>',
                    if(itembankr::get_last_char_of_string(
                      itembankr::remove_last_char_of_string(notes[i-1])) == "#" |
                      itembankr::get_last_char_of_string(
                        itembankr::remove_last_char_of_string(notes[i-1])) == "b") '<accidental>natural</accidental>',
                    '</note>')
    }

  }
  res

}


iformat.notes.pitch.classes <- function(notes, octave = 4L, asChord = FALSE) {

  res <- ""

  for(i in seq_along(notes)) {

    alter <- format.accidentals.for.music.xml(notes[i])[[1]] # alters specifies if not sharp or flat

    note.without.sharp.or.flat <- format.accidentals.for.music.xml(notes[i])[[2]]

    if (i == 1) {
      res <- paste0(res, '<note>
        <pitch>
        <step>', note.without.sharp.or.flat, '</step>',
                    alter,
                    '<octave>', octave, '</octave>
        </pitch>
        <duration>4</duration>
        <type>whole</type>
        </note>')

    }


    else {
      res <- paste0(res, '<note>',
                    ifelse(asChord, '<chord/>', ' '), # format as chord
                    '<pitch>
                <step>', note.without.sharp.or.flat, '</step>',
                    alter,
                    '<octave>', octave, '</octave>
                </pitch>
                <duration>4</duration>
                <type>whole</type>
                </note>')
    }

  }
  res
}



format.notes.midi <- function(notes, asChord = FALSE) {
  notes <- itembankr::midi_to_sci_notation(notes)
  res <- format.notes.scientific_music_notation(notes = notes, asChord = asChord)
}


format.notes <- function(type, notes, octave = 4L, asChord = FALSE) {
  if (type == "pitch_classes") {
    res <- format.notes.pitch.classes(notes, octave = octave, asChord = asChord)
  }
  else if (type == "scientific_music_notation") {
    # check if in correct format
    lapply(notes, itembankr::is_sci_notation)
    res <- format.notes.scientific_music_notation(notes = notes, asChord = asChord)
  }
  else if (type == "midi_notes") {
    res <- format.notes.midi(notes = notes, asChord = asChord)
  }

  else {
    stop('Unrecognised notation format. Must be one of pitch_classes, scientific_music_notation or midi_notes')
  }
  res

}


play.notes.html.wrapper <- function(stimuli_pitches, stimuli_rhythms) {

  # https://developer.aliyun.com/mirror/npm/package/tone-rhythm

  shiny::tags$div(shiny::tags$button("Play", id = "playNotes"),
                  shiny::tags$script(htmltools::HTML(paste0('

        var synth = new Tone.Synth().toMaster();

        var {
      getBarsBeats,
      addTimes,
      getTransportTimes,
      mergeMusicDataPart
      } = toneRhythm.toneRhythm(Tone.Time); ',
                                                            'var rhythms = ', rjson::toJSON(stimuli_rhythms), '; ',
                                                            'var transportTimes = getTransportTimes(rhythms);
                  var pitches = ', rjson::toJSON(stimuli_pitches), '; ',
                                                            'var mergedData = mergeMusicDataPart({
                rhythms: rhythms,
                notes: pitches,
                startTime: \'0:3:2\'
              });

              var melodyPart = new Tone.Part((time, value) => {
          synth.triggerAttackRelease(value.note, value.duration, time);
          }, mergedData).start(0);

          var playButton = document.getElementById(\'playNotes\');
          playButton.onclick = function() { Tone.Transport.start(); };

                    '))))

}

open.music.display.wrapper <- function(xml, id = "sheet_music", return_div = TRUE, sheet_music_start_hidden = FALSE) {

  non_underscore_id <- stringr::str_remove(id, "_")

  shiny::tags$div(
    shiny::tags$br(),
    if(return_div) shiny::tags$div(id=id, style = if(sheet_music_start_hidden) "visibility: hidden;" else "visibility: visible;"),
    shiny::tags$script(htmltools::HTML(paste0('
                var ', id, '_osmd = new opensheetmusicdisplay.OpenSheetMusicDisplay(\"', id, '\", {drawingParameters: "compact",
                drawPartNames: false, drawMeasureNumbers: false, drawMetronomeMarks: false});
                var loadPromise = ', id, '_osmd.load(`',xml,'`);
                              loadPromise.then(function(){
                              var ', non_underscore_id, ' = document.getElementById("', id, '");
                              ', id, '_osmd.render();
                              var scoreWidth = String(parseInt(', id, '_osmd.graphic.musicPages[0].musicSystems[0].PositionAndShape.size.width)*10);
                              scoreWidth = scoreWidth.concat("px");
                              ', non_underscore_id, '.style.width = scoreWidth;
                              });'))),

    if(sheet_music_start_hidden) shiny::tags$script(shiny::HTML(paste0('setTimeout(() => { var sm = document.getElementById("',id, '");  sm.style.visibility = "hidden"; } , 200);')))
  )

}


sort_durations <- function(durations, note_length, stimuli) {
  if(length(durations) == 0) {
    durations <- rep(note_length, length(stimuli))
  }
  durations
}

show_first_melody_note <- function(give_first_melody_note,
                                   stimuli,
                                   transpose_visual_notation = 0L,
                                   clef = "auto",
                                   show_first_melody_note_visual = TRUE,
                                   audio_play_button_id = "firstMelodyPlay",
                                   first_note_message = psychTestR::i18n("first_note_is"),
                                   transposed_message = psychTestR::i18n("transposed"),
                                   play_first_note_button_text = psychTestR::i18n("play_first_note"),
                                   lowest_reading_note = NA) {

  if(transpose_visual_notation  != 0L) {
    transposed_visual_note <- stimuli[1] + transpose_visual_notation
  } else {
    transposed_visual_note <- stimuli[1]
  }

  if(!is.na(lowest_reading_note)) {
    if(transposed_visual_note < lowest_reading_note) {
      transposed_visual_note <- transposed_visual_note + 12
    }
  }

  if(give_first_melody_note) {
    shiny::tags$div(
      id = "first_note",
      shiny::tags$p(first_note_message),
      if(transpose_visual_notation != 0L) shiny::tags$p(transposed_message),
      if(show_first_melody_note_visual) present_stimuli_midi_notes_visual(transposed_visual_note, clef = clef, id = "firstMelodyNoteVisual"),
      present_stimuli_midi_notes_auditory(stimuli[1], play_button_text = play_first_note_button_text, clef = clef,
                                          play_button_id = audio_play_button_id, transpose_visual_notation = 0L)
    )
  } else {
    return(" ")
  }
}
