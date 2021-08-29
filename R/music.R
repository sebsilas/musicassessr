
find.closest.value <- function(x, vector, return_value) {
  # given a value, x, and a vector of values,
  # return the index of the value in the vector, or the value itself, which is closest to x
  # if return_value == TRUE, return the value, otherwise the index
  res <- base::which.min(abs(vector - x))
  res <- ifelse(return_value, vector[res], res)
}


get.all.octaves.in.gamut <- function(note, gamut_min = midi.gamut.min, gamut_max = midi.gamut.max) {

  # given a note and a range/gamut, find all midi octaves of that note within the specified range/gamut
  res <- c(note)

  # first go down
  while(note > gamut_min) {
    note <- note - 12
    res <- c(res, note)
  }
  # then go up
  while(note < gamut_max) {
    note <- note + 12
    res <- c(res, note)
  }
  res <- res[!duplicated(res)]
  res <- res[order(res)]
}


find.closest.stimuli.pitch.to.user.production.pitches <- function(stimuli_pitches, user_production_pitches, allOctaves = TRUE) {

  # if allOctaves is true, get the possible pitches in all other octaves. this should therefore resolve issues
  # where someone was presented stimuli out of their range and is penalised for it
  if (allOctaves == TRUE) {
    res <- sapply(user_production_pitches, find.closest.value, get.all.octaves.in.gamut(stimuli_pitches), return_value = TRUE)
  } else {
    res <- sapply(user_production_pitches, find.closest.value, stimuli_pitches, return_value = TRUE)
  }
  res
}



# and some singing accuracy metrics on read in
cents <- function(notea, noteb) {
  # get the cents between two notes (as frequencies)
  res <- 1200 * log2(noteb/notea)
  res
}

vector.cents <- function(reference_note, vector_of_values) {
  # given a vector of values and a target note, give the cents of the vector note relative to the target note
  res <- vapply(vector_of_values, cents, "notea" = reference_note, FUN.VALUE = 100.001)
  res
}

vector.cents.between.two.vectors <- function(vectora, vectorb) {
  # for each note (as a freq) in a vector, get the cents difference of each note in vector A and vector B
  res <- c()
  for (n in 1:length(vectora)) {
    cent_res <- cents(vectora[n], vectorb[n])
    res <- c(res, cent_res)
  }
  res
}


vector.cents.first.note <- function(vector_of_values) {
  # given a vector of frequencies, give the cents relative to the first note
  res <- vapply(vector_of_values, cents, "notea" = vector_of_values[1], FUN.VALUE = 100.001)
  res
}



choose_clef_from_mean <- function(mean_notes) {
  if(mean_notes >= 60) {
    clef <- "<sign>G</sign><line>2</line>"
  }
  else {
    clef <- "<sign>F</sign><line>4</line>"
  }
}

get_mean_of_notes <- function(notes, type, octave = NULL) {

  if(type == "scientific_music_notation") {
    round(mean(itembankr::sci_notation_to_midi(notes)))
  } else if(type == "midi_notes") {
    round(mean(notes))
  } else if(type == "pitch_classes") {
    round(mean(itembankr::sci.notation.to.midi.list[base::endsWith(names(itembankr::sci.notation.to.midi.list), as.character(octave))]))
  } else{
    warning("Unknown type")
  }
}


wrap.xml.template <- function(notes, clef = "auto", asChord = FALSE, type = "midi_notes", octave = 4) {

  mean_notes <- get_mean_of_notes(notes, type, octave)

  notes <- format.notes(type = type, notes = notes, asChord = asChord, octave = octave)

  if (clef == "treble") {
    clef <- "<sign>G</sign><line>2</line>"
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
                <type>whole</type>
                </note>')
    }

  }
  res

}


format.notes.pitch.classes <- function(notes, octave = 4, asChord = FALSE) {

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


format.notes <- function(type, notes, octave = 4, asChord = FALSE) {
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

open.music.display.wrapper <- function(xml, id = "sheet-music", return_div = TRUE) {

  non_underscore_id <- stringr::str_remove(id, "_")

  shiny::tags$div(
    shiny::tags$br(),
    if(return_div) shiny::tags$div(id=id),
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
                              });'))))
}


# range functions

get_instrument_range <- function(inst) {
  insts_table[insts_table$Instruments == inst, c("low_note", "high_note")]
}

set_instrument_range_code_block <- function(inst = NULL) {
  code_block(function(state, ...) {
    print('setting MIDI pages default')
    if(is.null(inst)) {
      inst <- get_global("inst", state)
    }
    set_global("bottom_range", get_instrument_range(inst)$low_note, state)
    set_global("top_range", get_instrument_range(inst)$high_note, state)
  })
}



produce_stimuli_in_range <- function(rel_melody, bottom_range = 21, top_range = 108) {
  # given some melodies in relative format, and a user range, produce random transpositions which fit in that range

  rel_melody <- str_mel_to_vector(rel_melody, sep = ",")
  dummy_abs_mel <- rel_to_abs_mel(rel_melody, start_note = 1)
  mel_range <- range(dummy_abs_mel)
  span <- sum(abs(mel_range))


  if(span > top_range - bottom_range) {
    stop('The span of the stimuli is greater than the range of the instrument. It is not possible to play on this instrument.')
  }

  gamut <- bottom_range:top_range
  gamut_clipped <- (bottom_range+span):(top_range-span)
  random_abs_mel <- 200:210  # just instantiate something out of range

  while(any(!random_abs_mel %in% gamut)) {
    # resample until a melody is found that sits in the range
    random_abs_mel_start_note <- sample(gamut_clipped, 1)
    random_abs_mel <- rel_to_abs_mel(rel_melody, start_note = random_abs_mel_start_note)
  }
  random_abs_mel
}



#find.closest.value(14, c(1, 6, 12, 28, 33), TRUE)

#as <- get.all.octaves.in.gamut(41, midi.gamut.min, midi.gamut.max)

#as2 <- unlist(lapply(c(51, 39, 41, 43), function(x) get.all.octaves.in.gamut(x, midi.gamut.min, midi.gamut.max)))


# get_instrument_range("Alto Saxophone")


