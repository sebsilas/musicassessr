
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

# range functions

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


#' Get the default range for an instrument
#'
#' @param instrument
#'
#' @return
#' @export
#'
#' @examples
default_range <- function(instrument) {

  inst <- insts_table %>% dplyr::filter(en == instrument)

  if(nrow(inst) == 0) {
    stop("Instrument not found")
  } else if(nrow(inst) > 1) {
    stop("Instrument not unique")
  } else {
    list(bottom_range = inst$low_note, top_range = inst$high_note)
  }

}



