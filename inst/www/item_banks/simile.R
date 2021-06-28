library(hrep)
library(tidyverse)

# constants
midi.gamut <- 21:108
midi.gamut.min <- midi.gamut[1]
midi.gamut.max <- midi.gamut[length(midi.gamut)]
midi.to.pitch.classes.numeric.list <- as.list(rep(as.integer(1:12), 8)[midi.gamut])
names(midi.to.pitch.classes.numeric.list) <- midi.gamut



find.closest.value <- function(x, vector, return_value) {
  # given a value, x, and a vector of values,
  # return the index of the value in the vector, or the value itself, which is closest to x
  # if return_value == TRUE, return the value, otherwise the index
  res <- base::which.min(abs(vector - x))

  res <- ifelse(return_value == TRUE, vector[res], res)

  res
}

#find.closest.value(14, c(1, 6, 12, 28, 33), TRUE)

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
  res
}

#as <- get.all.octaves.in.gamut(41, midi.gamut.min, midi.gamut.max)

#as2 <- unlist(lapply(c(51, 39, 41, 43), function(x) get.all.octaves.in.gamut(x, midi.gamut.min, midi.gamut.max)))


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


# constants

pitch.classes <- c("A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab")
midi.sci.notation.nos <- c(rep(0,3),rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12), 8)
scientific.pitch.classes <- paste0(pitch.classes, midi.sci.notation.nos)

midi.to.pitch.classes.list <- as.list(rep(pitch.classes, 8)[c(1:88)])
names(midi.to.pitch.classes.list) <- c(21:108)

midi.to.pitch.classes.numeric.list <- as.list(rep(as.integer(1:12), 8)[c(1:88)])
names(midi.to.pitch.classes.numeric.list) <- c(21:108)

midi.to.sci.notation.list <- scientific.pitch.classes
names(midi.to.sci.notation.list) <- c(21:108)

pitch.class.to.midi.list <- list(c(21, 33, 45, 57, 69, 81, 93, 105),
                                 c(22, 34, 46, 58, 70, 82, 94, 106),
                                 c(23, 35, 47, 59, 71, 83, 95, 107),
                                 c(24, 36, 48, 60, 72, 84, 96, 108),
                                 c(25, 37, 49, 61, 73, 85, 97),
                                 c(26, 38, 50, 62, 74, 86, 98),
                                 c(27, 39, 51, 63, 75, 87, 99),
                                 c(28, 40, 52, 64, 76, 88, 100),
                                 c(29, 41, 53, 65, 77, 89, 101),
                                 c(30, 42, 54, 66, 78, 90, 102),
                                 c(31, 43, 55, 67, 79, 91, 103),
                                 c(32, 44, 56, 68, 80, 92, 104)
                                 )

names(pitch.class.to.midi.list) <- pitch.classes

pitch.class.to.midi.notes <- function(pitch_class) {
  pitch.class.to.midi.list[[pitch_class]]
}

# functions

midi.to.pitch.class <- function(midi_note) {

  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.list[[as.character(x)]]))
  }
  pitch_class
}


midi.to.pitch.class.numeric <- function(midi_note) {

  if (length(midi_note) == 1) {
    pitch_class <- midi.to.pitch.classes.numeric.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.pitch.classes.numeric.list[[as.character(x)]]))
  }
  pitch_class
}


midi.to.sci.notation <- function(midi_note) {

  if (length(midi_note) == 1) {
    pitch_class <- midi.to.sci.notation.list[[as.character(midi_note)]]
  }
  else {
    pitch_class <- unlist(lapply(midi_note, function(x) midi.to.sci.notation.list[[as.character(x)]]))
  }
  pitch_class
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


### begin original

messagef <- function(...) message(sprintf(...))
#pc_labels <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "Bb", "B")
pc_labels <- c("C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B")
pc_labels_sharp <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
pc_labels_flat <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

asc <- function(x, n = 1){
  raw <- charToRaw(x)
  if(n < 0){
    n <- length(raw) + n + 1
  }
  if(n == 0){
    return(strtoi(raw, 16))
  }
  strtoi(raw, 16)[n]
}

edit_dist <- function(s, t){
  adist(s,t)[1,1]
}

edit_sim <- function(s, t){
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}

get_all_ngrams <- function(x, N = 3){
  l <- length(x) - N + 1
  stopifnot(l > 0)
  map_df(1:l, function(i){
    ngram <- x[i:(i + N - 1)]
    tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
}

#as in  Müllensiefen & Frieler (2004)
ngrukkon <- function(x, y, N = 3){
  #browser()
  x <- get_all_ngrams(x, N = N) %>% pull(value)
  y <- get_all_ngrams(y, N = N) %>% pull(value)
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  1 - sum(abs(tx  - ty))/(length(x) + length(y))
}

#Krumhansl-Schmuckler algorithm
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = T){
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    return(
      map_dfr(s, function(x){
        #browser()
        pv <- pitch_vec[segmentation == x]
        tibble(segment = x, key = get_implicit_harmonies(pv, NULL, only_winner = only_winnter) %>%   pull(key))
      })
    )

  }
  pitch_freq <- table(factor(pitch_vec  %% 12, levels = 0:11))
  correlations <-
    map_dfr(0:11, function(t){
      #browser()
      w_major <- cor.test(pitch_freq, ks_weights_major[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% pull(estimate)
      w_minor <- cor.test(pitch_freq, ks_weights_minor[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% pull(estimate)
      bind_rows(tibble(transposition = t,  match = w_major, type = "major", key = sprintf("%s-maj", pc_labels[t+1])),
                tibble(transposition = t,  match = w_minor, type = "minor", key = sprintf("%s-min", pc_labels[t+1])))
    }) %>% arrange(desc(match))
  if(only_winner){
    return(correlations[1,])
  }
  correlations
}
bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10){
  l <-length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    map_dfr(1:size, function(x){
      pv <- sample(pitch_vec, replace = T, sample_size)
      get_implicit_harmonies(pv, only_winner = T)
    })
  best_key <- bs %>% count(key) %>% arrange(desc(n)) %>% pull(key)
  bs %>% filter(key == best_key[1]) %>% head(1)
}

classify_duration <- function(dur_vec, ref_duration = .5){
  rel_dur <- dur_vec/ref_duration
  rhythm_class <- rep(-2, length(rel_dur))
  #rhythm_class[rel_dur <= .45] <- -2
  rhythm_class[rel_dur > 0.45] <- -1
  rhythm_class[rel_dur > 0.9] <- 0
  rhythm_class[rel_dur > 1.8] <- 1
  rhythm_class[rel_dur > 3.3] <- 2
  rhythm_class
}

rhythfuzz <- function(dur_vec1, dur_vec2){
  #browser()
  edit_sim(intToUtf8(dur_vec1 + 128), intToUtf8(dur_vec2 + 128))
}

harmcore <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL){
  #browser()
  implicit_harm1 <- get_implicit_harmonies(pitch_vec1, segmentation1) %>% pull(key)
  implicit_harm2 <- get_implicit_harmonies(pitch_vec2, segmentation2) %>% pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}

harmcore2 <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL){
  implicit_harm1 <- bootstrap_implicit_harmonies(pitch_vec1, segmentation1) %>% pull(key)
  implicit_harm2 <- bootstrap_implicit_harmonies(pitch_vec2, segmentation2) %>% pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}

#little helper to calculate modus of simple vector
modus <- function(x){
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){
  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = T)
  key1 <- ih1 %>% pull(key)
  pc1 <- ih1 %>% pull(transposition)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = T)
  pc2 <- ih2 %>% pull(transposition)
  key_diff <- (pc2 -  pc1) %% 12
  #messagef("Best key 1 = %s, best key 2 = %s, key diff = %d", key1, ih2 %>% head(1) %>% pull(key), key_diff )
  modus1 <- modus(pitch_vec1)
  modus2 <- modus(pitch_vec2)
  ret <- c(modus1 - modus2,
           round(mean(pitch_vec1)) - round(mean(pitch_vec2)),
           round(median(pitch_vec1)) - round(median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #messagef("Octave offset = %d", octave_offset)
  ret <- c(0, ret, octave_offset*12 + key_diff, octave_offset*12 + 12 - key_diff)
  unique(ret) %>% sort()

}
#finds transposition that maximize raw edit distance of two pitch vectors
#transposision in semitone of the *second* melody
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- map_dfr(trans_hints, function(x){
    #browser()
    tibble(transposition = x, sim = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% arrange(sim) %>% head(1) %>% pull(transposition)
}

opti3 <- function(pitch_vec1, dur_vec1, pitch_vec2, dur_vec2, N = 3, use_bootstrap = T){
  pitch_vec1 <- round(pitch_vec1)
  pitch_vec2 <- round(pitch_vec2)
  v_ngrukkon <- ngrukkon(pitch_vec1, pitch_vec2, N = N)
  dur_vec1 <- classify_duration(dur_vec1)
  dur_vec2 <- classify_duration(dur_vec2)
  v_rhythfuzz <- rhythfuzz(dur_vec1, dur_vec2)

  if(use_bootstrap){
    v_harmcore <- harmcore2(pitch_vec1, pitch_vec2)
  }
  else{
    v_harmcore <- harmcore(pitch_vec1, pitch_vec2)

  }
  opti3 <- 0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146

  #messagef("ngrukkon = %.3f, rhythfuzz = %.3f, harmcor = %.3f, opti3 = %.3f",
  #         v_ngrukkon, v_rhythfuzz, v_harmcore, opti3)

  opti3 <- max(min(opti3, 1), 0)

}

#read a pYIN note track and make it nice
read_melody <- function(fname){
  melody <-
    read.csv(fname, header = F) %>%
    as_tibble() %>%
    rename(onset = V1, freq = V3, dur = V2) %>%
    ## NB! switched columns in above line for sonic annotator PYIN! if output is from Tony, try rename(onset = V1, freq = V2, dur = V3)
    mutate(pitch = round(freq_to_midi(freq)),
           #cents_from_first_note = vector.cents.first.note(round(freq_to_midi(freq))),
           cents_deviation_from_nearest_midi_pitch = vector.cents.between.two.vectors(round(midi_to_freq(freq_to_midi(freq))), freq),
           # the last line looks tautological, but, by converting back and forth, you get the quantised pitch and can measure the cents deviation from this
           pitch_class = midi.to.pitch.class(round(freq_to_midi(freq))),
           pitch_class_numeric = midi.to.pitch.class.numeric(round(freq_to_midi(freq))),
           sci_notation = midi.to.sci.notation(round(freq_to_midi(freq))),
           interval = c(NA, diff(pitch)),
           ioi = c(NA, diff(onset)), ## <= seb edit. original => ioi = c(diff(onset), NA)
           ioi_class = classify_duration(ioi))
  #browser()
  if(any(is.na(melody$pitch)) || any(is.infinite(melody$pitch))){
    stop("Warning: Melody (%s) contains invalid pitches", fname)
  }
  if(any(melody$ioi[!is.na(melody$ioi)] < .01)){
    stop("Warnings: Melody (%s) contains IOIs less than 1 ms, possibly no note track", fname)
  }
  melody
}

#opti3 for melodies read by read_melody
#returns sorted tibble of transpositions of melody2 and opti3 similarity
opti3_df <- function(melody1, melody2, N = 3, use_bootstrap = F){
  trans_hints <- get_transposition_hints(melody1$pitch, melody2$pitch)
  v_rhythfuzz <- rhythfuzz(melody1$ioi_class, melody2$ioi_class)
  sims <- map_dfr(trans_hints, function(th){
    v_ngrukkon <- ngrukkon(melody1$pitch, melody2$pitch + th, N = N)
    if(use_bootstrap){
      v_harmcore <- harmcore2(melody1$pitch, melody2$pitch + th)
    }
    else{
      v_harmcore <- harmcore(melody1$pitch, melody2$pitch + th)

    }
    #browser()
    tibble(transposition = th,
           ngrukkon = v_ngrukkon,
           rhythfuzz = v_rhythfuzz,
           harmcore = v_harmcore,
           opti3 =  0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146)
  })
  sims %>% arrange(desc(opti3))
}
#windowed version, shifts shorter melody along longer
#returns tibble of shift offsets and highest opti3 similarity
best_subsequence_similarity <- function(melody1, melody2){

  shorter <- melody1
  longer <- melody2
  swap <- "1 along 2"
  l1 <- nrow(melody1)
  l2 <- nrow(melody2)
  if(l2 < l1){
    shorter <- melody2
    longer <- melody1
    swap <- "2 along 1"

  }
  l1 <- nrow(shorter)
  l2 <- nrow(longer)
  d_l <- l2 - l1
  map_dfr(1:(d_l + 1), function(offset){
    #messagef("Offset %d", offset)
    longer_snip <- longer[seq(offset, offset + l1 - 1),]
    tibble(offset = offset - 1, sim  =  opti3_df(shorter, longer_snip) %>% head(1) %>% pull(opti3))
  }) %>% mutate(process = swap) %>% arrange(desc(sim))
}

