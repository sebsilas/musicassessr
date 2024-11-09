

#' ngrukkon as in  Müllensiefen & Frieler (2004)
#'
#' @param x
#' @param y
#' @param N
#'
#' @return
#' @export
#'
#' @examples
ngrukkon <- function(x, y, N = 3){
  x_ngrams <- itembankr::get_all_ngrams(x, N = N) %>% dplyr::pull(value)
  y_ngrams <- itembankr::get_all_ngrams(y, N = N) %>% dplyr::pull(value)
  joint <- c(x_ngrams, y_ngrams) %>% table()
  tx <- factor(x_ngrams, levels = names(joint)) %>% table()
  ty <- factor(y_ngrams, levels = names(joint)) %>% table()
  1 - sum(abs(tx  - ty))/(length(x) + length(y))
}


#' An ngrukkon wrapper to produce warnings and return NAs rather than stop if one entry is too short
#'
#' @param x
#' @param y
#' @param N
#'
#' @return
#' @export
#'
#' @examples
ngrukkon_safe <- function(x, y, N = 3) {
  ngrukkon_warning(x)
  ngrukkon_warning(y)
  if( length(x) < N | length(y) < N ) {
    res <- NA
  } else {
    res <- ngrukkon(x, y, N)
  }
  res
}

ngrukkon_warning <- function(v) {
  # ngrukkon must be used on intervals not pitches, so warn based on a guess that the input might be pitch rather than interval values
  if(mean(v, na.rm = TRUE) > 20) warning("Are you definitely using intervals for ngrukkon?")
}


#' get harmonies via the Krumhansl-Schmuckler algorithm
#'
#' @param pitch_vec
#' @param segmentation
#' @param only_winner
#'
#' @return
#' @export
#'
#' @examples
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = TRUE) {

  #warning('Segmentation format must be as segment ID')

  # Krumhansl-Schmuckler algorithm
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    return(
      purrr::map_dfr(s, function(x){
        pv <- pitch_vec[segmentation == x]
        tidyr::tibble(segment = x, key = get_implicit_harmonies(pv, NULL, only_winner = only_winner) %>% dplyr::pull(key))
      })
    )

  }
  pitch_freq <- table(factor(pitch_vec %% 12, levels = 0:11))
  correlations <- purrr::map_dfr(0:11, function(t){
      w_major <- cor.test(pitch_freq, ks_weights_major[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% dplyr::pull(estimate)
      w_minor <- cor.test(pitch_freq, ks_weights_minor[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% dplyr::pull(estimate)
      dplyr::bind_rows(tidyr::tibble(transposition = t,  match = w_major, type = "major", key = sprintf("%s-maj", itembankr::pc_labels_flat[t+1])),
                       tidyr::tibble(transposition = t,  match = w_minor, type = "minor", key = sprintf("%s-min", itembankr::pc_labels_flat[t+1])))
    }) %>% dplyr::arrange(dplyr::desc(match))
  if(only_winner){
    return(correlations[1,])
  }
  correlations
}
bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10){
  if(!is.null(segmentation)){
    segments <- unique(segmentation)
    ret <-
      purrr::map_dfr(segments, function(seg){
        bootstrap_implicit_harmonies(pitch_vec[segmentation == seg],
                                     NULL,
                                     sample_frac = sample_frac,
                                     size = size) %>%
          dplyr::mutate(segment = seg)
      })
    return(ret)
  }
  l <-length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    purrr::map_dfr(1:size, function(x){
      pv <- sample(pitch_vec, replace = T, sample_size)
      get_implicit_harmonies(pitch_vec = pv,  only_winner = TRUE)
    })
  best_key <- bs %>% dplyr::count(key) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::pull(key)
  bs %>% dplyr::filter(key == best_key[1]) %>% head(1)
}



#' Compute the rhythfuzz measure
#'
#' @param ioi_class1
#' @param ioi_class2
#'
#' @return
#' @export
#'
#' @examples
rhythfuzz <- function(ioi_class1, ioi_class2) {
  edit_sim(intToUtf8(ioi_class1 + 128), intToUtf8(ioi_class2 + 128))
}


#' Compute harmcore
#'
#' @param pitch_vec1
#' @param pitch_vec2
#' @param segmentation1
#' @param segmentation2
#' @param segmentation_type
#'
#' @return
#' @export
#'
#' @examples
harmcore <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL, segmentation_type = c("phrase_boundary_marker",
                                                                                                               "segment_id")){

  segmentation_type <- match.arg(segmentation_type)

  # phrase_boundary_marker e.g., c(0, 0, 0, 1, 0 0, 1)
  # segment_id e.g., c(1, 1, 1, 2, 2, 2, 3)

  if(!is.null(segmentation1) & !is.null(segmentation2)) {
    if(segmentation_type == "phrase_boundary_marker") {
      segmentation1 <- cumsum(segmentation1)
      segmentation2 <- cumsum(segmentation2)
    }
  }

  implicit_harm1 <- get_implicit_harmonies(pitch_vec1, segmentation1) %>% dplyr::pull(key)
  implicit_harm2 <- get_implicit_harmonies(pitch_vec2, segmentation2) %>% dplyr::pull(key)


  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}




harmcore2 <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL, segmentation_type = c("phrase_boundary_marker",
                                                                                                                "segment_id")) {

  # phrase_boundary_marker e.g., c(0, 0, 0, 1, 0 0, 1)
  # segment_id e.g., c(1, 1, 1, 2, 2, 2, 3)

  segmentation_type <- match.arg(segmentation_type)

  if(segmentation_type == "phrase_boundary_marker") {
    segmentation1 <- cumsum(segmentation1)
    segmentation2 <- cumsum(segmentation2)
  }

  implicit_harm1 <- bootstrap_implicit_harmonies(pitch_vec1, segmentation1) %>% dplyr::pull(key)
  implicit_harm2 <- bootstrap_implicit_harmonies(pitch_vec2, segmentation2) %>% dplyr::pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  harmcore2 <- edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}

#little helper to calculate modus of simple vector
modus <- function(x){
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){

  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = TRUE)
  key1 <- ih1 %>% dplyr::pull(key)
  pc1 <- ih1 %>% dplyr::pull(transposition)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = TRUE)
  pc2 <- ih2 %>% dplyr::pull(transposition)
  key_diff <- (pc2 -  pc1) %% 12
  #messagef("Best key 1 = %s, best key 2 = %s, key diff = %d", key1, ih2 %>% head(1) %>% dplyr::pull(key), key_diff )
  modus1 <- modus(pitch_vec1)
  modus2 <- modus(pitch_vec2)
  ret <- c(modus1 - modus2,
           round(mean(pitch_vec1)) - round(mean(pitch_vec2)),
           round(median(pitch_vec1)) - round(median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #messagef("Octave offset = %d", octave_offset)
  ret <- c(0, ret, octave_offset*12 + key_diff, octave_offset * 12 + 12 - key_diff)
  unique(ret) %>% sort()

}
#finds transposition that maximize raw edit distance of two pitch vectors
#transposision in semitone of the *second* melody
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- purrr:map_dfr(trans_hints, function(x){
    tidyr::tibble(transposition = x, sim = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% dplyr::arrange(sim) %>% head(1) %>% dplyr::pull(transposition)
}



#' Score using the opti3 measure of similarity
#'
#' @param pitch_vec1
#' @param onset_vec1
#' @param pitch_vec2
#' @param onset_vec2
#' @param N
#' @param use_bootstrap
#' @param return_components
#' @param segmentation1
#' @param segmentation2
#'
#' @return
#' @export
#'
#' @examples
opti3 <- function(pitch_vec1, onset_vec1,
                  pitch_vec2, onset_vec2,
                  N = 3,
                  use_bootstrap = TRUE,
                  return_components = FALSE,
                  segmentation1 = NULL,
                  segmentation2 = NULL) {

  warning('It is recommended to use the more comprehensive opti3_df version of opti3')

  stopifnot(all(c(pitch_vec1, pitch_vec2) > 0L))

  pitch_vec1 <- round(pitch_vec1)
  pitch_vec2 <- round(pitch_vec2)
  v_ngrukkon <- ngrukkon_safe(diff(pitch_vec1), diff(pitch_vec2), N = N)

  ioi1 <- c(NA, diff(onset_vec1))
  ioi2 <- c(NA, diff(onset_vec2))

  ioi_class1 <- classify_duration(ioi1)
  ioi_class2 <- classify_duration(ioi2)

  v_rhythfuzz <- rhythfuzz(ioi_class1, ioi_class2)

  if(use_bootstrap) {
    v_harmcore <- harmcore2(pitch_vec1, pitch_vec2)
  }
  else{
    v_harmcore <- harmcore(pitch_vec1, pitch_vec2)

  }
  opti3 <- 0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146

  #messagef("ngrukkon = %.3f, rhythfuzz = %.3f, harmcor = %.3f, opti3 = %.3f",
  #         v_ngrukkon, v_rhythfuzz, v_harmcore, opti3)

  opti3 <- max(min(opti3, 1), 0)

  if(return_components) {
    list("opti3" = opti3,
         "ngrukkon" = v_ngrukkon,
         "rhythfuzz" = v_rhythfuzz,
         "harmcore" = v_harmcore)
  } else {
    opti3
  }

}


#' read a pYIN note track (outputted from Sonic Annotator or Tony) and make it nice
#'
#' @param fname
#' @param style
#'
#' @return
#' @export
#'
#' @examples
read_melody <- function(fname, style = c("sonic_annotator", "tony")) {
  warning("Have you specified whether it is a Sonic Annotator vs. Tony pitch track correctly?")
  melody <-
    read.csv(fname, header = FALSE) %>%
    tidyr::as_tibble() %>%
    {if(style == "sonic_annotator") dplyr::rename(., onset = V1, freq = V3, dur = V2) else dplyr::rename(.,onset = V1, freq = V2, dur = V3)} %>%
    itembankr::produce_extra_melodic_features() ## NB! sonic annotator and tony output different column orders, hence the above

  if(any(is.na(melody$note)) || any(is.infinite(melody$note))){
    stop("Warning: Melody (%s) contains invalid pitches", fname)
  }
  if(any(melody$ioi[!is.na(melody$ioi)] < .01)){
    stop("Warnings: Melody (%s) contains IOIs less than 1 ms, possibly no note track", fname)
  }
  melody
}




#' opti3 for melodies read by read_melody
#' returns sorted tibble of transpositions of melody2 and opti3 similarity
#'
#' @param melody1
#' @param melody2
#' @param N
#' @param use_bootstrap
#' @param return_winner
#'
#' @return
#' @export
#'
#' @examples
opti3_df <- function(melody1, melody2, N = 3, use_bootstrap = FALSE, return_winner = TRUE) {

  trans_hints <- get_transposition_hints(melody1$note, melody2$note)
  v_rhythfuzz <- rhythfuzz(melody1$ioi_class, melody2$ioi_class)
  v_ngrukkon <- ngrukkon_safe(diff(melody1$note), diff(melody2$note), N = N)

  sims <- purrr::map_dfr(trans_hints, function(th){

    if(use_bootstrap) {
      v_harmcore <- harmcore2(melody1$note, melody2$note + th, segmentation1 = NULL, segmentation2 = NULL)
    }
    else{
      v_harmcore <- harmcore(melody1$note, melody2$note + th, segmentation1 = NULL, segmentation2 = NULL)
    }

    v_ngrukkon_na_handled <- if(is.na(v_ngrukkon)) 0 else v_ngrukkon

    opti3 <-  0.505 *  v_ngrukkon_na_handled + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146
    opti3 <- max(min(opti3, 1), 0)

    tibble::tibble(transposition = th,
                   ngrukkon = v_ngrukkon,
                   rhythfuzz = v_rhythfuzz,
                   harmcore = v_harmcore,
                   opti3 = opti3)
  })
  res <- sims %>% dplyr::arrange(dplyr::desc(opti3))
  if(return_winner) {
    res %>% dplyr::slice(1)
  } else {
    res
  }
}


best_subsequence_similarity <- function(melody1, melody2){
  #windowed version, shifts shorter melody along longer
  #returns tibble of shift offsets and highest opti3 similarity

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
  purrr::map_dfr(1:(d_l + 1), function(offset){
    #messagef("Offset %d", offset)
    longer_snip <- longer[seq(offset, offset + l1 - 1),]
    tidyr::tibble(offset = offset - 1, sim  =  opti3_df(shorter, longer_snip) %>% head(1) %>% dplyr::pull(opti3))
  }) %>% dplyr::mutate(process = swap) %>% dplyr::arrange(desc(sim))
}





messagef <- function(...) message(sprintf(...))

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

edit_sim <- function(s, t) {
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}



# # ioi1 <- c(0.3, 0.3, 0.3, 0.3)
# # ioi2 <- c(0.243809524,0.284444444,0.325079365,0.487619047)
# #
# #
# # ioi_class1 <- classify_duration(ioi1)
# # ioi_class2 <- classify_duration(ioi2)
# #
# # v_rhythfuzz <- rhythfuzz(ioi_class1, ioi_class2)
# #
# # v_rhythfuzz
#
#


# res <- tibble::tibble(onset = c(0.667573696, 0.963628118, 1.265487528, 1.613786848),
#                       dur = c(0.243809524, 0.284444444, 0.325079365, 0.487619047),
#                       freq = c(203.277, 180.519, 171.475, 183.124),
#                       note = c(56, 54, 53, 54)) %>%
#   itembankr::produce_extra_melodic_features()
#
# stim_length <- 4L
#
# stimuli <- c(68, 66, 65, 66)
# stimuli_durations <- rep(0.3, 4)
#
# tt <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res)
# tt


# Test empty df

# res2 <- tibble::tibble(onset = integer(0),
#                        dur = integer(0),
#                        freq = integer(0),
#                        note = integer(0))
#

# tt <- musicassessr::get_opti3(stimuli, stimuli_durations, stim_length, res2)
# tt

