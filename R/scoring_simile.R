
#' Get all ngrams from a given vector
#'
#' @param x
#' @param N
#'
#' @return
#' @export
#'
#' @examples
get_all_ngrams <- function(x, N = 3){
  l <- length(x) - N + 1
  stopifnot(l > 0)
  purrr::map_df(1:l, function(i){
    ngram <- x[i:(i + N - 1)]
    tidyr::tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
}

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
  #browser()
  x <- get_all_ngrams(x, N = N) %>% dplyr::pull(value)
  y <- get_all_ngrams(y, N = N) %>% dplyr::pull(value)
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  1 - sum(abs(tx  - ty))/(length(x) + length(y))
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
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = T){
  #Krumhansl-Schmuckler algorithm
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    return(
      purrr:map_dfr(s, function(x){
        #browser()
        pv <- pitch_vec[segmentation == x]
        tidyr::tibble(segment = x, key = get_implicit_harmonies(pv, NULL, only_winner = only_winnter) %>% dplyr::pull(key))
      })
    )

  }
  pitch_freq <- table(factor(pitch_vec  %% 12, levels = 0:11))
  correlations <-
    purrr::map_dfr(0:11, function(t){
      #browser()
      w_major <- cor.test(pitch_freq, ks_weights_major[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% dplyr::pull(estimate)
      w_minor <- cor.test(pitch_freq, ks_weights_minor[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% dplyr::pull(estimate)
      dplyr::bind_rows(tidyr::tibble(transposition = t,  match = w_major, type = "major", key = sprintf("%s-maj", pc_labels_flat[t+1])),
                       tidyr::tibble(transposition = t,  match = w_minor, type = "minor", key = sprintf("%s-min", pc_labels_flat[t+1])))
    }) %>% dplyr::arrange(desc(match))
  if(only_winner){
    return(correlations[1,])
  }
  correlations
}
bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10){
  l <-length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    purrr::map_dfr(1:size, function(x){
      pv <- sample(pitch_vec, replace = T, sample_size)
      get_implicit_harmonies(pv, only_winner = T)
    })
  best_key <- bs %>% dplyr::count(key) %>% dplyr::arrange(desc(n)) %>% dplyr::pull(key)
  bs %>% dplyr::filter(key == best_key[1]) %>% head(1)
}

#' Classify ioi class (see Frieler.. )
#'
#' @param dur_vec
#' @param ref_duration
#'
#' @return
#' @export
#'
#' @examples
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
  implicit_harm1 <- get_implicit_harmonies(pitch_vec1, segmentation1) %>% dplyr::pull(key)
  implicit_harm2 <- get_implicit_harmonies(pitch_vec2, segmentation2) %>% dplyr::pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2), toSingleChars = TRUE)
}

harmcore2 <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL){
  implicit_harm1 <- bootstrap_implicit_harmonies(pitch_vec1, segmentation1) %>% dplyr::pull(key)
  implicit_harm2 <- bootstrap_implicit_harmonies(pitch_vec2, segmentation2) %>% dplyr::pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2), toSingleChars = TRUE)
}

#little helper to calculate modus of simple vector
modus <- function(x){
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){
  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = T)
  key1 <- ih1 %>% dplyr::pull(key)
  pc1 <- ih1 %>% dplyr::pull(transposition)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = T)
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
  ret <- c(0, ret, octave_offset*12 + key_diff, octave_offset*12 + 12 - key_diff)
  unique(ret) %>% sort()

}
#finds transposition that maximize raw edit distance of two pitch vectors
#transposision in semitone of the *second* melody
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- purrr:map_dfr(trans_hints, function(x){
    #browser()
    tidyr::tibble(transposition = x, sim = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% dplyr::arrange(sim) %>% head(1) %>% dplyr::pull(transposition)
}


#' Score using the opti3 measure of similarity
#'
#' @param pitch_vec1
#' @param dur_vec1
#' @param pitch_vec2
#' @param dur_vec2
#' @param N
#' @param use_bootstrap
#' @param return_components
#'
#' @return
#' @export
#'
#' @examples
opti3 <- function(pitch_vec1, dur_vec1, pitch_vec2, dur_vec2, N = 3, use_bootstrap = TRUE, return_components = FALSE) {

  stopifnot(all(c(pitch_vec1, pitch_vec2) > 0L))

  pitch_vec1 <- round(pitch_vec1)
  pitch_vec2 <- round(pitch_vec2)
  v_ngrukkon <- ngrukkon(pitch_vec1, pitch_vec2, N = N)
  dur_vec1 <- classify_duration(dur_vec1)
  dur_vec2 <- classify_duration(dur_vec2)
  v_rhythfuzz <- rhythfuzz(dur_vec1, dur_vec2)

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


#read a pYIN note track and make it nice
read_melody <- function(fname, style = c("sonic_annotator", "tony")) {
  warning("Have you specified whether it is a Sonic Annotator vs. Tony pitch track correctly?")
  melody <-
    read.csv(fname, header = F) %>%
    tidyr::as_tibble() %>%
    {if(style == "sonic_annotator") dplyr::rename(., onset = V1, freq = V3, dur = V2) else dplyr::rename(.,onset = V1, freq = V2, dur = V3)} %>%
    ## NB! sonic annotator and tony output different column orders, hence the above
    produce_extra_melodic_features()

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
  stopifnot(all(c(melody1$pitch, melody2$pitch) > 0L))
  trans_hints <- get_transposition_hints(melody1$pitch, melody2$pitch)
  v_rhythfuzz <- rhythfuzz(melody1$ioi_class, melody2$ioi_class)
  sims <- purrr::map_dfr(trans_hints, function(th){
    v_ngrukkon <- ngrukkon(melody1$pitch, melody2$pitch + th, N = N)
    if(use_bootstrap){
      v_harmcore <- harmcore2(melody1$pitch, melody2$pitch + th)
    }
    else{
      v_harmcore <- harmcore(melody1$pitch, melody2$pitch + th)
    }
    #browser()
    tidyr::tibble(transposition = th,
                  ngrukkon = v_ngrukkon,
                  rhythfuzz = v_rhythfuzz,
                  harmcore = v_harmcore,
                  opti3 =  0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146)
  })
  sims %>% dplyr::arrange(desc(opti3))
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

edit_sim <- function(s, t){
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}
# melody_1 <- read_melody('/Users/sebsilas/Downloads/K_S/HBD_test/seb.csv', style = "tony")
# melody_2 <- read_melody('/Users/sebsilas/Downloads/K_S/HBD_test/sylvia.csv', style = "tony")
# da <- opti3_df(melody_1, melody_2)
# da2 <- opti3(melody_1$pitch, melody_1$dur, melody_2$pitch, melody_2$dur)
# da3 <- opti3(melody_1$pitch, melody_1$dur, melody_2$pitch, melody_2$dur, return_components = TRUE)
