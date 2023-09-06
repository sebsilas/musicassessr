

score_rhythm_production <- function(stimuli_durations, user_durations, bpm = NULL) {


  stimuli_bpm <- round(60/stimuli_durations)  # Note, this is not a good way to get the BPM for actual stimuli, but will work for the beat sync trials where the stimuli is basically a metronome :

  mean_dur <- mean(user_durations, na.rm = TRUE)

  if(is.null(bpm)) {
    bpm <- round(60/mean_dur) # This is a proxy but not particularly sophisticated..
  }

  if(is.scalar.na.or.null(stimuli_durations)) {
    dtw_dist <- NA
    tam_dist <- NA
  } else {
    dtw_dist <- dtw::dtw(stimuli_durations, user_durations)$distance
    tam_dist <- TSdist::TAMDistance(stimuli_durations, user_durations)
  }

  res <- list(
    stimuli_durations = stimuli_durations,
    mean_duration = mean_dur,
    precision = sd(user_durations, na.rm = TRUE),
    accuracy = stats::mad(user_durations, center = bpm_to_ms(stimuli_bpm)),
    dtw_distance = dtw_dist,
    tam_distance = tam_dist,
    user_bpm = bpm,
    user_durations = user_durations,
    rhythfuzz = rhythfuzz(stimuli_durations, user_durations)
  )

  return(res)
}
