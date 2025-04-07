

# TODO: look at bioacoustics package features

#' Extract audio features from an audio file
#'
#' @param audio_file_path
#'
#' @return
#' @export
#'
#' @examples
extract_audio_features <- function(audio_file_path) {

  file_extension <- tools::file_ext(audio_file_path)

  # Load the audio file
  if(file_extension %in% c("wav", "WAV")) {
    audio <- tuneR::readWave(audio_file_path)
  } else if(file_extension %in% c("mp3", "MP3")) {
    audio <- tuneR::readMP3(audio_file_path)
  } else {
    stop("Audio file format not supported.")
  }

  # Extract MFCCs
  mfcc_df <- tuneR::melfcc(audio) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   list(mean = ~ mean(.x, na.rm = TRUE),
                                        sd = ~ sd(.x, na.rm = TRUE))))

  # Extract spectral features
  spec <- seewave::spec(audio, plot = FALSE, fftw = F)
  spec_prop <- seewave::specprop(spec)
  spec_df <- as.data.frame(t(spec_prop))

  # Extract temporal features
  tempo_df <- extract_temporal_features(audio)

  ecoacoustics_df <- compute_ecoacoustics(audio)

  # Combine all features into one data frame
  features_df <- dplyr::bind_cols(
    mfcc_df,
    spec_df,
    tempo_df,
    ecoacoustics_df) %>%
    dplyr::mutate(file_key = basename(audio_file_path)) %>%
    dplyr::relocate(file_key)

  return(features_df)
}


# Function to extract temporal features including envelope features
extract_temporal_features <- function(audio) {

  # Extract basic information
  duration <- length(audio@left) / audio@samp.rate
  sample_rate <- audio@samp.rate
  bit_depth <- audio@bit

  # Calculate amplitude envelope
  env <- seewave::env(audio, plot = FALSE)
  norm_env <- env / max(env)

  # Attack time
  attack_start <- which(norm_env >= 0.1)[1]
  attack_end <- which(norm_env >= 0.9)[1]
  attack_time <- (attack_end - attack_start) / sample_rate

  # Sustain level and decay time
  sustain_level <- mean(norm_env[(attack_end + 1):(length(norm_env) / 2)])
  decay_end <- which(norm_env <= sustain_level)[1]
  decay_time <- (decay_end - attack_end) / sample_rate

  # Release time
  release_start <- which(norm_env == max(norm_env[(length(norm_env) / 2):length(norm_env)]))[1]
  release_end <- which(norm_env <= 0.1)[1]
  release_time <- (release_end - release_start) / sample_rate

  # Temporal centroid
  time_vector <- seq(0, length(norm_env) - 1) / sample_rate
  temporal_centroid <- sum(time_vector * norm_env) / sum(norm_env)

  # Root Mean Square (RMS) and Zero-Crossing Rate (ZCR)
  rms_val <- seewave::rms(env)
  zcr <- seewave::zcr(audio, wl = NULL)
  shannon_entropy <- seewave::H(audio)

  # Create a list to hold the features
  temporal_features <- tibble::tibble(
    duration = duration,
    sample_rate = sample_rate,
    bit_depth = bit_depth,
    rms = rms_val,
    zero_crossing_rate = zcr,
    shannon_entropy = shannon_entropy,
    attack_time = attack_time,
    decay_time = decay_time,
    sustain_level = sustain_level,
    release_time = release_time,
    temporal_centroid = temporal_centroid
  )

  return(temporal_features)
}




compute_ecoacoustics <- function(audio) {

  mspec <- seewave::meanspec(audio, plot=FALSE)
  fp <- nrow(seewave::fpeaks(mspec, plot=FALSE))
  sdspec <- seewave::soundscapespec(audio, plot=FALSE)
  ndsi <- tryCatch(soundecology::ndsi(audio),
                   error = function(err) {
                     logging::logerror(err)
                     NA
                   })
  env <- seewave::env(audio, plot = FALSE)

  adi <- tryCatch(soundecology::acoustic_complexity(audio),
                  error = function(cond) {
                    print(cond)
                    list(AciTotAll_left = NA,
                         AciTotAll_left_bymin = NA)
                  })


  aci <- tryCatch(seewave::ACI(audio),
                  error = function(cond) {
                    print(cond)
                    NA
                  })


  tibble::tibble(
    acoustic_complexity_index = aci,
    acoustic_diversity_index = adi$AciTotAll_left,
    acoustic_diversity_index2 = adi$AciTotAll_left_bymin,
    acoustic_diversity_index3 = tryCatch(soundecology::acoustic_diversity(audio)$adi_left, error = log_err_but_return_na),
    acoustic_entropy_index = tryCatch(seewave::H(audio), error = log_err_but_return_na),
    acoustic_evenness_index = tryCatch(soundecology::acoustic_evenness(audio)$aei_left, error = log_err_but_return_na),
    bioacoustic_index = tryCatch(soundecology::bioacoustic_index(audio)$left_area, error = log_err_but_return_na),
    frequency_peaks_number = fp,
    amplitude_index = tryCatch(seewave::M(audio), error = log_err_but_return_na),
    normalized_difference_soundscape_index = tryCatch(seewave::NDSI(sdspec), error = log_err_but_return_na),
    spectral_entropy_ndsi = if(is.scalar.na(ndsi)) NA else ndsi$ndsi_left,
    spectral_entropy_anthrophony = if(is.scalar.na(ndsi)) NA else ndsi$anthrophony_left,
    spectral_entropy_biophony = if(is.scalar.na(ndsi)) NA else ndsi$biophony_left,
    spectral_entropy2 = tryCatch(seewave::sh(mspec), error = log_err_but_return_na),
    temporal_entropy = tryCatch(seewave::th(env), error = log_err_but_return_na)
  )

}


# tt <- extract_audio_features("~/lyricassessr/data-raw/Vocals/Tenor2/Eh/Tenor2_Eh_24.wav")

