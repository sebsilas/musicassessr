% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trial_blocks.R
\name{sing_arrhythmic_melody_trials}
\alias{sing_arrhythmic_melody_trials}
\title{Sing arrhythmic melody trial block}
\usage{
sing_arrhythmic_melody_trials(
  var_name = "arrhythmic_melody",
  module_name = "sing_arrhythmic_melodies",
  page_text = psychTestR::i18n("sing_melody_page_text"),
  page_title = psychTestR::i18n("Sing_the_Melody"),
  instruction_text = psychTestR::i18n("sing_melody_instruction_text"),
  sampler_function = sample_arrhythmic,
  item_bank,
  num_items = integer(),
  num_examples = 0L,
  feedback = FALSE,
  get_answer = get_answer_pyin_melodic_production,
  sound = "piano",
  page_type = "record_audio_page",
  get_trial_characteristics_function = NULL,
  item_characteristics_sampler_function = NULL,
  item_characteristics_pars = NULL,
  rel_to_abs_mel_function = rel_to_abs_mel_mean_centred,
  max_goes = 3L,
  max_goes_forced = FALSE,
  display_modality = "auditory",
  show_progress = TRUE,
  get_similarity_to_previous_melody = FALSE
)
}
\arguments{
\item{get_similarity_to_previous_melody}{}
}
\description{
Sing arrhythmic melody trial block
}
