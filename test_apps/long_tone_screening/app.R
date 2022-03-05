library(SAA)

SAA::SAA_standalone(long_tone_trials_as_screening = TRUE, SNR_test = F, get_range = F, examples = 0,
                    num_items = list(long_tones = 2L, arrhythmic = 2, rhythmic = 2), musicassessr_state = 'test')

# shiny::runApp("test_apps/long_tone_screening/app.R")
