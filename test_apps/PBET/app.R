PBET::PBET_standalone(num_items = list("interval_perception" = 0L,
                                       "find_this_note" = 0L,
                                       "arrhythmic" = list("key_easy" = 10L, "key_hard" = 10L),
                                       "rhythmic" = list("key_easy" = 10L, "key_hard" = 10L),
                                        "wjd_audio" = list("key_easy" = 0L, "key_hard" = 0L)),
                      musicassessr_state = "test",
                      SNR_test = F, get_range = F,
                      feedback = F,
                      final_results = TRUE,
                      max_goes_forced = TRUE,
                      max_goes = 1L,
                      give_first_melody_note = TRUE,

                      melody_length = 3:4)

# shiny::runApp("test_apps/PBET/app.R")

# for leaderboard to work...

# setwd('test_apps/PBET')
# shiny::runApp(".")


