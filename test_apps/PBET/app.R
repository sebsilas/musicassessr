
library(PBET)

# setwd('test_apps/PBET')

PBET_standalone(num_items = list("interval_perception" = 0L,
                                 "find_this_note" = 0L,
                                 "arrhythmic" = list("key_easy" = 1L,
                                                     "key_hard" = 1L),
                                 "rhythmic" = list("key_easy" = 1L,
                                                   "key_hard" = 1L),
                                 "wjd_audio" = list("key_easy" = 0L,
                                                    "key_hard" = 0L)),
                skip_setup = TRUE,
                concise_wording = TRUE,
                force_p_id_from_url = TRUE,
                allow_any_p_id_url = TRUE,
                logo = "musicassessr-assets/img/slice_the_pie_logo.jpg",
                logo_width = "200px",
                logo_height = "81px")


# shiny::runApp("test_apps/PBET/app.R")

# for leaderboard to work...


# shiny::runApp(".")


