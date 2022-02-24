library(psychTestR)
library(dplyr)

psychTestR::make_test(
  psychTestR::new_timeline(
    psychTestR::join(
      musicassessr::musicassessr_init(),
      microphone_calibration_page(),

      psychTestR::one_button_page("First, test in a loop."),

      get_SNR_pages_loop(),

      psychTestR::one_button_page("Then, test without a loop and error which will prevent the test from running further (if the SNR test fails)."),

      get_SNR_pages(),

      psychTestR::final_page("Finished!")
    ), dict = musicassessr::dict(NULL)),
  opt = psychTestR::test_options(
    title = "Sonscience Test",
    admin_password = "demo",
    languages = c("en"),
    additional_scripts = musicassessr::musicassessr_js('test')
  )
)

# shiny::runApp("test_apps/SNR_test/app.R")




