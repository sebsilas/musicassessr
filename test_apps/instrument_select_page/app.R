library(psychTestR)

psychTestR::make_test(
  psychTestR::new_timeline(
    psychTestR::join(
      select_musical_instrument_page(),
      psychTestR::final_page("Finished!")
    ), dict = musicassessr::dict(NULL)),
  opt = psychTestR::test_options(
    title = "Instrument select page test",
    admin_password = "demo",
    languages = c("de", "en")
  )
)

# shiny::runApp("test_apps/instrument_select_page/app.R")
