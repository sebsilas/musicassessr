

psychTestR::make_test(
  psychTestR::join(
    psychTestR::one_button_page("We're going to test some spoken words."),
    record_spoken_words_page(),
    feedback_recorded_words_plot_page(),
    psychTestR::final_page("Finished!")
  )
)

# shiny::runApp("test_apps/spoken_words/app.R")
