

launch <- function(final_page_ui = psychTestR::i18n("bye")) {

  timeline <- psychTestR::join(
      testdict::hello_wrapper(),
      psychTestR::final_page(final_page_ui)
    ) %>% testdict::wrap_tl()

  psychTestR::make_test(timeline, opt = psychTestR::demo_options(languages = "en"))
}

launch2 <- function(final_page_ui = psychTestR::i18n("thank_you_for_completing")) {

  timeline <- psychTestR::join(
    #testdict::hello_wrapper(),
    psychTestR::final_page(final_page_ui)
  ) %>% testdict::wrap_tl2()

  psychTestR::make_test(timeline, opt = psychTestR::demo_options(languages = "en"))
}

launch3 <-
  make_musicassessr_test
