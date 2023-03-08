test_that("feedback pages work", {

  fb_page <- feedback_melodic_production()
  expect_is(fb_page, "reactive_page")

  fb_long_tone <- feedback_long_tone()
  expect_is(fb_long_tone, "reactive_page")

})




test_that("list_to_shiny_table works", {

  shiny_tab <- list_to_shiny_table(list("a" = 1, "b" = 2))

  expect_is(shiny_tab, "shiny.render.function")
})


test_that("add_feedback works", {


  tl <- psychTestR::join(
    lapply(LETTERS, psychTestR::one_button_page)
  )

  expect_equal(length(tl), 26)
  expect_is(tl, "list")


  tl2 <- add_feedback(tl, function() 'feedback_melodic_production')

  expect_equal(length(tl2), 52)
  expect_is(tl2, "list")

})


