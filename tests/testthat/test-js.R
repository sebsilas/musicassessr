

test_that("include_musicassessr_js works", {

  t <- include_musicassessr_js(record_audio = FALSE)

  expect_is(t, "shiny.tag.list")

})


test_that("musicassessr_js works", {

  t <- musicassessr_js(app_name = "", record_audio = FALSE)

  expect_is(t, "character")

})



class(musicassessr_js(app_name = "", record_audio = FALSE))
