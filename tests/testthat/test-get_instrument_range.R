test_that("sing_happy_birthday_page works", {
  p <- sing_happy_birthday_page()
  expect_is(p, "page")
})
