test_that("melody string becomes vector", {
  x <- melody_str_to_vector("60, 62, 64, 65", sep = ",")
  y <- c(60, 62, 64, 65)
  expect_equal(x, y)
})
