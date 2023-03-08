test_that("check_answer works", {

  expect_true(check_answer("a", "a", type = "single_letters"))

  expect_false(check_answer("a", "b", type = "single_letters"))

  expect_false(check_answer("aaa", "bbb", reverse = FALSE, type = "chunk_letters"))

  expect_true(check_answer("aaa", "aaa", type = "chunk_letters"))

  expect_true(check_answer(11, 22, type = "single_numbers"))

  expect_true(check_answer(11, 11, type = "single_numbers"))

  expect_true(check_answer(11, 11, reverse = TRUE,  type = "chunk_digits"))

  expect_false(check_answer(123, 123, reverse = TRUE, type = "chunk_digits"))

  expect_true(check_answer(123, 321, reverse = TRUE, type = "chunk_digits"))



  expect_true(check_answer(1:3, 3:1, reverse = TRUE, type = "chunk_digits"))
  expect_false(check_answer(1:3, 3:1, reverse = FALSE, type = "chunk_digits"))
  expect_false(check_answer(1:3, 1:3, reverse = TRUE, type = "chunk_digits"))
  expect_error(check_answer(1:3, 1:4, reverse = TRUE, type = "chunk_digits"))

  expect_true(check_answer(c(10,50,60), c(10,50,60), type = "midi_pitches"))
  expect_false(check_answer(c(10,50,60), c(10,50,60), reverse = TRUE, type = "midi_pitches"))
  expect_true(check_answer(c(10,50,60), c(60,50,10), reverse = TRUE, type = "midi_pitches"))



})




