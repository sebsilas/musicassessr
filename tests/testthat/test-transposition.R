test_that("sample_keys_by_difficulty works", {

  t <- sample_keys_by_difficulty("Piano", n_easy = 4, n_hard = 4)

  expect_equal(dim(t), c(8, 6))

})

