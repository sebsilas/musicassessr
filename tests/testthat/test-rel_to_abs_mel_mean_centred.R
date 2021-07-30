test_that("rel_to_abs_mel_mean_centred works", {
  rel_to_abs_mel_mean_centred_res <- rel_to_abs_mel_mean_centred(itembankr::Berkowitz[1000, "melody"], 40, 65, FALSE)
  expect_equal(rel_to_abs_mel_mean_centred_res, c(52, 53, 55, 53, 51))
})
