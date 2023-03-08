test_that("adaptive_arrhythmic_melody_trials works", {

  adaptive_trials <- musicassessr::adaptive_arrhythmic_melody_trials("aSAA_arrhythmic",
                                                                     10L,
                                                                     Berkowitz::Berkowitz_IRT_arrhythmic_scaled,
                                                                     Berkowitz::lm2.2_scaled,
                                                                     demo = TRUE,
                                                                     fixed_effects = c("N",
                                                                                       "step.cont.loc.var",
                                                                                       "tonalness",
                                                                                       "log_freq"))

  expect_is(adaptive_trials, "timeline")
})


