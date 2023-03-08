
test_that("get_all_octaves_in_gamut works", {

  # Test 1

  t <- get_all_octaves_in_gamut(41, midi.gamut.min, midi.gamut.max)

  expect_setequal(t, c(17, 29, 41, 53, 65, 77, 89, 101, 113))


  # Test 2

  t2 <- get_all_octaves_in_gamut(41:42, midi.gamut.min, midi.gamut.max) # should be stopped

  m <- matrix(c(17, 29, 41, 53, 65, 77, 89, 101, 113,
                18, 30, 42, 54, 66, 78, 90, 102, 114))

  dim(m) <- c(9, 2)

  expect_true(all.equal(t2, m))


})




test_that("get_all_octaves_in_gamut works", {

  # Test 1


  t <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = c(60, 62, 65, 64), user_production_pitches = c(50, 60, 70, 80, 76))

  expect_setequal(t, c(50, 60, 72, 77, 76))


  # Test 2

  t2 <- find_closest_stimuli_pitch_to_user_production_pitches(stimuli_pitches = c(60, 62, 65, 64), user_production_pitches = c(50, 60, 70, 80, 76), allOctaves = FALSE)

  expect_setequal(t2, c(60, 60, 65, 65, 65))


})




