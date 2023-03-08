
test_that("check_additional_measures_args works", {

  expect_true(check_additional_measures_args(test_additional_measures_fun_success))

  expect_error(check_additional_measures_args(test_additional_measures_fun_failure))

})


test_that("score_melodic_production works", {



  t <- score_melodic_production(user_melody_input = c(60, 62, 64, 65),
                                user_duration_input = rep(1, 4),
                                user_onset_input = c(0, cumsum(rep(1, 3))),
                                stimuli = c(60, 62, 64, 65),
                                stimuli_duration = rep(1, 4))



  t2 <- score_melodic_production(user_melody_input = c(60, 67, 64, 65),
                                 user_duration_input = rep(1, 4),
                                 user_onset_input = c(0, cumsum(rep(1, 3))),
                                 stimuli = c(60, 62, 64, 65),
                                 stimuli_duration = rep(1, 4))


  t3 <- score_melodic_production(user_melody_input = c(60, 67, 64, 65, 60, 60),
                                 user_duration_input = rep(1, 6),
                                 user_onset_input = c(0, cumsum(rep(1, 5))),
                                 stimuli = c(60, 62, 64, 65),
                                 stimuli_duration = rep(1, 4))

  # octave

  t4 <- score_melodic_production(user_melody_input = c(72, 62, 64, 65),
                                 user_duration_input = rep(1, 4),
                                 user_onset_input = c(0, cumsum(rep(1, 3))),
                                 stimuli = c(60, 62, 64, 65),
                                 stimuli_duration = rep(1, 4))




  t5 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
                                 user_duration_input = c(1, 1.5, 1, 1.2, 1),
                                 user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
                                 stimuli = c(60, 62, 63, 64, 65),
                                 stimuli_durations = c(1, 1, 1, 1, 1))


  # with additional measures





  t6 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
                                 user_duration_input = c(1, 1.5, 1, 1.2, 1),
                                 user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
                                 stimuli = c(60, 62, 63, 64, 65),
                                 stimuli_durations = c(1, 1, 1, 1, 1),
                                 additional_scoring_measures = log_scores)

  t7 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
                                 user_duration_input = c(1, 1.5, 1, 1.2, 1),
                                 user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
                                 stimuli = c(60, 62, 63, 64, 65),
                                 stimuli_durations = c(1, 1, 1, 1, 1),
                                 additional_scoring_measures = check_no_notes_above_c4)

  t8 <- score_melodic_production(user_melody_input = c(60, 61, 63, 64, 65),
                                 user_duration_input = c(1, 1.5, 1, 1.2, 1),
                                 user_onset_input = cumsum(c(1, 1, 1, 1, 1)),
                                 stimuli = c(60, 62, 63, 64, 65),
                                 stimuli_durations = c(1, 1, 1, 1, 1),
                                 additional_scoring_measures = list(log_scores, check_no_notes_above_c4))


  expect_equal(length(t), 38)
  expect_equal(length(t2), 38)
  expect_equal(length(t3), 38)
  expect_equal(length(t4), 38)
  expect_equal(length(t5), 38)
  expect_equal(length(t6), 38)
  expect_equal(length(t7), 38)
  expect_equal(length(t8), 38)


  names <- c("stimuli", "stimuli_durations", "stimuli_length",
            "user_response_note", "user_response_note_summary",
            "user_response_midi_note_off", "pyin_pitch_track",
            "durations", "user_response_pitch_classes", "onsets_noteon",
            "onsets_noteoff", "trial_length", "no_note_events", "no_correct",
            "no_errors", "errors_boolean", "correct_boolean_octaves_allowed", "errors_boolean_octaves_allowed",
            "no_correct_octaves_allowed", "no_errors_octaves_allowed", "proportion_of_correct_note_events",
            "proportion_of_correct_note_events_octaves_allowed",
            "proportion_of_correct_note_events_controlled_by_stimuli_length_log_normal",
            "proportion_of_correct_note_events_octaves_allowed_controlled_by_stimuli_length_log_normal",
            "proportion_of_stimuli_notes_found",
            "proportion_of_stimuli_notes_found_octaves_allowed",
            "opti3", "ngrukkon", "harmcore", "rhythfuzz", "melody_dtw",
            "mean_cents_deviation_from_nearest_stimuli_pitch",
            "mean_cents_deviation_from_nearest_midi_pitch", "answer_meta_data",
            "additional_scoring_measures",
            "production",
            "melody_note_accuracy",
            "melody_interval_accuracy")

  expect_setequal(names, names(t))
  expect_setequal(names, names(t2))
  expect_setequal(names, names(t3))
  expect_setequal(names, names(t4))
  expect_setequal(names, names(t5))
  expect_setequal(names, names(t6))
  expect_setequal(names, names(t7))
  expect_setequal(names, names(t8))




})




