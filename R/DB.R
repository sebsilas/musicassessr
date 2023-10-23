
db_append_scores_session <- function(db_con, session_id, mean_opti3_arrhythmic, mean_opti3_arrhythmic_first_attempt, mean_opti3_arrhythmic_last_attempt,
                                     ability_estimate_arrhythmic_first_attempt, ability_estimate_arrhythmic_last_attempt,
                                     mean_opti3_rhythmic, mean_opti3_rhythmic_first_attempt, mean_opti3_rhythmic_last_attempt,
                                    ability_estimate_rhythmic_first_attempt, ability_estimate_rhythmic_last_attempt) {

  session_scores_df <- tibble::tibble(session_id = session_id,
                                      mean_opti3_arrhythmic = mean_opti3_arrhythmic,
                                      mean_opti3_arrhythmic_first_attempt = mean_opti3_arrhythmic_first_attempt,
                                      mean_opti3_arrhythmic_last_attempt = mean_opti3_arrhythmic_last_attempt,
                                      ability_estimate_arrhythmic_first_attempt = ability_estimate_arrhythmic_first_attempt,
                                      ability_estimate_arrhythmic_last_attempt = ability_estimate_arrhythmic_last_attempt,
                                      mean_opti3_rhythmic = mean_opti3_rhythmic,
                                      mean_opti3_rhythmic_first_attempt = mean_opti3_rhythmic_first_attempt,
                                      mean_opti3_rhythmic_last_attempt = mean_opti3_rhythmic_last_attempt,
                                      ability_estimate_rhythmic_first_attempt = ability_estimate_rhythmic_first_attempt,
                                      ability_estimate_rhythmic_last_attempt = ability_estimate_rhythmic_last_attempt)

  db_append_to_table(db_con, table = "scores_session", data = session_scores_df, primary_key_col = "scores_session_id")

}


db_append_session <- function(db_con,
                              condition_id,
                              user_id,
                              psychTestR_session_id,
                              time_completed,
                              experiment_id) {

  session_df <- tibble::tibble(condition_id = condition_id,
                               user_id = user_id,
                               psychTestR_session_id = psychTestR_session_id,
                               time_completed = time_completed,
                               experiment_id = experiment_id)

  session_id <- db_append_to_table(db_con, table = "sessions", data = session_df, primary_key_col = "session_id")
  session_id
}



db_append_melodic_production <- function(db_con, trial_id, pyin_res, correct_boolean) {
  melodic_production_df <- pyin_res %>%
    dplyr::select(-file_name) %>%
    dplyr::mutate(trial_id = trial_id,
                  correct = correct_boolean)
  db_append_to_table(db_con, table = "melodic_production", data = melodic_production_df)
}


# pyin::test_pyin() %>% dplyr::select(onset:note) %>% db_append_melodic_production()

db_append_trials <- function(db_con, audio_file, time_started, time_completed, instrument,
                             attempt, item_id, display_modality, phase, rhythmic, item_bank_id, session_id, test_id) {

  stopifnot(
            is.scalar.character(audio_file),
            is(time_started, "POSIXct"),
            is(time_completed, "POSIXct"),
            is.scalar.character(instrument),
            is.integer(attempt),
            is.scalar.character(item_id) || is.na(item_id),
            is.scalar.character(display_modality),
            is.scalar.character(phase) || is.na(phase),
            is.scalar.logical(rhythmic) || is.na(rhythmic),
            is.integer(item_bank_id) || is.na(item_bank_id),
            is.integer(session_id),
            is.integer(test_id)
            )

  trial_df <- tibble::tibble(audio_file = audio_file,
                             time_started = time_started,
                             time_completed = time_completed,
                             instrument = instrument,
                             attempt = attempt,
                             item_id = item_id,
                             display_modality = display_modality,
                             phase = phase,
                             rhythmic = rhythmic,
                             item_bank_id = item_bank_id,
                             session_id = session_id,
                             test_id = test_id)

  db_append_to_table(db_con, table = "trials", data = trial_df, primary_key_col = "trial_id")
}


# t <- db_append_trials(item_name = "test_item", audio_file = "test.wav", attempt = 1L,
#                  time_started = Sys.time(), time_completed = Sys.time() - lubridate::seconds(10), instrument = "test_instrument")
#




db_append_items_studied <- function(review = FALSE, next_review_date = NULL) {

  stopifnot(
    is.scalar.logical(review),
    is(next_review_date, "POSIXct"),
  )


  items_studied_df <- tibble::tibble(review = review, next_review_date = next_review_date)

  db_append_to_table(con, table = "items_studied", data = items_studied_df)
}



db_append_conditions <- function(condition_name, condition_description) {

  stopifnot(
    is.scalar.character(condition_name),
    is.scalar.character(condition_description)
  )


  conditions_df <- tibble::tibble(condition_name = condition_name,
                                  condition_description = condition_description)

  db_append_to_table(con, table = "conditions", data = conditions_df)
}



db_append_users <- function(db_con, username, password, enabled = TRUE, created_at = Sys.time() ) {

  stopifnot(
    is.scalar.character(username),
    is.scalar.character(password),
    is.scalar.logical(enabled),
    is(created_at, "POSIXct")
  )


  users_df <- tibble::tibble(username = username,
                             password = digest::hmac(
                               key = Sys.getenv(x = "ENCRYPTION_KEY"),
                               object = password,
                               algo = "sha512"
                             ),
                             enabled = enabled,
                             created_at = created_at)

  db_append_to_table(db_con, table = "users", data = users_df)
}

db_append_session_tokens <- function(user_id, expiration_in_seconds) {

  stopifnot(
    is.character(user_id),
    is.numeric(expiration_in_seconds)
  )

  token <- create_session_token()

  expires <- Sys.time() + expiration_in_seconds


  session_tokens_df <- tibble::tibble(
                             user_id = user_id,
                             token = token,
                             created_at = created_at,
                             expires = expires,
                             active = TRUE)

  db_append_to_table(con, table = "session_tokens", data = session_tokens_df)
}


db_append_user_instrument_info <- function(db_con, user_id, instrument_id, bottom_range, top_range) {

  stopifnot(
    is.integer(user_id),
    is.integer(instrument_id),
    is.integer(bottom_range) & bottom_range %in% midi.gamut,
    is.integer(top_range) & top_range %in% midi.gamut
  )


  instrument_ranges_df <- tibble::tibble(
    user_id = user_id,
    instrument_id = instrument_id,
    bottom_range = bottom_range,
    top_range = top_range)

  db_append_to_table(db_con, table = "user_instrument_info", data = instrument_ranges_df)
}



#' Add final session info to postgres db
#'
#'
#' @return
#' @export
#'
#' @examples
elt_add_final_session_info_to_db <- function() {


  psychTestR::code_block(function(state, ...) {

    use_musicassessr_db <- psychTestR::get_global("use_musicassessr_db", state)

    if(use_musicassessr_db) {

      # Get session info
      session_id <- psychTestR::get_global("session_id", state) # Created earlier
      test_id <- psychTestR::get_global("test_id", state)
      condition_id <- psychTestR::get_global("condition_id", state)
      user_id <- psychTestR::get_global("user_id", state)
      experiment_id <- psychTestR::get_global("experiment_id", state)
      item_bank_id <- psychTestR::get_global("item_bank_id", state)
      session_info <- psychTestR::get_session_info(state, complete = TRUE)
      psychTestR_session_id <- session_info$p_id
      time_started <- session_info$time_started
      time_completed <- session_info$current_time

      db_con <- psychTestR::get_global('db_con', state)

      # Get trials
      trial_table <- compile_item_trials(db_con, test_id, session_id) # Here we give a session ID, because we only want to assess trials in this session


      # Join on scores
      trial_table <- trial_table %>%
        dplyr::rename(ngrukkon_between_melody_and_parent_melody = ngrukkon) %>%
        dplyr::left_join(get_table(db_con, "scores_trial"))

      # First attempt
      first_attempt_trial_table <- trial_table %>%
        dplyr::group_by(melody) %>%
        dplyr::slice_min(attempt, with_ties = FALSE) %>%
        dplyr::ungroup()

      # Last attempt
      last_attempt_trial_table <- trial_table %>%
        dplyr::group_by(melody) %>%
        dplyr::slice_max(attempt, with_ties = FALSE) %>% #
        dplyr::ungroup()

      # Produce session-level scores

        ## Arrhythmic

        ### Mean opti3
      mean_opti3_arrhythmic <- trial_table %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)
          #### First attempt
        mean_opti3_arrhythmic_first_attempt <- first_attempt_trial_table %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)
          #### Last attempt
        mean_opti3_arrhythmic_last_attempt <- last_attempt_trial_table %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)

        ### Ability estimate
        #### First attempt
        ability_estimate_arrhythmic_first_attempt <-
          first_attempt_trial_table %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::select(N, step.cont.loc.var, log_freq, i.entropy, opti3) %>%
          dplyr::mutate(tmp_scores = opti3) %>%
          psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(Berkowitz::lm2.2,  new_data = .)
        #### Last attempt
        ability_estimate_arrhythmic_last_attempt <-
          last_attempt_trial_table %>%
          dplyr::filter(!rhythmic) %>%
          dplyr::select(N, step.cont.loc.var, tonalness, log_freq, opti3) %>%
          dplyr::mutate(tmp_scores = opti3) %>%
          psychTestRCATME::predict_based_on_mixed_effects_arrhythmic_model(Berkowitz::lm2.2,  new_data = . )


        ## Rhythmic

        ### Mean opti3
        mean_opti3_rhythmic <- trial_table %>%
          dplyr::filter(rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)
        #### First attempt
        mean_opti3_rhythmic_first_attempt <- first_attempt_trial_table %>%
          dplyr::filter(rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)
        #### Last attempt
        mean_opti3_rhythmic_last_attempt <- last_attempt_trial_table %>%
          dplyr::filter(rhythmic) %>%
          dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
          dplyr::pull(mean_opti3)

        ### Ability estimate
        #### First attempt
        ability_estimate_rhythmic_first_attempt <-
          first_attempt_trial_table %>%
          dplyr::filter(rhythmic) %>%
          dplyr::select(N, step.cont.loc.var, log_freq, d.entropy, i.entropy, opti3) %>%
          dplyr::mutate(tmp_scores = opti3) %>%
          psychTestRCATME::predict_based_on_mixed_effects_rhythmic_model(Berkowitz::lm3.2,  new_data = .)
        #### Last attempt
        ability_estimate_rhythmic_last_attempt <-
          last_attempt_trial_table %>%
          dplyr::filter(rhythmic) %>%
          dplyr::select(N, step.cont.loc.var, log_freq, d.entropy, i.entropy, opti3) %>%
          dplyr::mutate(tmp_scores = opti3) %>%
          psychTestRCATME::predict_based_on_mixed_effects_rhythmic_model(Berkowitz::lm3.2,  new_data = . )


      # Append scores
      db_append_scores_session(db_con,
                               session_id,
                               mean_opti3_arrhythmic, mean_opti3_arrhythmic_first_attempt, mean_opti3_arrhythmic_last_attempt,
                               ability_estimate_arrhythmic_first_attempt, ability_estimate_arrhythmic_last_attempt,

                               mean_opti3_rhythmic, mean_opti3_rhythmic_first_attempt, mean_opti3_rhythmic_last_attempt,
                               ability_estimate_rhythmic_first_attempt, ability_estimate_rhythmic_last_attempt)

      # Update sessions table with time finished

      session_df <- get_table(db_con, 'sessions', collect = FALSE)

      complete_time <- Sys.time()

      logging::loginfo("Storing complete time as %s", complete_time)

      update <- dbplyr::copy_inline(db_con, data.frame(session_id = session_id, time_completed = complete_time))

      dplyr::rows_update(session_df, update, in_place = TRUE, by = "session_id", unmatched = "ignore")


    }
  })
}




get_test_name <- function(test, get_local, state) {
  if(is.null(test)) {
    if(get_local) {
      test <- psychTestR::get_local("test", state)
    } else {
      test <- psychTestR::get_global("test", state)
    }
  }
  test
}



get_session_trials <- function(session_id) {
  trials <- get_table("trials") %>%
    dplyr::filter(session_id == !! session_id)
}




#' Get a random selection of previous trials based on a user ID
#'
#' Note currently this doesn't distinguish between the most recent session and others further back, nor the trial type previously (arrhythmic, rhythmic etc.)
#'
#' @param db_con
#' @param user_id
#' @param no_reviews
#'
#' @return
#'
#' @examples
get_review_trials <- function(no_reviews, state) {

  # Get DB con
  db_con <- psychTestR::get_global("db_con", state)
  user_id <- psychTestR::get_global("user_id", state)
  current_test_id <- psychTestR::get_global("test_id", state)

  user_trials <- compile_item_trials(db_con, current_test_id) # Note, that there is a "session_id" argument we probably want to explore using in the future

  # Sample from previous trials
  user_trials %>%
    dplyr::select(melody, abs_melody, durations, item_id) %>%
    dplyr::slice_sample(n = no_reviews) %>%
    dplyr::collect() %>%
    dplyr::mutate(melody_no = dplyr::row_number() )


}


item_bank_name_to_id <- function(db_con, ib_name) {
  get_table(db_con, "item_banks") %>%
    dplyr::filter(item_bank_name == ib_name) %>%
    dplyr::collect() %>%
    dplyr::pull(item_bank_id)
}

# get_user_trials_from_last_session("Seb")


#' Get a user's range based on their user ID
#'
#' @param user_id
#'
#' @return
#' @export
#'
#' @examples
get_range_from_user_id <- function(db_con, user_id, instrument_id = 1L) {

  res <- get_table(db_con, 'user_instrument_info') %>%
    dplyr::filter(user_id == user_id, instrument_id == instrument_id)

  list(bottom_range = res$bottom_range, top_range = res$top_range)

}

# get_range_from_user_id(db_con, user_id = 2)

db_append_scores_trial <- function(db_con,
                                   trial_length,
                                   no_recalled_notes,
                                   no_correct_notes,
                                   no_error_notes,
                                   no_correct_notes_octaves_allowed,
                                   no_error_notes_octaves_allowed,
                                   proportion_of_correct_note_events,
                                   proportion_of_correct_note_events_octaves_allowed,
                                   proportion_of_stimuli_notes_found,
                                   proportion_of_stimuli_notes_found_octaves_allowed,
                                   opti3,
                                   ngrukkon,
                                   harmcore,
                                   rhythfuzz,
                                   melody_dtw,
                                   mean_cents_deviation_from_nearest_stimuli_pitch,
                                   mean_cents_deviation_from_nearest_midi_pitch,
                                   melody_note_accuracy,
                                   melody_interval_accuracy,
                                   accuracy,
                                   precision,
                                   recall,
                                   F1_score,
                                   PMI,
                                   trial_id) {

  scores_df <- tibble::tibble(
    trial_length = trial_length,
    no_recalled_notes = no_recalled_notes,
    no_correct_notes = no_correct_notes,
    no_error_notes = no_error_notes,
    no_correct_notes_octaves_allowed = no_correct_notes_octaves_allowed,
    no_error_notes_octaves_allowed = no_error_notes_octaves_allowed,
    proportion_of_correct_note_events = proportion_of_correct_note_events,
    proportion_of_correct_note_events_octaves_allowed = proportion_of_correct_note_events_octaves_allowed,
    proportion_of_stimuli_notes_found = proportion_of_stimuli_notes_found,
    proportion_of_stimuli_notes_found_octaves_allowed = proportion_of_stimuli_notes_found_octaves_allowed,
    opti3 = opti3,
    ngrukkon = ngrukkon,
    harmcore = harmcore,
    rhythfuzz = rhythfuzz,
    melody_dtw = melody_dtw,
    mean_cents_deviation_from_nearest_stimuli_pitch = mean_cents_deviation_from_nearest_stimuli_pitch,
    mean_cents_deviation_from_nearest_midi_pitch = mean_cents_deviation_from_nearest_midi_pitch,
    melody_note_accuracy = melody_note_accuracy,
    melody_interval_accuracy = melody_interval_accuracy,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    F1_score = F1_score,
    PMI = PMI,
    trial_id = trial_id)

  db_append_to_table(db_con, table = "scores_trial", data = scores_df)


}

