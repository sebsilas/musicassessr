
instantiate_db <- function(host, username, password, overwrite = FALSE) {

  # leave this unexported, to make it less easy to accidentally delete data

  if(overwrite) {

    if(askYesNo("Careful! Are you sure you want to overwrite the underlying database?")) {

      if(askYesNo("Are you really sure?!")) {
        con <- connect_to_db(host, username, password)

        print('instantiate new user')

        backup_db()

        # create a table to store trials

        trials <- tibble::tibble(
          # NB: take the times and dates from psychTestR
          file_name = character(),
          test_username = character(),
          test = character(),
          trial_id = numeric(),
          session_id = character(),
          abs_melody = character(),
          melody = character(),
          opti3 = numeric(),
          N = numeric(),
          step.cont.loc.var = numeric(),
          tonalness = numeric(),
          log_freq = numeric(),
          attempt = numeric())


        sessions <- tibble::tibble(
          # NB: take the times and dates from psychTestR
          test_username = character(),
          test = character(),
          session_id = character(),
          time_started = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
          time_completed = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
          session_length = numeric(),
          mean_opti3 = numeric(),
          ability_estimate = numeric())

        production <- tibble::tibble(
          file_name = character(),
          test_username = character(),
          test = character(),
          session_id = character(),
          trial_id = integer(),
          abs_melody = character(),
          melody = character(),
          onset = numeric(),
          dur = numeric(),
          freq = numeric(),
          note = numeric(),
          user_pitch_classes = character(),
          correct_note = logical())

        DBI::dbWriteTable(con, "production", production, overwrite = overwrite)
        DBI::dbWriteTable(con, "trials", trials, overwrite = overwrite)
        DBI::dbWriteTable(con, "sessions", sessions, overwrite = overwrite)
        print('tables: ')
        print(DBI::dbListTables(con))
        DBI::dbDisconnect(con)

      }

    }

  }

}



#' Get a postgres table
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
get_table <- function(name = c("trials", "sessions", "production")) {

  con <- connect_to_db()

  if (name > 1) {
    name <- name[1]
  }
  print(name)
  tab <- DBI::dbReadTable(con, name)

  DBI::dbDisconnect(con)
  tab
}




backup_db <- function() {

  trials <- get_table("trials")
  sessions <- get_table("sessions")
  production <- get_table("production")

  save(trials, sessions, production, file = paste0("backup/LongitudinalBackup_",
                                                   format(Sys.time(), "%a_%b_%d_%X_%Y"), ".rda"))

}




#' Connect to a postgres DB
#'
#' @param host
#' @param username
#' @param password
#'
#' @return
#' @export
#'
#' @examples
connect_to_db <- function(host = Sys.getenv("DB_HOST"),
                          db_name = Sys.getenv("DB_NAME"),
                          username = Sys.getenv("DB_USER"),
                          password = Sys.getenv("DB_PASSWORD")) {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = 5432,
    dbname = db_name,
    user = username,
    password = password
  )
  print('connect to DB')
  print("tables: ")
  print(DBI::dbListTables(con))
  con
}


append_to_db <- function(db_name, value) {

  con <- connect_to_db(host, username, password)

  DBI::dbWriteTable(conn = con,
                    name = db_name,
                    value = value,
                    append = TRUE)

  print('added trial: ')
  print(dplyr::tbl(con, db_name))
  DBI::dbDisconnect(con)
}

db_add_new_trial <- function(test_username,
                             test,
                             session_id,
                             melody,
                             opti3,
                             file_name,
                             N,
                             step.cont.loc.var,
                             tonalness,
                             log_freq,
                             attempt) {

  print('db_add_new_trial')
  cat(
    N,
    step.cont.loc.var,
    tonalness,
    log_freq,
    attempt
  )

  trial_id <- nrow(get_table("trials")) + 1

  trial <- tibble::tibble_row(
    file_name = file_name,
    test_username = test_username,
    test = test,
    trial_id = trial_id,
    session_id = session_id,
    abs_melody = melody,
    melody = paste0(diff(itembankr::str_mel_to_vector(melody)), collapse = ","),
    opti3 = opti3,
    N = N,
    step.cont.loc.var = step.cont.loc.var,
    tonalness = tonalness,
    log_freq = log_freq,
    attempt = attempt)

  append_to_db("trials", trial)

  return(trial_id)

}


db_add_new_session <- function(test_username,
                               test,
                               session_id,
                               time_started,
                               time_completed = lubridate::now(),
                               mean_opti3,
                               ability_estimate) {


  session <- tibble::tibble_row(
    # NB: take the times and dates from psychTestR
    test_username = test_username,
    test = test,
    session_id = session_id,
    time_started = as.POSIXct(time_started, format = "%Y-%m-d-H:%M:%OS"),
    time_completed = as.POSIXct(time_completed, format = "%Y-%m-d-H:%M:%OS"),
    session_length = as.numeric(time_completed - time_started),
    mean_opti3 = mean_opti3,
    ability_estimate = ability_estimate)

  append_to_db("sessions", session)

}



db_add_new_production <- function(test_username,
                                  test,
                                  session_id,
                                  trial_id,
                                  melody,
                                  pyin_res) {

  if(!is.null(pyin_res$pyin_res)) {
    pyin_res <- pyin_res$pyin_res
  }

  if(all(is.na(pyin_res))) {

    production <- pyin_res %>%
      dplyr::mutate(
        test_username = test_username,
        test = test,
        session_id = session_id,
        trial_id = trial_id,
        abs_melody = melody,
        melody = paste0(diff(itembankr::str_mel_to_vector(melody)), collapse = ","),
        user_pitch_classes = as.character(NA),
        correct_note = as.logical(NA))

  } else {

    melody_v <- itembankr::str_mel_to_vector(melody)
    stimuli_pitch_classes <- itembankr::midi_to_pitch_class(melody_v)

    production <- pyin_res %>%
      dplyr::mutate(
        test_username = test_username,
        test = test,
        session_id = session_id,
        trial_id = trial_id,
        abs_melody = melody,
        melody = paste0(diff(itembankr::str_mel_to_vector(melody)), collapse = ","),
        user_pitch_classes = dplyr::case_when(is.na(note) ~ as.character(NA), TRUE ~ as.character(itembankr::midi.to.pitch.classes.list[note])),
        correct_note = user_pitch_classes %in% stimuli_pitch_classes)

  }


  append_to_db("production", production)

}



add_trial_to_db <- function(test_username = NA,
                            test,
                            session_id,
                            melody,
                            opti3,
                            pyin_res,
                            N,
                            step.cont.loc.var,
                            tonalness,
                            log_freq,
                            attempt) {


  trial_id <- db_add_new_trial(test_username,
                               test,
                               session_id,
                               melody,
                               opti3,
                               pyin_res$pyin_res$file_name[1],
                               N,
                               step.cont.loc.var,
                               tonalness,
                               log_freq,
                               attempt)

  db_add_new_production(test_username, test, session_id, trial_id, melody, pyin_res)

}

#' Add session to postgres db
#'
#' @param take_scores_from
#' @param get_local
#' @param test
#'
#' @return
#' @export
#'
#' @examples
elt_add_session_to_db <- function(take_scores_from = c("db_table", "session_variable"),
                                  get_local = FALSE,
                                  test = NULL) {

  psychTestR::code_block(function(state, ...) {

    store_results_in_db <- psychTestR::get_global("store_results_in_db", state)

    if(store_results_in_db) {

      test_username <- psychTestR::get_global("test_username", state)

      test <- get_test_name(test, get_local, state)

      session_info <- psychTestR::get_session_info(state, complete = TRUE)
      session_id <- session_info$p_id
      time_started <- session_info$time_started
      time_completed <- session_info$current_time

      trial_table <- musicassessr::get_table("trials") %>%
        dplyr::filter(session_id == !! session_id,
                      test == !! test,
                      test_username == !! test_username) %>%
        dplyr::group_by(melody) %>%
        dplyr::slice_max(order_by = attempt, with_ties = FALSE) %>%
        dplyr::ungroup()

      mean_opti3 <- trial_table %>%
        dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
        dplyr::pull(mean_opti3)


      if (take_scores_from == "session_variable") {

        print('taking scores from session_variable')

        presampled_items <- psychTestR::get_local("presampled_items", state)

        new_data <- cbind(presampled_items, tibble::tibble(opti3 = trial_table$opti3, attempt = trial_table$attempt))

      } else if(take_scores_from == "db_table") {

        print('taking scores from db_table')

        new_data <- trial_table

      } else {
        stop('take_scores_from not recognised')
      }

      new_data <- new_data %>%
        dplyr::select(N, step.cont.loc.var, tonalness, opti3) %>%
        dplyr::mutate(log_freq = 0, # use mean for now
                      tmp_scores = opti3)


      ability_estimate <- psychTestRCATME::predict_based_on_mixed_effects_model(Berkowitz::lm2.2_scaled,
                                                                                new_data)


      db_add_new_session(test_username, test, session_id, time_started, time_completed, mean_opti3, ability_estimate)
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




# instantiate_db(host = host, username = username, password = password, overwrite = TRUE)

# trials <- get_table("trials")
# sessions <- get_table("sessions")
# production  <- get_table("production")








