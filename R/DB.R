connect_to_db <- function(host, username, password) {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = 5432,
    dbname = username,
    user = username,
    password = password
  )
  print('connect to DB')
  print("tables: ")
  print(DBI::dbListTables(con))
  con
}

instantiate_db <- function(host, username, password, overwrite = FALSE) {

  if(overwrite) {
    if(askYesNo("Careful! Are you sure you want to overwrite the underlying database?")) {
      con <- connect_to_db(host, username, password)

      print('instantiate new user')

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
        opti3 = numeric())


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

db_add_new_trial <- function(test_username, test, session_id, melody, opti3, file_name) {

  trial_id <- nrow(get_table("trials")) + 1

  trial <- tibble::tibble_row(
    file_name = file_name,
    test_username = test_username,
    test = test,
    trial_id = trial_id,
    session_id = session_id,
    abs_melody = melody,
    melody = paste0(diff(itembankr::str_mel_to_vector(melody)), collapse = ","),
    opti3 = opti3)

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

  print('db_add_new_session')

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

  print('db_add_new_production')

  melody_v <- itembankr::str_mel_to_vector(melody)
  stimuli_pitch_classes <- itembankr::midi_to_pitch_class(melody_v)

  production <- pyin_res$pyin_res %>%
    dplyr::mutate(
      test_username = test_username,
      test = test,
      session_id = session_id,
      trial_id = trial_id,
      abs_melody = melody,
      melody = paste0(diff(itembankr::str_mel_to_vector(melody)), collapse = ","),
      user_pitch_classes = itembankr::midi_to_pitch_class(note),
      correct_note = user_pitch_classes %in% stimuli_pitch_classes)

  append_to_db("production", production)

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

  con <- connect_to_db(host, username, password)

  if (name > 1) {
    name <- name[1]
  }
  print(name)
  tab <- DBI::dbReadTable(con, name)

  DBI::dbDisconnect(con)
  tab
}

add_trial_to_db <- function(test_username = NA, test, session_id,  melody, opti3, pyin_res) {
  trial_id <- db_add_new_trial(test_username, test, session_id, melody, opti3, pyin_res$pyin_res$file_name[1])
  db_add_new_production(test_username, test, session_id, trial_id, melody, pyin_res)
}


#' Add session to postgres db
#'
#' @return
#' @export
#'
#' @examples
elt_add_session_to_db <- function() {
  psychTestR::code_block(function(state, ...) {
    store_results_in_db <- psychTestR::get_global("store_results_in_db", state)
    if(store_results_in_db) {
      test_username <- psychTestR::get_global("test_username", state)
      test <- psychTestR::get_global("test", state)
      session_info <- psychTestR::get_session_info(state, complete = TRUE)
      session_id <- session_info$p_id
      time_started <- session_info$time_started
      time_completed <- session_info$current_time

      mean_opti3 <- musicassessr::get_table("trials") %>%
        dplyr::filter(session_id == session_id) %>%
        dplyr::summarise(mean_opti3 = mean(opti3, na.rm = TRUE)) %>%
        dplyr::pull(mean_opti3)

      ability_estimate <- NA

      db_add_new_session(test_username, test, session_id, time_started, time_completed, mean_opti3, ability_estimate)
    }
  })
}

