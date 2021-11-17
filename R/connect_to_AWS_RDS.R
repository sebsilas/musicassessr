connect_to_db <- function(host, username, password) {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = 5432,
    dbname = username,
    user = username,
    password = password
  )
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
        test_username = character(),
        test = character(),
        trial_id = numeric(),
        session_id = character(),
        time_started = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        time_completed = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        melody = character(),
        session_length = numeric(),
        mean_opti3 = numeric(),
        mean_accuracy = numeric(),
        ability_estimate = numeric())


      sessions <- tibble::tibble(
        # NB: take the times and dates from psychTestR
        test_username = character(),
        test = character(),
        session_id = character(),
        time_started = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        time_completed = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        session_length = numeric(),
        mean_opti3 = numeric(),
        mean_accuracy = numeric(),
        ability_estimate = numeric())

      production <- tibble::tibble(
        test_username = character(),
        test = character(),
        session_id = character(),
        trial_id = integer(),
        time_started = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        time_completed = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),
        session_length = character(),
        melody = character(),
        onset = numeric(),
        dur = numeric(),
        freq = numeric(),
        note = numeric(),
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
                    name = "trials",
                    value = value,
                    append = TRUE)

  print('added trial: ')
  print(dplyr::tbl(con, db_name))
  DBI::dbDisconnect(con)
}

db_add_new_trial <- function(test_username, test, session_id, time_started, time_completed = lubridate::now(), melody, opti3, accuracy) {

  trial <- tibble::tibble_row(
                      test_username = test_username,
                      test = test,
                      trial_id = nrow(get_table("trials")) + 1,
                      session_id = session_id,
                      time_started = as.POSIXct(NA, format = "%Y-%m-d-H:%M:%OS"),
                      time_completed = as.POSIXct(NA, format = "%Y-%m-d-H:%M:%OS"),
                      melody = melody,
                       opti3 = opti3,
                       accuracy = accuracy)

  append_to_db("trials", trial)


}


db_add_new_session <- function(test_username, test, session_id, time_started, time_completed = lubridate::now(),
                               mean_opti3, mean_accuracy, ability_estimate) {

  session <- tibble::tibble_row(
    # NB: take the times and dates from psychTestR
    test_username = test_username,
    test = test,
    session_id = session_id,
    time_started = as.POSIXct(NA, format = "%Y-%m-d-H:%M:%OS"),
    time_completed = as.POSIXct(NA, format = "%Y-%m-d-H:%M:%OS"),
    session_length = as.numeric(time_completed - time_started),
    mean_opti3 = mean_opti3,
    mean_accuracy = mean_accuracy,
    ability_estimate = ability_estimate)

  append_to_db("sessions", session)

}


db_add_new_production <- function(test_username, test, session_id, trial_id, time_started, time_completed,
                               melody, pyin_res) {

    production <- tibble::tibble_row(
      test_username = test_username,
      test = test,
      session_id = session_id,
      trial_id = trial_id,
      time_started = time_started,
      time_completed = time_completed,
      session_length = as.numeric(time_completed - time_started),
      melody = melody,
      onset = pyin_res$onset,
      dur = pyin_res$dur,
      freq = pyin_res$freq,
      note = pyin_res$note,
      correct_note = pyin_res$correct)

    append_to_db("production", production)

}

# retrieve a table
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

add_trial_to_db <- function(test_username, test, session_id, time_started, time_completed, melody, opti3, accuracy, pyin_res) {
  print('add_trial_to_db')
  print(test_username)
  print(test)
  print(session_id)
  print(time_started)
  print(time_completed)
  print(melody)
  print(opti3)
  print(accuracy)
  print(pyin_res)
  db_add_new_trial(test_username, test, session_id, time_started, time_completed, melody, opti3, accuracy)
  db_add_new_production(test_username, test, session_id, trial_id, time_started, time_completed, melody, pyin_res)
}


elt_add_session_to_db <- function(mean_opti3, mean_accuracy, ability_estimate) {
  psychTestR::code_block(function(state, ...) {

    store_results_in_db <- psychTestR::get_global("store_results_in_db", state)
    if(store_results_in_db) {
      test_username <- psychTestR::get_global("test_username", state)
      test <- psychTestR::get_global("test", state)
      session_info <- psychTestR::get_session_info(state, complete = TRUE)
      session_id <- session_info$p_id
      time_started <- session_info$current_time
      time_completed <- session_info$current_time
      db_add_new_session(test_username, test, session_id, time_started, time_completed, mean_opti3, mean_accuracy, ability_estimate)
    }
  })
}







host <- "localhost"
username <- "postgres"
password <- "ilikecheesepie432"



#instantiate_db(host = host, username = username, password = password, overwrite = TRUE)


# get_table("sessions")
# get_table("production")
# get_table("trials")




### end of trial::

# db_add_new_trial(session_id, time = lubridate::now(), date = lubridate::now(), melody, opti3, accuracy)


# db_add_new_production(session_id, trial_id, time_started, time_completed, melody, pyin_res)


### end of test::

# db_add_new_session(session_id, time_started, time_completed, mean_opti3, mean_accuracy, ability_estimate)





# AWS

# Connect to the database using an IAM authentication token.
rds <- paws::rds()

token <- rds$build_auth_token("melodypost.csrfnleurwwu.eu-central-1.rds.amazonaws.com", "eu-central-1b", "postgres")

# con <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   host = "melodypost.csrfnleurwwu.eu-central-1.rds.amazonaws.com", port = 5432, dbname = "postgres",
#   user = "postgres", password = "ilikecheesepie432"
# )

# con <- instantiate_db(host = "melodypost.csrfnleurwwu.eu-central-1.rds.amazonaws.com",
#                                username = "postgres",
#                                password = "ilikecheesepie432")
