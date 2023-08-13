




# FYI: pgAdmin 4 is what I use to monitor things

# https://stackoverflow.com/questions/1110349/how-can-i-define-a-composite-primary-key-in-sql

# time_started = as.POSIXct(character(), format = "%Y-%m-d-H:%M:%OS"),

#' Get a postgres table
#'
#' @param db_con
#' @param name
#'
#' @return
#' @export
#'
#' @examples
get_table <- function(db_con, name) {

  logging::loginfo("Getting table %s", name)

  tab <- DBI::dbReadTable(db_con, name)

  tab
}




backup_db <- function() {

  trials <- get_table("trials")
  sessions <- get_table("sessions")
  production <- get_table("production")

  save(trials, sessions, production, file = paste0("backup/LongitudinalBackup_", format(Sys.time(), "%a_%b_%d_%X_%Y"), ".rda"))

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

  logging::loginfo('Connecting to database %s', db_name)
  con
}


#' Append a data frame to a table in the database
#'
#'
#' @param db_con A database connection environment
#' @param table Name of table to which to append in database
#' @param data dataframe to be appended
#'
#' @return Returns number of rows affected
#' @export
db_append_to_table <- function(db_con, table, data, primary_key_col = NULL){

  entries <- nrow(data)
  logging::loginfo("Appending %s entries to table %s", entries, table)
  res <- DBI::dbAppendTable(db_con, table, data)

  if(!is.null(primary_key_col)) {
    primary_key_col <- as.name(primary_key_col)
    primary_key <- dplyr::tbl(db_con, table) %>%
      dplyr::slice_max(!! primary_key_col) %>%
      dplyr::collect() %>%
      dplyr::pull(!! primary_key_col)
    return(primary_key)
  } else {
    return(res)
  }

}


check_ids_exist <- function(db_con, experiment_id = NULL, experiment_condition_id = NULL, user_id = NULL) {
  if(!is.null(experiment_id)) {
    if(!check_id_exists(db_con, table_name = "experiments", id_col = "experiment_id", id = experiment_id)) stop(paste0("Experiment ID ", experiment_id, " does not exist."))
  }

  if(!is.null(experiment_condition_id)) {
    if(!check_id_exists(db_con, table_name = "conditions", id_col = "condition_id", id = experiment_condition_id)) stop(paste0("Condition ID ", experiment_condition_id, " does not exist."))
  }

  if(!is.null(user_id)) {
    if(!check_id_exists(db_con, table_name = "users", id_col = "user_id", id = user_id)) stop(paste0("User ID ", user_id, " does not exist."))
  }

  logging::loginfo("All specified IDs exist in DB.")

  return(TRUE)

}

check_id_exists <- function(db_con, table_name, id_col, id) {

  tb <- get_table(db_con, table_name) %>%
    dplyr::filter(!!as.symbol(id_col) == id)

  return(nrow(tb) != 0)
}



db_disconnect_shiny <- function(state, ...) {
    db_con <- psychTestR::get_global("db_con", state)
    if(!is.null(db_con)) {
      logging::loginfo("Disconnecting from DB")
      DBI::dbDisconnect(db_con)
    }
}
