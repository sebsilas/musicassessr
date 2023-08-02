

#' Initiate a musicassessr test
#'
#' @param use_musicassessr_db
#' @param app_name
#' @param experiment_id
#' @param experiment_condition_id
#' @param user_id
#'
#' @return
#' @export
#'
#' @examples
musicassessr_init <- function(use_musicassessr_db = FALSE,
                              app_name = "",
                              experiment_id = NULL,
                              experiment_condition_id = NULL,
                              user_id = NULL) {


  psychTestR::code_block(function(state, ...) {

      if(use_musicassessr_db) {

        # Init the DB connection (and return it for immediate)
        db_con <- connect_to_db_state(state)
        session_info <- psychTestR::get_session_info(state, complete = FALSE)
        psychTestR_session_id <- session_info$p_id
        time_completed <- NULL # The test is beginning
        # time_started is auto-generated at the SQL level

        # Check the specified IDs exist in the DB
        check_ids_exist(db_con, experiment_id, experiment_condition_id, user_id)

        # Append session
        # N.B This session_id is the primary key in the sessions database
        session_id <- db_append_session(db_con, experiment_condition_id, user_id, psychTestR_session_id, time_completed, experiment_id)

        psychTestR::set_global("session_id", session_id, state)

      }



      psychTestR::set_global("app_name", app_name, state)
      psychTestR::set_global("use_musicassessr_db", use_musicassessr_db, state)
      psychTestR::set_global("scores", c(), state)
      psychTestR::set_global("transpose_first_melody_note", 0, state)
      psychTestR::set_global("clef", "auto", state)
      psychTestR::set_global("experiment_id", experiment_id, state)
      psychTestR::set_global("experiment_condition_id", experiment_condition_id, state)
      psychTestR::set_global("user_id", user_id, state)

  })
}


#' Set test ID
#'
#' @param test_name
#' @param test_id
#'
#' @return
#' @export
#'
#' @examples
set_test <- function(test_name, test_id = NULL) {

  psychTestR::code_block(function(state, ...) {

    if(!is.null(test_id)) {
      db_con <- psychTestR::get_global("db_con", state)
      if(is.null(db_con)) stop("If test_id is non-NULL, then use_musicassessr_db must be true.")
      check_id_exists(db_con, table_name = "tests", id_col = "test_id", id = test_id)
    }

    # We need to do this at global level, becauses tests may have many modules.
    # Hence, in multi test timelines, you need overwrite the below globally when a new test begins
    psychTestR::set_global("test_name", test_name, state)
    psychTestR::set_global("test_id", test_id, state)
  })

}


