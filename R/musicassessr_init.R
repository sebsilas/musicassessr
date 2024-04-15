

#' Initiate a musicassessr test
#'
#' @param use_musicassessr_db
#' @param app_name
#' @param experiment_id
#' @param experiment_condition_id
#' @param user_id
#' @param asynchronous_api_mode
#' @param instrument_id
#' @param inst
#' @param default_range
#'
#' @return
#' @export
#'
#' @examples
musicassessr_init <- function(use_musicassessr_db = FALSE,
                              app_name = "",
                              experiment_id = NULL,
                              experiment_condition_id = NULL,
                              user_id = NULL,
                              asynchronous_api_mode = FALSE,
                              instrument_id = NULL,
                              inst = NULL,
                              default_range = set_default_range("Piano")) {

  if(asynchronous_api_mode) {
    logging::loginfo("Turn Asynchronous API mode on...")
    future::plan(future::multisession, workers = 2)
    logging::loginfo("...Asynchronous API mode turned on.")
  }

  psychTestR::join(

    set_instrument_range(default_range$bottom_range,
                         default_range$top_range,
                         default_range$clef),

    psychTestR::code_block(function(state, ...) {

      psychTestR::set_global("musicassessr_db", use_musicassessr_db, state)
      psychTestR::set_global("asynchronous_api_mode", asynchronous_api_mode, state)

      if(!is.null(instrument_id)) {
        psychTestR::set_global("instrument_id", instrument_id, state)
      }

      if(!is.null(inst)) {
        psychTestR::set_global("inst", inst, state)
      }

      if(use_musicassessr_db) {

        if(!asynchronous_api_mode) {

          # Init the DB connection (and return it for immediate use)
          db_con <- musicassessrdb::connect_to_db_state(state)

          # Check the specified IDs exist in the DB
          musicassessrdb::check_ids_exist(db_con, experiment_id, experiment_condition_id, user_id)
        }
        session_info <- psychTestR::get_session_info(state, complete = FALSE)
        psychTestR_session_id <- session_info$p_id
        time_completed <- NULL # The test is beginning
        # time_started is auto-generated at the SQL level
        user_id <- if(is.null(user_id)) psychTestR::get_global('user_id', state) else user_id

        if(is.null(user_id)) {
          stop("user_id should not be NULL, at this point, if using musicassessr_db. User validate_user_entry_into test or set through the test manually.")
        }


        if(is.null(psychTestR::get_global("session_id", state))) { # This makes sure we don't save the same session twice in the DB (e.g., when there are multiple tests nested in one session)
          # Append session
          # N.B This session_id is the primary key in the sessions database

          if(asynchronous_api_mode) {

            logging::loginfo("Appending session via API")

            if(Sys.getenv("ENDPOINT_URL") == "") {
              stop("You need to set the ENDPOINT_URL!")
            }

            session_id <- future::future({
                musicassessrdb::store_db_session_api(experiment_id, experiment_condition_id, user_id, psychTestR_session_id, time_completed)
              }) %...>% (function(result) {
              logging::loginfo("Returning promise result: %s", result)
                if(result$status == 200) {
                  return(result$session_id)
                } else {
                  return(NA)
                }
            })

          } else {
            logging::loginfo("Appending session directly to DB")
            session_id <- musicassessrdb::db_append_session(db_con, experiment_condition_id, user_id, psychTestR_session_id, time_completed, experiment_id)
          }

          psychTestR::set_global("session_id", session_id, state)

        }

      }

      # Set vars
      psychTestR::set_global("app_name", app_name, state)
      psychTestR::set_global("use_musicassessr_db", use_musicassessr_db, state)
      psychTestR::set_global("scores", c(), state)
      psychTestR::set_global("experiment_id", experiment_id, state)
      psychTestR::set_global("experiment_condition_id", experiment_condition_id, state)
      psychTestR::set_global("user_id", user_id, state)

    })
  )
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

    if(!is.null(test_id) && ! psychTestR::get_global("asynchronous_api_mode", state)) {

      db_con <- psychTestR::get_global("db_con", state)

      if(is.null(db_con)) {
        stop("If test_id is non-NULL, then use_musicassessr_db must be true.")
      }

      if(!is.null(db_con)) {
        musicassessrdb::check_id_exists(db_con, table_name = "tests", id_col = "test_id", id = test_id)
      }

    }

    logging::loginfo("Setting test ID: %s", test_id)

    # We need to do this at global level, becauses tests may have many modules.
    # Hence, in multi test timelines, you need overwrite the below globally when a new test begins
    psychTestR::set_global("test_name", test_name, state)
    psychTestR::set_global("test_id", test_id, state)
  })

}



#' Set instrument ID
#'
#' @param instrument_id
#' @param as_code_block
#' @param state
#' @param set_range
#'
#' @return
#' @export
#'
#' @examples
set_instrument <- function(instrument_id = NULL, as_code_block = TRUE, state = NULL, set_range = TRUE) {

  if(!as_code_block && is.null(state)) {
    stop("state must be a state object if as_code_block is FALSE")
  }

  set_inst <- function(state, ...) {

    if(!is.null(instrument_id)) {

      db_con <- psychTestR::get_global("db_con", state)

      if(!is.null(db_con)) {
        if(DBI::dbIsValid(db_con)) {
          musicassessrdb::check_id_exists(db_con, table_name = "instruments", id_col = "instrument_id", id = instrument_id)
        }
      }

      logging::loginfo("Setting instrument ID, manually specified. ID: %s", instrument_id)

      psychTestR::set_global("instrument_id", instrument_id, state)

      inst <- insts_table %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(id == instrument_id)

      logging::loginfo("Instrument: %s", inst$en)
      logging::loginfo("Transpose: %s", inst$transpose)
      logging::loginfo("Clef: %s", inst$clef)

      psychTestR::set_global("inst", inst$en, state)
      psychTestR::set_global("transpose_visual_notation", as.integer(inst$transpose), state)
      psychTestR::set_global("clef", inst$clef, state)

      if(set_range) {

        logging::loginfo("Setting range based on selection")
        logging::loginfo("Bottom range: %s", inst$low_note)
        logging::loginfo("Top range: %s", inst$high_note)

        psychTestR::set_global("bottom_range", inst$low_note, state)
        psychTestR::set_global("top_range", inst$high_note, state)
      }


    }

  }

  if(as_code_block) {
    return(psychTestR::code_block(set_inst))
  } else {
    set_inst(state)
  }



}
