

#' Initiate a musicassessr test
#'
#' @param app_name
#' @param experiment_id
#' @param experiment_condition_id
#' @param user_id
#' @param asynchronous_api_mode
#'
#' @return
#' @export
#'
#' @examples
musicassessr_init <- function(app_name = "",
                              experiment_id = NULL,
                              experiment_condition_id = NULL,
                              user_id = NULL,
                              asynchronous_api_mode = FALSE,
                              instrument_id = NULL,
                              inst = NULL,
                              default_range = set_default_range("Piano")) {

  psychTestR::join(

    set_instrument_range(default_range$bottom_range,
                         default_range$top_range,
                         default_range$clef),

    psychTestR::code_block(function(state, ...) {

      psychTestR::set_global("asynchronous_api_mode", asynchronous_api_mode, state)

      if(!is.null(instrument_id)) {
        psychTestR::set_global("instrument_id", instrument_id, state)
      }

      if(!is.null(inst)) {
        psychTestR::set_global("inst", inst, state)
      }


      user_id <- if(is.null(user_id)) psychTestR::get_global('user_id', state) else user_id

      if(asynchronous_api_mode && is.null(user_id)) {
        stop("user_id should not be NULL, at this point, if using asynchronous_api_mode User validate_user_entry_into test or set through the test manually.")
      }

      session_info <- psychTestR::get_session_info(state, complete = FALSE)
      psychTestR_session_id <- session_info$p_id
      # Set vars
      psychTestR::set_global("psychTestR_session_id", psychTestR_session_id, state)
      psychTestR::set_global("compute_session_scores_and_end_session_api_called", FALSE, state)
      psychTestR::set_global("app_name", app_name, state)
      psychTestR::set_global("scores", c(), state)
      psychTestR::set_global("experiment_id", experiment_id, state)
      psychTestR::set_global("experiment_condition_id", experiment_condition_id, state)
      psychTestR::set_global("user_id", user_id, state)

    })
  )
}


#' Call API on start
#'
#' @param experiment_id,
#' @param experiment_condition_id
#' @param user_id
#'
#' @return
#' @export
#'
#' @examples
call_api_on_start <- function(experiment_id = NA, experiment_condition_id = NA, user_id = NULL) {

  function(state, session) {

    if(is.null(user_id)) {
      logging::loginfo("Grabbing user_id from URL parameter")
      url_params <- psychTestR::get_url_params(state)
      user_id <- url_params$user_id
      logging::loginfo("user_id %s", user_id)
    }


    logging::loginfo("Turn Asynchronous API mode on...")
    future::plan(future::multisession, workers = 2)

    logging::loginfo("Appending session via API")

    if(Sys.getenv("ENDPOINT_URL") == "") {
      stop("You need to set the ENDPOINT_URL!")
    }

    musicassessr_session_id <<- future::future({
      future_res <- musicassessrdb::store_db_session_api(experiment_id, experiment_condition_id, user_id)
      print('future_res...')
      print(future_res)
      future_res
    }, seed = NULL) %...>% (function(result) {
      logging::loginfo("Returning promise result: %s", result)
      if(result$status == 200) {
        return(result$session_id)
      } else {
        return(NA)
      }
    })

  }

}

#'
#' #' Set session ID later
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' set_session_id_later <- function() {
#'   psychTestR::code_block(function(state, ...) {
#'     session_id <- get_promise_value(musicassessr_session_id)
#'     logging::loginfo("Sending musicassessr_session_id to psychTestR... %s", session_id)
#'     psychTestR::set_global("session_id", session_id, state)
#'   })
#' }

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
#'
#' @return
#' @export
#'
#' @examples
set_instrument <- function(instrument_id = NULL) {

  psychTestR::code_block(function(state, ...) {

    if(!is.null(instrument_id)) {


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

    }

  })

}



