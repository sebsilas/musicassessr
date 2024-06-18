

#' Initiate a musicassessr test
#'
#' @param app_name
#' @param experiment_id
#' @param experiment_condition_id
#' @param user_id
#' @param asynchronous_api_mode
#' @param default_range
#' @param username
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
                              default_range = set_default_range("Piano"),
                              username = NULL) {


  psychTestR::join(


    # In multiple test time lines, we want to be able to change between e.g., Voice and Instrument
    # So this aspect (code_block) of musicassessr_init can be fired twice in a single session...

    psychTestR::code_block(function(state, ...) {

      if(!is.null(instrument_id)) {
        psychTestR::set_global("instrument_id", instrument_id, state)
      }

      if(!is.null(inst)) {
        psychTestR::set_global("inst", inst, state)
      }

    }),

    # ... conversely, we do not want to fire the async API stuff more than once. Hence, the conditional.

    psychTestR::conditional(function(state, ...) {

      is.null(psychTestR::get_global("musicassessr_inited", state))

    }, logic = psychTestR::join(


      # Set default range (which can be customised or instead replaced later by the user via setup pages)
      set_instrument_range(default_range$bottom_range,
                           default_range$top_range,
                           default_range$clef),



      psychTestR::reactive_page(function(state, ...) {


        psychTestR::set_global("asynchronous_api_mode", asynchronous_api_mode, state)


        if(asynchronous_api_mode) {

          # Make sure the app matches the server
          Sys.setenv(TZ="UTC")

          if(is.null(user_id) && is.null(psychTestR::get_global("user_id", state))) {

            logging::loginfo("Grabbing user_id from URL parameter")


            # NB. The URL params MUST be collected in a reactive_page

            url_params <- psychTestR::get_url_params(state)

            job_id <- url_params$job_id
            session_token <- url_params$session_token

            if(!is.null(job_id)) {
              logging::loginfo("job_id %s", job_id)
              psychTestR::set_global("job_id", job_id, state)
            }


            if(!is.null(session_token)) {

              session_token_res <- musicassessrdb::check_jwt(session_token)

              if(session_token_res$success) {
                username <- session_token_res$username
                user_id <- session_token_res$user_id
              }
            }

          }



          if(!is.null(username)) {
            logging::loginfo("username %s", username)
            psychTestR::set_global("username", username, state)
          }


          if(!is.null(user_id)) {
            logging::loginfo("user_id %s", user_id)
            psychTestR::set_global("user_id", user_id, state)
          }


          if(Sys.getenv("ENDPOINT_URL") == "") {
            stop("You need to set the ENDPOINT_URL!")
          }


          user_id <- psychTestR::get_global("user_id",state)
          username <- psychTestR::get_global("username",state)

          if(asynchronous_api_mode && is.null(user_id) && is.null(username)) {
            stop("user_id or username should not be NULL, at this point. It should have been set through the test or the URL parameter.")
          }

        }


        return_correct_entry_page(asynchronous_api_mode, user_id, username)

      }),

      psychTestR::code_block(function(state, ...) {

          # NB. the async function MUST happen in a code_block or it will block

          logging::loginfo("Create future")

          if(is.null(psychTestR::get_global("session_api_called", state)) && asynchronous_api_mode && is.null(psychTestR::get_global("session_id", state))) { # Make sure it doesn't fire twice

            if(is.null(user_id)) {
              user_id <- psychTestR::get_global("user_id", state)
            }

            session_id <- future::future({

              logging::loginfo("Call store_db_session_api asynchronously")
                logging::loginfo("experiment_id: %s", experiment_id)
                logging::loginfo("experiment_condition_id: %s", experiment_condition_id)
                logging::loginfo("user_id: %s", user_id)

                res <- musicassessrdb::store_db_session_api(experiment_id, experiment_condition_id, user_id)
                res

            }, seed = NULL, future.plan = future::multisession) %...>% (function(result) {

              logging::loginfo("Returning promise result: %s", result)
              if(is.scalar.na.or.null(result)) {
                return(NA)
              } else if(result$status == 200) {
                return(result)
              } else {
                return(NA)
              }

            })
            psychTestR::set_global("session_api_called", TRUE, state)
            logging::loginfo("...future created.")

            if(is.null(psychTestR::get_global("session_id", state))) {
              psychTestR::set_global("session_id", session_id, state)
              logging::loginfo("session_id set")
            }

          }

          logging::loginfo("Set vars")

          session_info <- psychTestR::get_session_info(state, complete = FALSE)
          psychTestR_session_id <- session_info$p_id
          # Set vars
          psychTestR::set_global("psychTestR_session_id", psychTestR_session_id, state)
          psychTestR::set_global("compute_session_scores_and_end_session_api_called", FALSE, state)
          psychTestR::set_global("app_name", app_name, state)
          psychTestR::set_global("scores", c(), state)
          psychTestR::set_global("experiment_id", experiment_id, state)
          psychTestR::set_global("experiment_condition_id", experiment_condition_id, state)
          # So that we don't do this again
          psychTestR::set_global("musicassessr_inited", TRUE, state)

      })
    )

    )
  )
}

return_correct_entry_page <- function(asynchronous_api_mode, user_id, username) {

  if(asynchronous_api_mode && is.null(user_id) && is.null(username)) {
    ui <- shiny::tags$div(musicassessr_css(), shiny::tags$p('You could not be validated.'))
  } else if(asynchronous_api_mode && !is.null(user_id) && !is.null(username)) {
    ui <- async_success_ui(username)
  } else {
    ui <- shiny::tags$div(musicassessr_css(), shiny::tags$p("Let's proceed!"))
  }

  ui <- shiny::tags$div(
    ui,
    shiny::tags$div(shiny::tags$input(id = "user_info"), class = "_hidden"),
    shiny::tags$button(psychTestR::i18n("Next"), id="getUserInfoButton", onclick="getUserInfo();testFeatureCapability();next_page();", class="btn btn-default action-button")
  )

  psychTestR::page(ui = ui,
                   label = "user_info",
                   save_answer = TRUE,
                   get_answer = user_info_check,
                   on_complete = if(asynchronous_api_mode) user_info_async else NULL)

}


async_success_ui <- function(username) {
  shiny::tags$div(
    musicassessr_css(),
    shiny::tags$script("var upload_to_s3 = true; console.log('Turning S3 mode on');"),
    shiny::tags$p(paste0("Hello ", username, "!"))
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

      lowest_reading_note <- inst$lowest_reading_note

      logging::loginfo("lowest_reading_note: %s", lowest_reading_note)

      psychTestR::set_global("lowest_reading_note", lowest_reading_note, state)

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



