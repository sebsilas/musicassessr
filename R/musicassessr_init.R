


#' Initiate a musicassessr test
#'
#' @param test_username
#' @param test
#' @param store_results_in_db
#' @param local_app_file_dir
#' @param sonic_annotator_local_location
#'
#' @return
#' @export
#'
#' @examples
musicassessr_init <- function(test_username,
                              test,
                              store_results_in_db = FALSE,
                              local_app_file_dir,
                              sonic_annotator_local_location) {

  psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("store_results_in_db", store_results_in_db, state)
    psychTestR::set_global("test_username", test_username, state)
    psychTestR::set_global("test", test, state)
    psychTestR::set_global("local_app_file_dir", local_app_file_dir, state)
    psychTestR::set_global("sonic_annotator_local_location", sonic_annotator_local_location, state)

  })
}
