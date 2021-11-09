
#' Initiate a musicassessr test
#'
#' @param test_username
#' @param test
#' @param store_results_in_db
#'
#' @return
#' @export
#'
#' @examples
musicassessr_init <- function(test_username, test, store_results_in_db) {
  psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("store_results_in_db", store_results_in_db, state)
    psychTestR::set_global("test_username", test_username, state)
    psychTestR::set_global("test", test, state)

  })
}
