# functions for testing purposes (i.e to speed up development)

#' Set a dummy range (to avoid having to manually set in the test)
#'
#' @param bottom_range
#' @param top_range
#'
#' @return
#' @export
#'
#' @examples
fake_range <- function(bottom_range = 48, top_range = 72) {
  psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("bottom_range", bottom_range, state)
    psychTestR::set_global("top_range", top_range, state)
  })
}
