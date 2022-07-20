
get_self_chosen_anonymous_id <- function() {

  psychTestR::join(
    psychTestR::get_p_id(prompt = "Please provide an anonymous identifier that only you will know, in case something goes wrong."),
    psychTestR::reactive_page(function(state, ...) {
      p_id <- psychTestR::answer(state)
      psychTestR::set_global("p_id", p_id, state)
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$script(paste0('const p_id = \"', p_id, '\";')),
        shiny::tags$p(paste0("Thank you, ", p_id, ". Now let's set things up."))))
    })
  )

}

pass_p_id_to_js <- function() {
  psychTestR::reactive_page(function(state, ...) {
    p_id <- psychTestR::p_id(state)
    psychTestR::set_global("p_id", p_id, state)
    psychTestR::one_button_page(shiny::tags$div(
      shiny::tags$script(paste0('const p_id = \"', p_id, '\";')),
      shiny::tags$p(paste0("Now let's set things up."))))
  })
}
