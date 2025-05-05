
get_self_chosen_anonymous_id <- function() {

  psychTestR::join(
    psychTestR::get_p_id(prompt = p_id_prompt),
    psychTestR::reactive_page(function(state, ...) {
      p_id <- psychTestR::answer(state)
      psychTestR::set_global("p_id", p_id, state)
      psychTestR::one_button_page(shiny::tags$div(
        #shiny::tags$script(paste0('p_id = \"', p_id, '\";')),
        shiny::tags$p(paste0(psychTestR::i18n("Thank_you"), ", ", p_id, ". ", psychTestR::i18n("setup_intro")))),
        button_text = psychTestR::i18n("Next"))
    })
  )

}

pass_p_id_to_js <- function() {
  psychTestR::reactive_page(function(state, ...) {
    p_id <- psychTestR::p_id(state)
    psychTestR::set_global("p_id", p_id, state)
    psychTestR::one_button_page(shiny::tags$div(
      #shiny::tags$script(paste0('p_id = \"', p_id, '\";')),
      shiny::tags$p(psychTestR::i18n("setup_intro"))), button_text = psychTestR::i18n("Next"))
  })
}
