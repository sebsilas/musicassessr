
record_key_presses_page <- function(body = shiny::tags$p("Tap using your keypad"),
                                    show_console_area = TRUE,
                                    label = "record_key_presses_page", ...) {

  ui <- shiny::tags$div(id = "mainDiv",
                        shiny::tags$div(class="fx",
                        shiny::tags$div(shiny::tags$div(id = "body", body),
                                        psychTestR::trigger_button("next", psychTestR::i18n("Next")),
                                        console_area(show_console_area))),
                        shiny::includeScript(system.file("www/js/getButtonPresses.js", package = 'musicassessr'))
    )

    psychTestR::page(label = label, ui = ui, get_answer = get_answer_key_presses_page, save_answer = TRUE)
}


console_area <- function(show) {
  if(show) {
    shiny::tags$div(
      shiny::tags$div(shiny::tags$br(),
                      shiny::tags$textarea(rows="5", name="test-target", id="test-target"),
                      shiny::tags$button('clear console', type="button", name="btn-clear-console", id="btn-clear-console")
      ),
      shiny::tags$div(class="flex",
               shiny::tags$pre(id="console-log"))
    )
  }
  else {
    shiny::tags$div()
  }
}

