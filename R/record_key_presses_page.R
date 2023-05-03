
console_area <- function(show) {
  if(show) {
    shiny::tags$div(
      shiny::tags$div(shiny::tags$br(),
        tags$textarea(rows="5", name="test-target", id="test-target"),
        tags$button('clear console', type="button", name="btn-clear-console", id="btn-clear-console")
        ),
        tags$div(class="flex",
                 tags$pre(id="console-log"))
    )
  }
  else {
    shiny::tags$div()
  }
}
record_key_presses_page <- function(body, show_console_area = FALSE, ...) {
    psychTestR::page(
    label = "test",
    ui = shiny::tags$div(id = "mainDiv", shiny::tags$div(class="fx",
                          shiny::tags$div(
                              shiny::tags$div(id = "body", body),
                             psychTestR::trigger_button("next", psychTestR::i18n("Next")),
                             console_area(show_console_area)
                           )
    ),
    shiny::includeScript(system.file("www/js/getButtonPresses.js"))
    ),
    get_answer = function(input, ...) {

      print('get_answer_buttonpresses. input: ')
      print(input)
      lapply(input, print)

      list(onsets_keydown = rjson::fromJSON(input$onsets_keydown))

      # list(user_response_keypress = fromJSON(input$user_response_keypress),
      #     onsets = fromJSON(input$onsets),
      #     onsets_keyup = fromJSON(input$onsets_keyup),
      #     user_response_keypress_keyup = fromJSON(input$user_response_keypress_keyup),
      #     onsets_keydown = fromJSON(input$onsets_keydown),
      #     user_response_keypress_keydown = fromJSON(input$user_response_keypress_keydown))

    },
    save_answer = TRUE
  )
}
