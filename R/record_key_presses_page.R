

record_key_presses_page <- function(body, ...) {
    psychTestR::page(
    label = "test",
    ui = tags$div(id = "mainDiv", tags$div(class="fx",
                           div(
                             div(id = "body", body),
                             trigger_button("next", "Next"),
                             #br(),
                             #tags$textarea(rows="5", name="test-target", id="test-target"),
                             #tags$button('clear console', type="button", name="btn-clear-console", id="btn-clear-console")
                           #),
                           #tags$div(class="flex",
                           #         tags$pre(id="console-log")
                           )
    ),
    includeScript(path="www/js/getButtonPresses.js")
    ),
    get_answer = function(input, ...) {

      print('get_answer_buttonpresses. input: ')
      print(input)
      lapply(input, print)

      list(onsets_keydown = fromJSON(input$onsets_keydown))

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
