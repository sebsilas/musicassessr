
play_words <- function(words_vector, voice) {
  words_string <- paste0(words_vector, collapse = ' ')
  js.function <- paste0('var words = "',words_string, '\"; ',
                        'var voice = "', voice, '\"; ', collapse = "")
  tags$script(js.function)
}

select_voice_page <- function() {
  # set the selected voice
  page(
    label = "select_voice",
    ui = div(
      tags$h2('Voice Selection Page'),
      tags$p('Listen to some voices using the dropdown below and choose your preference.'),
      tags$br(),
      htmltools::HTML('<select id = "selectVoice"></select>'),
      tags$br(),
      tags$br(),
      tags$button("Test Voice", id = "playButton"),
      tags$br(),
      tags$br(),
      trigger_button('next', "Next"),
      tags$script('var words = \"Here is a sample of the chosen voice.\"'),
      includeScript('www/js/speechSynthesis.js'),
    ),
    get_answer = function(input, state, ...) {
      set_global("selected_voice", input$selected_voice, state)
      print(input$selected_voice)
      list(selected_voice = input$selected_voice)
    },
    save_answer = TRUE
  )
}


play_text_page <- function(stimuli, underlying_page_type = "one_button_page", page_text = "Click to hear the stimuli", page_title = " ", ...) {
  # display the selected voice from the last page
  # page_type: select underlying page type

    reactive_page(function(state, ...) {

      page.fun <- get(underlying_page_type)

      selected.voice <- get_global("selected_voice", state)

      body_content <- div(
        htmltools::HTML('<select id = "selectVoice" hidden></select>'),
        play_words(stimuli, selected.voice),
        tags$h2(page_title),
        tags$p(page_text),
        tags$br(),
        tags$button("Play", id = "playButton"),
        tags$br(),
        tags$br(),
        includeScript('www/js/speechSynthesis.js')
      )

      args <- list("body" = body_content)

      # set the page up with additional arguments
      page <- do.call(what = page.fun, args)

      page
  })
}


record_spoken_words_page <- function() {

  # set the selected voice
  page(
    label = "test",
    ui = div(
      tags$p(tags$em("...diagnostic messages"), class = "output"),
      tags$button("Record", onclick="recognition.start();"),
      trigger_button("next", "Next"),
      includeScript('R/js/captureSpeech.js')
    ),
    get_answer = function(input, ...) {
      list(confidence = input$confidence,
           user_response = input$user_response)

    },
    save_answer = TRUE
  )
}

