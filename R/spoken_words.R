


#' A page for selecting a voice before a play words page
#'
#' @return
#' @export
#'
#' @examples
select_voice_page <- function() {
  # set the selected voice
  psychTestR::page(
    label = "select_voice",
    ui = shiny::tags$div(
      shiny::tags$h2('Voice Selection Page'),
      shiny::tags$p('Listen to some voices using the dropdown below and choose your preference.'),
      shiny::tags$br(),
      htmltools::HTML('<select id = "selectVoice"></select>'),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::tags$button("Test Voice", id = "playButton"),
      shiny::tags$br(),
      shiny::tags$br(),
      psychTestR::trigger_button('next', psychTestR::i18n("Next") ),
      shiny::tags$script('var words = \"Here is a sample of the chosen voice.\"'),
      shiny::includeScript(system.file("www/js/speechSynthesis.js")),
    ),
    get_answer = function(input, state, ...) {
      psychTestR::set_global("selected_voice", input$selected_voice, state)
      logging::loginfo("Selected voice: %s", input$selected_voice)
      list(selected_voice = input$selected_voice)
    },
    save_answer = TRUE
  )
}


play_text_page <- function(stimuli, underlying_page_type = "one_button_page", page_text = "Click to hear the stimuli", page_title = " ", ...) {
  # display the selected voice from the last page
  # page_type: select underlying page type

    psychTestR::reactive_page(function(state, ...) {

      page.fun <- get(underlying_page_type)

      selected.voice <- get_global("selected_voice", state)

      body_content <- shiny::tags$div(
        htmltools::HTML('<select id = "selectVoice" hidden></select>'),
        shiny::play_words(stimuli, selected.voice),
        shiny::tags$h2(page_title),
        shiny::tags$p(page_text),
        shiny::tags$br(),
        shiny::tags$button("Play", id = "playButton"),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::includeScript('www/js/speechSynthesis.js')
      )

      args <- list("body" = body_content)

      # set the page up with additional arguments
      page <- do.call(what = page.fun, args)

      page
  })
}


record_spoken_words_page <- function() {

  # set the selected voice
  psychTestR::page(
    label = "test",
    ui = shiny::tags$div(
      shiny::tags$p(shiny::tags$em("...diagnostic messages"), class = "output"),
      shiny::tags$button("Record", onclick="recognition.start();"),
      psychTestR::trigger_button("next", psychTestR::i18n("Next") ),
      shiny::includeScript(system.file("www/js/captureSpeech.js", package = "musicassessr"))
    ),
    get_answer = function(input, ...) {
      list(confidence = input$confidence,
           user_response_words = input$user_response_words,
           onsets = input$onsets)

    },
    save_answer = TRUE
  )
}


play_words <- function(words_vector, voice) {
  words_string <- paste0(words_vector, collapse = ' ')
  js.function <- paste0('var words = "',words_string, '\"; ',
                        'var voice = "', voice, '\"; ', collapse = "")
  shiny::tags$script(js.function)
}
