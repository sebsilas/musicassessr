

recorded_words_plot <- function(words, onsets) {

  onsets <- 1:length(words) # hack for now, som reason onsets is too long..

  y <- rep(c(2, 3, 4, 3), length(onsets))
  y <- y[1:length(onsets)]


  tb <- tibble::tibble(x = onsets,
                       y = y,
                       word = words)

  ggplot2::ggplot(tb, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(label = tb$word) +
    ggplot2::ylim(c(1,5))

}

feedback_recorded_words_plot_page <- function() {
  psychTestR::reactive_page(function(answer, state, ...) {

    words_plot <- shiny::tags$div(
      shiny::renderPlot({
      recorded_words_plot(strsplit(rjson::fromJSON(answer$user_response_words), split = " ")[[1]], as.numeric(rjson::fromJSON(answer$onsets)))
    }, width = 500)
    )

    psychTestR::one_button_page(words_plot)
  })
}
