soprano <- 60:84
alto <- 53:77
tenor <- 48:72
baritone <- 45:69
bass <- 40:64

vocal_ranges <- list("Soprano" = soprano,
                     "Alto" = alto,
                     "Tenor" = tenor,
                     "Baritone" = baritone,
                     "Bass" = bass)

mean_vocal_ranges <- lapply(vocal_ranges, mean)



#' A page to select a voice range
#'
#' @param with_examples
#'
#' @return
#' @export
#'
#' @examples
get_voice_range_page <- function(with_examples = TRUE) {
  # could/should remake in R functions

  if(with_examples) {
    psychTestR::NAFC_page(
      label = "voice_range",
      prompt = shiny::tags$div(
        shiny::tags$p("Listen to the audio examples of people singing. Select the example you think best matches your voice."),
        htmltools::HTML(
      '<p><strong>Soprano</strong></p>
        <audio controls>
        <source src="https://adaptiveeartraining.com/magmaGold/audio/voice_ranges/soprano_voice_no_intro.m4a" type="audio/mp4">
          Your browser does not support the audio element.
        </audio>


          <p><strong>Alto</strong></p>
          <audio controls>
          <source src="https://adaptiveeartraining.com/magmaGold/audio/voice_ranges/alto_voice_no_intro.m4a" type="audio/mp4">
            Your browser does not support the audio element.
          </audio>

            <p><strong>Tenor</strong></p>
            <audio controls>
            <source src="https://adaptiveeartraining.com/magmaGold/audio/voice_ranges/tenor_voice_no_intro.m4a" type="audio/mp4">
              Your browser does not support the audio element.
            </audio>

              <p><strong>Baritone</strong></p>
              <audio controls>
              <source src="https://adaptiveeartraining.com/magmaGold/audio/voice_ranges/baritone_voice_no_intro.m4a" type="audio/mp4">
                Your browser does not support the audio element.
              </audio>

                <p><strong>Bass</strong></p>
                <audio controls>
                <source src="https://adaptiveeartraining.com/magmaGold/audio/voice_ranges/bass_voice_no_intro.m4a" type="audio/mp4">
                  Your browser does not support the audio element.
                </audio>')),
      choices = names(vocal_ranges),
      arrange_vertically = FALSE,
      on_complete = function(state, answer, ...){
        psychTestR::set_global("range", answer, state)
      })
  } else {

    psychTestR::NAFC_page(
      label = "voice_range",
      prompt = shiny::tags$div(
        shiny::tags$p("If you know your voice type, please select one of the following, or click \"Not sure\"")),
      choices = c(names(vocal_ranges), "Not sure"),
      arrange_vertically = FALSE,
      on_complete = function(state, answer, ...){
        answer <- ifelse(answer == "Not sure", "Tenor", answer)
        psychTestR::set_global("range", answer, state)
      })

  }
}

