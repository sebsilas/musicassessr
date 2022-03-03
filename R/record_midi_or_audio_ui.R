
# common ui template for midi and audio pages
record_midi_or_audio_ui <- function(body = " ",
                                    label = "record_audio_page",
                                    stimuli = " ",
                                    stimuli_reactive = FALSE,
                                    page_text = " ",
                                    page_title = " ",
                                    page_type = " ",
                              interactive = FALSE,
                              show_record_button = TRUE,
                              get_answer,
                              answer_meta_data = tibble::tibble(),
                              show_aws_controls = FALSE,
                              button_text = "Record",
                              stop_button_text = "Stop",
                              record_duration = NULL,
                              on_complete = NULL,
                              auto_next_page = FALSE,
                              save_answer = TRUE,
                              page_text_first = TRUE,
                              happy_with_response =  FALSE,
                              attempts_left = integer(),
                              max_goes_forced = FALSE,
                              autoInstantiate = FALSE,
                              midi_device = " ",
                              p_id = " ", ...) {


  interactive <- ifelse(interactive, "true", "false")

  psychTestR::page(ui = shiny::tags$div(

    shiny::tags$head(

      shiny::tags$script(paste0('console.log(\"this is a ', page_type, '\");')),
      if(page_type == "record_midi_page") autoInstantiateMidi(instantiate = autoInstantiate, midi_device, interactive),

      if(page_type == "record_audio_page") send_page_label_and_p_id_to_js(label, p_id),

      auto_next_page(auto_next_page),

      shiny::tags$script(set_answer_meta_data(answer_meta_data))

    ),
    shiny::tags$body(

      shiny::tags$h2(page_title),
      if(page_text_first) shiny::tags$p(page_text),

      shiny::tags$div(body),
      reactive_stimuli(stimuli_function = stimuli_function,
                       stimuli_reactive = stimuli_reactive,
                       prepared_stimuli = abs_mel),

      present_record_button(present = show_record_button, type = page_type,
                            button_text = button_text, record_duration = record_duration,
                            stop_button_text = stop_button_text),

      if(page_type == "record_audio_page") loading(),

      happy_with_response_message(happy_with_response, attempts_left, max_goes_forced),
      if(page_type == "record_audio_page") deploy_aws_pyin(show_aws_controls = show_aws_controls, stop_button_text),

      if(!page_text_first) shiny::tags$p(page_text)
    )
  ),
  label = label,
  get_answer = get_answer,
  save_answer = save_answer,
  on_complete = on_complete
  )

}


send_page_label_and_p_id_to_js <- function(label, p_id) {
  shiny::tags$script(paste0('var page_label = \"', label, '\"; ',
                            'var p_id = \"', p_id, '\";'))
}

auto_next_page <- function(auto_next_page) {
  if(auto_next_page) {
    shiny::tags$script('var auto_next_page = true;')
  } else {
    shiny::tags$script('var auto_next_page = false;')
  }
}


loading <- function() {
  htmltools::HTML('
  <div class="hollow-dots-spinner" :style="spinnerStyle;display:none;">
    <div class="dot"></div>
      <div class="dot"></div>
        <div class="dot"></div>
          </div>
    <div id="loading" style="display: none;"></div>
    ')
}


return_correct_attempts_left <- function(attempts_left, max_goes_forced = FALSE) {

  if(max_goes_forced) {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1.forced")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2.forced")
  } else {
    attempts_remaining_1 <- psychTestR::i18n("attempts_remaining_1")
    attempts_remaining_several.2 <- psychTestR::i18n("attempts_remaining_several.2")
  }

  if(attempts_left == 0L) {
    #label = paste0(var_name,"_attempt_", number_attempts, "_choice"),
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("attempts_remaining_0")),
                    shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else if (attempts_left == 1L) {
    # label = paste0(var_name,"_attempt_", number_attempts, "_choice")
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("happy_with_response_message")), shiny::tags$p(attempts_remaining_1),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = psychTestR::i18n("Try_Again"), label = psychTestR::i18n("Try_Again"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  } else {
    # label = paste0(var_name,"_attempt_", number_attempts, "_choice")
    shiny::tags$div(id = "happy_with_response", style = "display:none;",
                    shiny::tags$p(psychTestR::i18n("happy_with_response_message")),
                    shiny::tags$p(paste0(psychTestR::i18n("attempts_remaining_several.1"), " ", attempts_left, " ", attempts_remaining_several.2)),
                    shiny::tags$button(psychTestR::i18n("Try_Again"), id = psychTestR::i18n("Try_Again"), label = psychTestR::i18n("Try_Again"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button"),
                    if(!max_goes_forced) shiny::tags$button(psychTestR::i18n("Continue"), id = psychTestR::i18n("Continue"), label = psychTestR::i18n("Continue"), onclick = "hide_happy_with_response_message();Shiny.setInputValue('user_satisfied', this.id); next_page();", class="btn btn-default action-button")
    )
  }
}



happy_with_response_message <- function(happy_with_response_message, attempts_left, max_goes_forced = FALSE) {
  if(happy_with_response_message) {
    shiny::tags$div(
      return_correct_attempts_left(attempts_left, max_goes_forced),
      shiny::tags$script('var show_happy_with_response = true;')
    )
  } else {
    shiny::tags$div(
      shiny::tags$script('var show_happy_with_response = false;')
    )
  }
}

