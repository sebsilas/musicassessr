
# constants
soprano <- 60:84
alto <- 53:77
tenor <- 48:72
baritone <- 45:69
bass <- 40:64

ranges <- list("Soprano" = soprano,
               "Alto" = alto, "Tenor" = tenor,
               "Baritone" = baritone,
               "Bass" = bass)


user_range_width <- 3


# list of page types that require a user-specific starting pitch

user.starting.range.pages <- list("play_long_tone_record_audio_page", "play_interval_record_audio_page", "play_midi_file_record_audio_page", "play_melody_from_list_record_audio_page", "play_melody_record_audio_page")


# core functions

create.p.id <- function(state, ...) {
  p_id <- sprintf("p_%s",paste(base::sample(1:9, 26, replace = TRUE), collapse = ''))
  set_global("p_id", p_id, state)
  cat("p_id is", p_id, sep = "\n")
}

get.p.id <- function(state, ...) {
  p_id <- get_global("p_id", state)
  return(p_id)
}

compute.SNR <- function(signal, noise) {
  # nice interpretation: https://reviseomatic.org/help/e-misc/Decibels.php
  signal <- env(signal, f = 44100)
  noise <- env(noise, f = 44100)
  SNR <- 20*log10(abs(rms(signal)-rms(noise))/rms(noise))
  return(SNR)
}


get_user_session_dir <- function (state) {

  session_dir <- paste0("output/sessions/",get_session_info(state, complete = FALSE)$p_id,"/")

  return (session_dir)

}

need.headphones <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res == "Yes, I am wearing headphones.") TRUE
  else display_error("Sorry, you cannot complete the test unless you are using headphones.")
}

need.quiet <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res == "Yes") TRUE
  else display_error("Sorry, you cannot complete the test unless you are in a quiet environment.")
}

need.consent <- function(answer, ...) {
  res <- suppressWarnings(answer)
  if (!is.na(res) && res == "Yes") TRUE
  else display_error("Sorry, you cannot complete the test unless you give consent to this.")
}

user_info_check <- function(input, state, ...)  {

  # check the info and save it including participant ID
  if (input$browser_capable == "FALSE") {
    display_error("Sorry, your browser does not have the have requirements to complete the test. Please download the latest version of Google Chrome to complete the experiment.")
  }

  else {
    list("user_info" = fromJSON(input$user_info),
         "p_id" = get.p.id(state),
         "sound_out_id" = get_global("soundout_id", state)
    )
  }

}


button.text.to.choices <- function(button_text) {
  # where multiple choices are given in data file in the form "Yes/No",
  # separate into a character vector of the choiecs

  choices <- unlist(strsplit(button_text, "/"))

  return(choices)
}



rel.to.abs.mel <- function(start_note, list_of_rel_notes) {
  # convert a relative representation of a melody to an absolute one, given a starting note
  new.mel <- cumsum(c(start_note, as.numeric(unlist(list_of_rel_notes))))
  return(new.mel)
}


# range functions

get.vocal.range <- function(string_of_range) {

  return(ranges[[string_of_range]])
}


generate.user.range <- function(note) {
  # given a starting note, create a range for the user to present stimuli in
  range <- c(-user_range_width:user_range_width) + note
  return(range)
}



tidy.melody.from.corpus <- function(mel) {
  mel <- as.numeric(unlist(strsplit(mel, ",")))
}

mean.of.stimuli <- function(rel_melody) {
  res <- round(mean(rel.to.abs.mel(0, rel_melody)))
  res
}

random.melody.from.corpus <- function() {
  mel <- tidy.melody.from.corpus(berkowitz.item.bank[sample(1:nrow(berkowitz.item.bank), 1), 1])
  mel
}


rel.to.abs.mel.mean.centred <- function(rel_melody, user_mean_note, range = NULL) {
  # produce a melody which is centered on the user's range.
  # NB: the "mean stimuli note" could/should be sampled from around the user's mean range i.e +/- 3 semitones

  mean_of_stimuli <- mean.of.stimuli(rel_melody)

  min.range <- range[1]
  max.range <- range[length(range)]

  user_mean_corrected_to_stimuli <- user_mean_note - mean_of_stimuli
  stimuli_centred_to_user_mean <- rel.to.abs.mel(user_mean_corrected_to_stimuli, rel_melody)


  # the rel melody should be the same when converted back
  #print(diff(stimuli_centred_to_user_mean))
  #print(rel_melody)


  # data <- data.frame("x"=1:length(stimuli_centred_to_user_mean), "y"=stimuli_centred_to_user_mean)
  #
  # # Plot
  # print(plot_gg <- data %>%
  #   ggplot( aes(x=x, y=y)) +
  #   geom_line() +
  #   geom_point() +
  #   geom_hline(yintercept = user_mean_note, color = "blue") +
  #   geom_hline(yintercept = user_mean_corrected_to_stimuli, color = "red", linetype="dotted") +
  #   geom_hline(yintercept = min.range, color = "green") +
  #   geom_hline(yintercept = max.range, color = "green"))
  #
  return(stimuli_centred_to_user_mean)

}


# functions for testing

rangeTest <- function() {

  # get random range
  vocal_range <- sample(ranges, 1)[[1]]
  print(vocal_range)
  # get random melody
  rel_melody <- random.melody.from.corpus()

  sampled_mean_note <- mean(vocal_range) + sample(-3:3, 1)

  centred.mel <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note, vocal_range)
  print(centred.mel)
  res <- centred.mel[centred.mel %in% intersect(centred.mel,  vocal_range)] # NB this is necessary because of duplicates
  print(res)
  no.present <- length(res)

  print_res <- paste0(no.present, " out of ", length(centred.mel), " present in range")
  print(print_res)
}



# results gathering

get.timecode <- function(input, state, getStimuli, getRhythms, ...) {

  # if getStimuliData == TRUE, get stimuli data
  # if getRhythms == TRUE, also get rhythm data

  page_answer <- list(trial.timecode = input$timecode)

  if (getStimuli == TRUE) {
    page_answer$playback.count <- input$playback_count
    page_answer$stimuli.pitch <- input$stimuli_pitch
    page_answer$playback.times <- input$playback_times
  }

  if (getRhythms == TRUE) {
    page_answer$stimuli.ticks <- fromJSON(input$stimuli_ticks)
    page_answer$stimuli.duration <- fromJSON(input$stimuli_duration)
    page_answer$stimuli.durationTicks <- fromJSON(input$stimuli_durationTicks)
  }

  return(page_answer)


}




save.range <- function(answer, state, ...) {
  set_global("user_range",answer,state)
}

get.range <- function(state, ...) {
  range <- get_global("user_range",state)
  range
}

random.note.from.user.range <- function(pageb, sample_mean_note) {

  # prepend the page with a codeblock which does the dirty work

  cb <- code_block(function(state, answer, ...) {
    # a page wrapper for generating the stimuli from a random starting note (within the participants calculated range)

    # retrieve user range
    saved_user_range <- get_global("user_range", state)   # user_range: a range of absolute "starting" midi values

    # https://www.musicnotes.com/now/tips/determine-vocal-range/

    if (saved_user_range == "Soprano") {
      print("Soprano")
      lowest_freq <- 60 + vocal.range.factor
      highest_freq <- 84 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }

    else if (saved_user_range == "Alto") {
      print("Alto")
      lowest_freq <- 53 + vocal.range.factor
      highest_freq <- 77 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }

    else if (saved_user_range == "Tenor") {
      print("Tenor")
      lowest_freq <- 48 + vocal.range.factor
      highest_freq <- 72 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }

    else if (saved_user_range == "Baritone") {
      print("Baritone")
      lowest_freq <- 45 + vocal.range.factor
      highest_freq <- 69 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }

    else {
      print("Bass")
      lowest_freq <- 40 + vocal.range.factor
      highest_freq <- 64 - vocal.range.factor
      range <- lowest_freq:highest_freq
    }

    if (sample_mean_note == TRUE) {
      sampled_mean_note <- mean(range) + sample(-3:3, 1)
      set_global("sampled_mean_note",sampled_mean_note, state)
    }

    else {
      sampled_note <- sample(range, 1)
      set_global("sampled_note",sampled_note, state)
    }

  }) # end code block

  # then wrap in a reactive page
  page <- reactive_page(pageb) # end reactive page

  both <- list(cb,page)

  return(both)


} # end main function





#### main test pages ####



play_long_tone_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
                                             save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
                                             note_no = "max", interval = NULL, sampled_note = NULL, p_id, ...) {

  # a page type for playing a 5-second tone and recording a user singing with it

  # The arguments must be in this order:
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
  # note_no = "max", interval = NULL, sampled_note, ...


  # user_range_index: which index of the user's stored range should be used for the long tone


  tone.for.js <- sampled_note

  # listen for clicks from play button then play


  ui <- div(

    html.head,

    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##

    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),

    # start body

    body,

    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("console.log(this.id);playTone(%s, 5, this.id, 'tone');", tone.for.js))
    ),

    shiny::tags$div(id="loading_area"),

    html.footer2

  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })

}







play_interval_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
                                            save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
                                            note_no = 2, sampled_mean_note = NULL, p_id, ...) {

  # The arguments must be in this order: label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
  #save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
  # note_no = "max", interval = NULL, sampled_mean_note

  # a page type for playing a single interval, recording user audio response and saving as a file

  interval <- rel.to.abs.mel.mean.centred(simple_intervals[stimuli_no], sampled_mean_note)

  interval.for.js <- toString(interval)

  # listen for clicks from play button then play


  ui <- div(

    html.head,

    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##

    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),

    # start body

    body,

    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id, 'piano');", interval.for.js))
    ),

    shiny::tags$div(id="loading_area"),

    html.footer2
  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })

}



# create a page type for playing back midi files

play_midi_file_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
                                             save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
                                             note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, ...) {

  # The first arguments must be in this order:
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
  # note_no = "max", interval = NULL, sampled_mean_note, ...

  # note_no. optionally limit number of notes

  # work out the difference between the first note of the stimuli and the user starting note to calculate no. of semitones to transpose

  melody <- rel.to.abs.mel.mean.centred(stimuli[stimuli_no], sampled_mean_note)

  transpose <- diff(c(melody[1], stimuli.abs[[stimuli_no]][1]))

  #cat("transpose by: ", -(transpose), sep="\n")

  if (is.null(note_no) == TRUE) {
    note_no <- "\"max\""
  }

  dir_of_midi <- "berkowitz_midi_rhythmic/"

  url <- paste0(dir_of_midi,"Berkowitz",stimuli_no,".mid")

  ui <- div(

    html.head,

    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##

    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),

    # start body
    body,

    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton",
                                       onclick=shiny::HTML(paste0("playMidiFileAndRecordAfter(\"",url,"\", true, ",note_no,", true, this.id, ",transpose,", 'piano')")))
    ),

    shiny::tags$div(id="loading_area"),
    html.footer2
  )

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  TRUE) })
}




play_melody_from_list_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
                                                    save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
                                                    note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, ...) {

  # The arguments must be in this order:
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
  # note_no = "max", interval = NULL, sampled_mean_note, ...

  # a page type for playing a melody, recording user audio response and saving as a file

  if (note_no == "max") {
    note_no <- length(stimuli[[stimuli_no]])
  }

  rel_melody <- stimuli[[stimuli_no]][0:note_no]

  melody <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note)

  mel.for.js <- toString(melody)

  # listen for clicks from play button then play


  ui <- div(

    html.head,

    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##

    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),

    # start body

    body,

    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], true, this.id, 'piano');", mel.for.js))
    ),

    shiny::tags$div(id="loading_area"),

    html.footer2


  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  TRUE) })

}



play_melody_record_audio_page <- function(label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
                                          save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no = NULL,
                                          note_no = "max", interval = NULL, sampled_mean_note = NULL, p_id, rel_melody = NULL, user_range = NULL, ...) {

  # The arguments must be in this order:
  # label= NULL, body = NULL, on_complete = NULL, admin_ui = NULL,
  # save_answer = TRUE, button_text = "Next", stimuli_corpus = NULL, stimuli_no,
  # note_no = "max", interval = NULL, sampled_mean_note, ...

  # a page type for playing a melody, recording user audio response and saving as a file

  cat("sampled_mean_note in play_melody_record_audio_page", sampled_mean_note)


  melody <- rel.to.abs.mel.mean.centred(rel_melody, sampled_mean_note, get.vocal.range(user_range))
  cat("melody in play_melody_record_audio_page", melody)

  mel.for.js <- toString(melody)

  # listen for clicks from play button then play


  ui <- div(

    html.head,

    ## instantiate empty variables to be updated by JS
    timecode <- NULL,
    playback_count <- NULL,
    stimuli_pitch <- NULL,
    stimuli_ticks <- NULL,
    stimuli_duration <- NULL,
    stimuli_durationTicks <- NULL,
    ##

    # set participant id
    shiny::tags$script(sprintf('var p_id = \"%s\";console.log(p_id);', p_id)),

    # start body

    body,

    shiny::tags$div(id="button_area",
                    shiny::tags$button(button_text, id="playButton", onclick=sprintf("playSeq([%s], false, this.id, 'piano');", mel.for.js))
    ),

    shiny::tags$div(id="loading_area"),

    html.footer2


  ) # end main div

  psychTestR::page(ui = ui, admin_ui = admin_ui, on_complete = on_complete, label = label, save_answer = TRUE, get_answer = function(input, state, ...) { get.timecode(input, state, getStimuli = TRUE, getRhythms =  FALSE) })

}

