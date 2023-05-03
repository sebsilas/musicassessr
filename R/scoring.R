


reverse_answer <- function(ans) {
  # if reverse == TRUE, then check for the reverse of the answer
  if (length(ans) == 1) {
    ans <- stringi::stri_reverse(ans)
  } else {
    ans <- rev(ans)
  }
  ans
}

check_answer <- function(user_response, correct_answer,
                         reverse = FALSE,
                         type = c("single_digits", "numbers", "chunks_numbers",
                                  "single_letters", "chunk_letters",
                                  "words",
                                  "midi_pitches",
                                  "rhythms",
                                  "melodies_with_rhythms")
) {

  # check that the lengths are the same
  stopifnot(length(user_response) == length(correct_answer))

  # reverse answer if need be
  if (reverse == TRUE) {
    correct_answer <- reverse_answer(correct_answer)
  }
  # match based on type of input
  if (length(user_response) > 1) {
    correct <- identical(user_response, correct_answer)
  } else if (type == "midi_pitches") {
    # itembankr::ngrukkon_safe()
  } else if (type == "rhythms") {
    ## rhythm metric
    # itembankr::rhythfuzz()
  } else if (type == "melodies_with_rhythms") {

  }

  else {
    correct <- user_response == correct_answer
  }
  correct
}

check_opti3 <- function(user_response, correct_answer, reverse = FALSE) {
  # if reverse == TRUE, then check for the reverse of the answer

  if (reverse == TRUE) {
    #correct <-
  }
  else {
    #user_response ==
  }
}




