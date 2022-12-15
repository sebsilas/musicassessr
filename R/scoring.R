


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
    # itembankr::ngrukkon()
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

# tests
# check_answer("a", "b", type = "single_letters")
# check_answer("a", "a", type = "single_letters")
# check_answer("aaa", "bbb", reverse = FALSE, type = "chunk_letters")
# check_answer("aaa", "aaa", type = "chunk_letters")
# check_answer(11, 22, type = "single_numbers")
# check_answer(11, 11, type = "single_numbers")
# check_answer(11, 11, reverse = TRUE,  type = "chunk_digits")
# check_answer(123, 123, reverse = TRUE, type = "chunk_digits")
# check_answer(123, 321, reverse = TRUE, type = "chunk_digits")
#
# check_answer(1:3, 3:1, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 3:1, reverse = FALSE, type = "chunk_digits")
# check_answer(1:3, 1:3, reverse = TRUE, type = "chunk_digits")
# check_answer(1:3, 1:4, reverse = TRUE, type = "chunk_digits")
#
# check_answer(c(10,50,60), c(10,50,60), type = "midi_pitches")
# check_answer(c(10,50,60), c(10,50,60), reverse = TRUE, type = "midi_pitches")
# check_answer(c(10,50,60), c(60,50,10), reverse = TRUE, type = "midi_pitches")
#


# t <- itembankr::midi_file_to_notes_and_durations('/Users/sebsilas/true.mid', string_df = FALSE)
# t2 <- itembankr::midi_file_to_notes_and_durations('/Users/sebsilas/true.mid', string_df = TRUE, itembankr::produce_extra_melodic_features = TRUE)
# t3 <- t2 %>% to_string_df()


