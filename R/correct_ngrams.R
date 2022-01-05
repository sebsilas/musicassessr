#
#
# stim <- c(60, 62, 64, 65, 67)
# stim_ngs <- itembankr::get_ngrams_multiple_sizes(stim, length(stim))$value
#
#
# user_prod <- c(65,67, 60, 62)
# user_prod_ngs <- itembankr::get_ngrams_multiple_sizes(user_prod, length(user_prod))$value
#
#
# correct_ngrams <- intersect(stim_ngs, user_prod_ngs)
#
# missed_ngrams <- setdiff(stim_ngs, user_prod_ngs)
#
# psychTestR::code_block(function(state, answer, ...) {
#   missed_ngrams <- psychTestR::answer(state)$missed_ngrams %>% arrange(difficulty)
#   revise <- psychTestR::get_global("revise", state)
#   updated_revise <- rbind(revise, missed_ngrams)
#   psychTestR::set_global("revise", update_revised, state)
#
# })
