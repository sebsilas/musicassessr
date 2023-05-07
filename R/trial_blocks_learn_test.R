
# Follow-up (test) trial block i.e., to redo trials previous done earlier in experiment


arrhythmic_melody_trials_test_block <- function() {


  # session_info <- psychTestR::get_session_info(state, complete = FALSE)
  # test_username <- psychTestR::get_global("test_username", state)
  # test <- psychTestR::get_local("test", state)
  #
  # current_session <- session_info$p_id # What if p_id is set custom?
  #current_session_trials <- get_session_trials(current_session)

  current_session_trials <- get_session_trials("d5d2d7ea59420f27e61ff2c5c817ccb040dda30638545ac5035b75b032ed648f") %>%
    itembankr::set_item_bank_class()

  musicassessr::arrhythmic_melody_trials(presampled = TRUE, item_bank = current_session_trials)
}




