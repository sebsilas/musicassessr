



db_append_tests <- function(test_name, test_description) {
  stopifnot(
    is.scalar.character(test_name),
    is.scalar.character(test_description)
  )

  db_con <- connect_to_db()

  test_names <- get_table(db_con, 'tests') %>%
    dplyr::pull(test_name)

  if(test_name %in% test_names) {
    stop("Already a test with this name")

  } else {
    tests_df <- tibble::tibble(test_name = test_name, test_description = test_description)
    id <- db_append_to_table(db_con, table = "tests", data = tests_df)

  }
  DBI::dbDisconnect(db_con)
  return(id)
}


create_musicassessr_tests <- function() {
  db_append_tests("SAA", "Singing Ability Assessment")
  db_append_tests("PBET", "Playing By Ear Test")
}

create_users <- function() {

  # test's user ID is 1
  db_append_users(db_con, 'test', Sys.getenv("USER_PW"))
  db_append_users(db_con, 'Seb', Sys.getenv("USER_PW"))

  # Seb's voice range
  db_append_user_instrument_info(db_con,
                                 user_id = 2L, # Seb's User ID is 2
                                 instrument_id = 1L,
                                 bottom_range = 43L,
                                 top_range = 69L)

}


