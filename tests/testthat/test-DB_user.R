test_that("validate_user_entry_into_test works", {

  tl <- psychTestR::join(psychTestR::one_button_page("Hi"))

  expect_is(validate_user_entry_into_test(validate_user_entry_into_test = TRUE, elts = tl),
            "list")
})
