test_that("dict works", {

  df <- data.frame(key = c("name", "hello", "bye"),
                   en = c("Matthew", "Hello", "Goodbye"),
                   de = c("Matthias", "Hallo", "Tschüss"))

  test_dict <- musicassessr::dict(additional_dict = df)

  expect_is(dict_test, "i18n_dict")

})



