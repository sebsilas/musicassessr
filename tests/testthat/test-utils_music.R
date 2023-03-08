
test_that("find.closest.value works", {
  expect_equal(find.closest.value(14, c(1, 6, 12, 28, 33), TRUE), 12)
})

test_that("get.all.octaves.in.gamut works", {
  expect_equal(find.closest.value(14, c(1, 6, 12, 28, 33), TRUE), 12)
})


test_that("find.closest.value works", {

  as <- get.all.octaves.in.gamut(41, midi.gamut.min, midi.gamut.max)

  expect_setequal(as, c(17, 29, 41, 53, 65, 77, 89, 101, 113))
})


test_that("get_instrument_range works", {

  correct <- tibble::tibble(low_note = 49, high_note = 81)
  expect_true(all.equal(correct, get_instrument_range("Alto Saxophone")))

})

