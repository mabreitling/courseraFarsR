library(courseraFarsR)

test_that("make_filename produces correct character", {
  expect_is(make_filename(2013), "character")
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")
})

# test if data is read and is of class "data.frame"
test_that("check reading raw data and transforming to tibble", {
  acc_tibble <- courseraFarsR::fars_read(make_filename(2013))
  expect_is(acc_tibble, 'data.frame')
})






