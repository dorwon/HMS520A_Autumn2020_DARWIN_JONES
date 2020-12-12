test_that("is_spooky_time recognizes Halloween", {

  expect_equal(is_spooky_time("2020-11-30"), FALSE)
  expect_equal(is_spooky_time("2020-10-31"), TRUE)
  expect_equal(is_spooky_time("1817-10-31"), TRUE)
  expect_equal(is_spooky_time("2020-10-30"), FALSE)
})

test_that("is_spooky_time recognizes Friday the 13th", {

  expect_equal(is_spooky_time("1964-3-13"), TRUE)
  expect_equal(is_spooky_time("2020-11-13"), TRUE)
  expect_equal(is_spooky_time("1801-02-13"), TRUE)

  expect_equal(is_spooky_time("1964-4-13"), FALSE)
  expect_equal(is_spooky_time("2020-11-12"), FALSE)
  expect_equal(is_spooky_time("1801-02-14"), FALSE)
})

test_that("is_spooky time recognizes dates, and char in format y/m/d, m/d/y", {

  expect_equal(is_spooky_time(as.Date("2020-11-13")), TRUE)
  expect_equal(is_spooky_time("11-13-2020"), TRUE)
  expect_equal(is_spooky_time("2020-11-13"), TRUE)
})

test_that("is_spooky_time recognizes char dates with - or /", {

  expect_equal(is_spooky_time("11-13-2020"), TRUE)
  expect_equal(is_spooky_time("11/13/2020"), TRUE)
})

test_that("is_spooky_time throws an error if input is incorrect type or format", {

  expect_error(is_spooky_time(11-13-2020), "Error: must supply date or character in date format")
  expect_error(is_spooky_time(c(2020, 11, 13)), "Error: must supply date or character in date format")
  expect_error(is_spooky_time("Friday the Thirteenth"), "character string is not in a standard unambiguous format")
  expect_error(is_spooky_time("13-2020-11"), "character string is not in a standard unambiguous format")
})
