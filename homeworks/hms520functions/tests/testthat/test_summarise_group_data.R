test_that("summarise_group_data input function works as expected", {
  expect_equal(as.numeric(summarise_group_data(mtcars,
                                    group_id = NULL,
                                    obs = "mpg",
                                    fun = mean)),
               mean(mtcars$mpg))

  expect_equal(as.numeric(summarise_group_data(mtcars,
                                               group_id = NULL,
                                               obs = "mpg",
                                               fun = max)),
               max(mtcars$mpg))

  expect_equal(as.numeric(summarise_group_data(mtcars,
                                               group_id = NULL,
                                               obs = "mpg",
                                               fun = sd)),
               sd(mtcars$mpg))
})

test_that("summarise_group_data group_by works when given 1 column", {
  expect_equal(summarise_group_data(mtcars, "gear", "mpg", mean),
               mtcars %>%
                    group_by(gear) %>%
                    summarise(mpg = mean(mpg)))
})

test_that("summarise_group_data group_by works when given 2 columns", {
  expect_equal(summarise_group_data(mtcars,
                                      group_id = c("gear", "am"),
                                      obs = "mpg",
                                      fun = mean),
               mtcars %>%
                 group_by(gear, am) %>%
                 summarise(mpg = mean(mpg)))
})

test_that("summarise_group_data and summari(z)e_group_data work identically", {
  expect_equal(summarise_group_data(mtcars, "gear", "mpg", mean),
               summarize_group_data(mtcars, "gear", "mpg", mean))
})

test_that("Error emerges when wrong column names are given", {
  expect_error(summarise_group_data(mtcars, "gear", obs = c("wrong", "disp"), fun = mean))
  expect_error(summarise_group_data(mtcars, "wrong", obs = c("mpg", "disp"), fun = mean))
})
