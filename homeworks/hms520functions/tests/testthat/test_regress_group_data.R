test_that("regress_group_data Intercept column has values if include_intercept is true and is NA if false", {

  expect_equal(is.na(regress_group_data(mtcars, "gear", "mpg", "disp")$`(Intercept)`[1]), FALSE)

  expect_equal(is.na(regress_group_data(mtcars, "gear", "mpg", "disp", include_intercept = FALSE)$`(Intercept)`[1]),
    TRUE)
})

test_that("regress_group_data produces tibble of correct size", {
  expect_equal(dim(regress_group_data(mtcars, "gear", "mpg", "disp")), c(3,3))
})
