test_that("create_formula creates formula object (which is type language)", {
  expect_equal(is.language(create_formula("y", "x")), TRUE)
})

test_that("formula ends in '+ 0' if intercept == FALSE", {
  expect_equal(create_formula("y", "x", intercept =  FALSE), as.formula("y ~ x + 0"))
  expect_equal(create_formula("y", c("x1", "x2"), intercept =  FALSE), as.formula("y ~ x1 + x2 + 0"))
})

test_that("formula is correct when intercept == TRUE", {
  expect_equal(create_formula("y", "x"), as.formula("y ~ x"))
  expect_equal(create_formula("y", c("x1", "x2")), as.formula("y ~ x1 + x2"))
})
