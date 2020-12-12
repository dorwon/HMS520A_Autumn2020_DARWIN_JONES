test_that("valid_g_d_inputs is TRUE when group_id is NULL or char and FALSE otherwise", {

  expect_equal(valid_g_d_inputs(NULL, c("a","b","c")), TRUE)
  expect_equal(valid_g_d_inputs(c("z","x","y"), c("a","b","c")), TRUE)

  expect_equal(valid_g_d_inputs(NA, c("a","b","c")), FALSE)
  expect_equal(valid_g_d_inputs(12345, c("a","b","c")), FALSE)
})

test_that("valid_gd_inputs is FALSE when other_vars is not of type char", {
  expect_equal(valid_g_d_inputs(NULL, 1312), FALSE)
  expect_equal(valid_g_d_inputs(c("z","x","y"), data.frame(a = "c")), FALSE )
})
