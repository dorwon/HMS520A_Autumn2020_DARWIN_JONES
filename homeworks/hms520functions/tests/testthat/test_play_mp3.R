
test_that("play_mp3 can play all songs in the asset folder", {

  expect_silent(play_mp3("spookysong"))
  expect_silent(play_mp3("sadtrombone"))
  expect_silent(play_mp3("mars"))
})

test_that("play_mp3 will throw error if song is not present", {

  expect_error(play_mp3("rickroll"), "Error: Song not found in assets folder")
  expect_error(play_mp3("spongebob"), "Error: Song not found in assets folder")
  expect_error(play_mp3("haidongsays"), "Error: Song not found in assets folder")
})

test_that("play_mp3 only accepts character objects of length 1", {

  expect_error(play_mp3(12345), "Error, Enter Song name as string")
  expect_error(play_mp3(c("spookysong", "mars")), "Error, Enter Song name as string")
  expect_error(play_mp3(Sys.Date()), "Error, Enter Song name as string")
})
