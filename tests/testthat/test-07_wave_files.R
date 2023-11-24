test_that("get_wav_length", {

  f <- tempfile()
  w <- tuneR::sine(440, duration = 100000)
  tuneR::writeWave(w, f)

  expect_silent(x <- get_wav_length(f))
  expect_equal(x, "2.27 seconds")
  expect_silent(x <- get_wav_length(f, return_numeric = TRUE))
  expect_equal(x, 2.27)
  unlink(f)
})

