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

test_that("clip_wave_single()", {

  # Create a dummy wave file for testing
  f <- temp_wavs(1)

  expect_silent(clip_wave_single(f, test_path("temp.wav"), clip_length = 1, start_time = 0.5))
  expect_error(clip_wave_single(f, test_path("temp.wav"), clip_length = 1, start_time = 0.5),
               "`overwrite` is FALSE")
  expect_equal(get_wav_length(test_path("temp.wav"), TRUE), 1)

  # Overwrite respected
  expect_silent(clip_wave_single(f, test_path("temp.wav"), clip_length = 0.5, start_time = 0.5,
                                 overwrite = TRUE))
  expect_equal(get_wav_length(test_path("temp.wav"), TRUE), 0.5)

  # No clipping if clip_length > wave_length
  # Trust wave_length (even if doesn't match)
  expect_silent(clip_wave_single(f, test_path("temp.wav"), clip_length = 1.5,
                                 wave_length = 1, overwrite = TRUE))
  expect_equal(get_wav_length(test_path("temp.wav"), TRUE),
               get_wav_length(f, TRUE))

  # Clean up
  unlink(f)
  unlink(test_path("temp.wav"))
})

test_that("clip_wave()", {

  # Create dummy wave files for testing
  w <- data.frame(path = temp_wavs(),
                  subdir_out = c("test1", "test2", "test1", "test2", "test1", "test2"),
                  subsubdir_out = c("a", "a", "b", "b", "c", "d"),
                  filename_out = paste0("wave", 1:6, ".wav"),
                  length = c(1, 1, 2, 1.5, 0.5, 0.75),
                  start_time = c(1.2, 0.5, 1, 0, 0.5,  0.3))

  clip_wave(w, dir_out = "clean", col_subdir_out = c("subdir_out", "subsubdir_out"))

    # Clean up
  unlink(temp_wavs())
  unlink("clean", recursive = TRUE) # Remove this new 'clean' directory
})
