test_that("Playing does not affect global seed", {
  skip_if_not(interactive())
  expect_equal({set.seed(1234)
    runif(1)} , {set.seed(1234)
      try(play_random_track("path/to/file/"), silent = TRUE)
      runif(1)}
    )
})

test_that("Weird file path gives error",
          {
            skip_if_not(interactive())
            expect_error({
            play_random_track("I feel a bad path rising")
            })})
