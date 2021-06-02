



test_that("brier_compute gives the right answer", {

  pred <- c(0.1, 0.2, 0.2, 0.3, 0.5, 0.6)
  resp <- c(0, 0, 0, 1, 1, 1)

  expect_equal(
    round(brier_compute(pred, resp), 3),
    0.165
  )

})
