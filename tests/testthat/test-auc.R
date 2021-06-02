

test_that("auc_compute gives the right answer", {

  pred <- c(0.1, 0.2, 0.2, 0.3, 0.5, 0.6)
  resp <- c(0, 0, 0, 1, 1, 1)

  # perfect prediction
  expect_equal(
    auc_compute(pred, resp),
    1
  )

  # one tie
  pred[3] <- 0.3

  expect_equal(
    round(auc_compute(pred, resp), 3),
    0.944
  )

  # one mis-match
  pred[3] <- 0.4

  expect_equal(
    round(auc_compute(pred, resp), 3),
    0.889
  )

})
