


test_that("prob_split_at_cut works as intended", {

  cut_values <- c(0, 1/3, 2/3, 1/2, 1)

  expect_equal(
    prob_split_at_cut(cut_values, cut_point = 1/2),
    c(0, 0, 1, 1, 1)
  )

  expect_equal(
    prob_split_at_cut(cut_values, cut_point = 1/5) ,
    c(0, 1, 1, 1, 1)
  )

})


test_that("prob_split_at_cut works as intended with static probs", {

  cut_values <- c(0, 1/3, 2/3, 1/2, 1)

  expect_equal(
    prob_split_stochastic(x = cut_values,
                          cut_lower = 1/3,
                          cut_upper = 2/3,
                          probs = c(0, 0, 0, 0, 0)),
    c(0, 0, 1, 0, 1)
  )

  expect_equal(
    prob_split_stochastic(x = cut_values,
                          cut_lower = 1/3,
                          cut_upper = 2/3,
                          probs = c(1, 1, 1, 1, 1)),
    c(0, 1, 1, 1, 1)
  )


})


