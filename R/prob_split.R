


prob_split_at_cut <- function(x, cut_point){

  xx <- x
  xx[x < cut_point]  <- 0
  xx[x >= cut_point] <- 1
  xx

}

prob_split_stochastic <- function(x, cut_upper, cut_lower, probs){

  xx <- x
  xx[x < cut_lower]  <- 0
  xx[x >= cut_upper] <- 1

  middle_index <- (x >= cut_lower) & (x < cut_upper)

  xx[middle_index] <- stats::rbinom(n = base::sum(middle_index),
                                    size = 1,
                                    prob = probs[middle_index])

  xx


}
