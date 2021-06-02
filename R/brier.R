

brier_compute <- function(pred, resp) mean((resp-pred)^2)
