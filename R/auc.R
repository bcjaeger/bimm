

# By Miron Kursa https://mbq.me
auc_compute <- function(pred, resp) {
  n1 <- sum(resp == 0)
  n2 <- sum(resp == 1)
  U  <- sum(rank(pred)[resp == 0]) - n1 * (n1 + 1) / 2
  1 - U / (n1 * n2)
}
