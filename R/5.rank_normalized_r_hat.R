rank_normalized_r_hat <-function(x){
  n <- length(x)
  ranks <- rank(x)
  ranks <- rank(x, ties.method = "average")
  stats::qnorm((ranks - 3 / 8) / (n + 1 / 4))
}
