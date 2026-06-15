#converts raw values into rank-normalized values
rank_normalize <-function(x){
  n <- length(x)
  ranks <- rank(x, ties.method = "average")
  stats::qnorm((ranks - 3 / 8) / (n + 1 / 4))
}
