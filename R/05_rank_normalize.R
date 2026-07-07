#' Rank-normalize numeric draws
#'
#' Converts a numeric vector of draws into rank-normalized values. The raw values
#' are first replaced by their ranks, and the ranks are transformed to approximate
#' standard normal scores.
#'
#' @param x A numeric vector of draws
#'
#' @return A numeric vector of rank-normalized values with the same length as 'x'
#'
#' @keywords internal

rank_normalize <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  if (length(x) < 2) {
    stop("`x` must contain at least 2 draws.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("`x` must not contain missing values.", call. = FALSE)
  }
  n <- length(x)
  ranks <- rank(x, ties.method = "average")
  stats::qnorm(
    (ranks - 3 / 8) / (n + 1 / 4)
  )
}
