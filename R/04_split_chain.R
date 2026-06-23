#' Split a chain into two halves
#'
#' Split a numeric vector of MCMC draws into two equal halves. If the number of
#' draws is odd, the final draw is discarded so that both halves have the same
#' length.
#'
#' @param x A numberical vector of draws from one chain for one parameter.
#'
#' @return A list with two numberic vectors: 'first', the first half of the chain,
#' and the 'second', the second half of the chain.
#'
#' @keywords internal

split_chain <- function(x) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.", call. = FALSE)
  }
  n <- length(x)
  if (n < 2) {
    stop("'x' must contain at least 2 draws.", call. = FALSE)
  }
  half_n <- floor(n / 2)
  list(
    first = x[seq_len(half_n)],
    second = x[(half_n + 1):(2 * half_n)]
  )
}
