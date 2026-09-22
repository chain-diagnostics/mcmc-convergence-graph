#' Split a chain into two halves
#'
#' Split a numeric vector of MCMC draws into two equal halves. If the number of
#' draws is odd, the middle draw is discarded so that both halves have the same
#' length, matching Stan's split R-hat.
#'
#' @param x A numberical vector of draws from one chain for one parameter.
#'
#' @return A list with two numberic vectors: 'first', the first half of the chain,
#' and the 'second', the second half of the chain.
#'
#' @keywords internal

split_chain <- function(x) {
  n <- length(x)
  half <- n / 2
  list(
    first = x[seq_len(floor(half))],
    second = x[ceiling(half + 1):n]
  )
}
