#' Convert input to a draws array
#'
#' Converts an input object to the three-dimensional draws array format used by
#' the package. If 'x' is already a three-dimensional array, it is used directly.
#' If 'x' is an rstan 'stanfit' object, posterior draws are extracted with chains
#' preserved.
#'
#' @param x A three-dimensional array of posterior draws, or an rstan 'stanfit
#' object
#' @param parameter Optional character vector of parameter names to extract when
#' 'x' is a 'stanfit' object. If 'Null', all parameters are extracted.
#'
#' @returns A checked three-dimensional array of posterior draws with dimensions
#' iteration by chains by parameters.
#'
#' @keywords internal

as_draws_array <- function(x, parameter = NULL) {
  if (is.array(x) && length(dim(x)) == 3) {
    draws <- x
  } else if (inherits(x, "stanfit")) {
    draws <- rstan::extract(x, pars = parameter, permuted = FALSE)
  } else {
    stop(
      "'x' must be either a 3-dimensional draws array or an rstan 'fit' object.",
      call. = FALSE
    )
  }
  check_draws_array(draws)
  draws
}
