# convert input x into draws array
# if x is already an array, use it directly, if not, convert it to an array
# inherits is a class check function

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
