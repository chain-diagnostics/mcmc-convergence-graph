#' Check a draws array
#'
#' Checks that `draws` is a three-dimensional array with dimensions
#' corresponding to iterations, chains, and parameters.
#'
#' @param draws A three-dimensional array of posterior draws.
#'
#' @return Invisibly returns `NULL`. Throws an error if `draws` is invalid.
#'
#' @keywords internal
check_draws_array <- function(draws) {
  if (!is.array(draws) || length(dim(draws)) != 3) {
    stop("`draws` must be a 3-dimensional array.", call. = FALSE)
  }

  if (dim(draws)[1] < 2) {
    stop("`draws` must contain at least 2 iterations.", call. = FALSE)
  }

  if (dim(draws)[2] < 2) {
    stop("`draws` must contain at least 2 chains.", call. = FALSE)
  }

  if (dim(draws)[3] < 1) {
    stop("`draws` must contain at least 1 parameter.", call. = FALSE)
  }

  invisible(NULL)
}
