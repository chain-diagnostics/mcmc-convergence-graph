#' Compute pairwise R-hat matrices for multiple parameters
#'
#' Computes pairwise R-hat matrices for a set of parameters in a
#' three-dimensional draws array. If no parameters are specified, matrices are
#' computed for all parameters in the draws array.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#'   all parameters in `draws` are used.
#'
#' @returns A named list of square numeric matrices. Each matrix contains
#' pairwise R-hat values for one parameter.
#'
#' @keywords internal
pairwise_rhat_matrices <- function(draws, parameters = NULL) {
  check_draws_array(draws)
  parameter_names <- dimnames(draws)[[3]]
  if (is.null(parameters)) {
    parameters <- parameter_names
  }
  matrices <- lapply(parameters, function(parameter) {
    pairwise_rhat_matrix(draws, parameter)
  })
  names(matrices) <- parameters
  matrices
}
