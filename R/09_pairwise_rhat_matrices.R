#' Compute pairwise R-hat matrices for multiple parameters
#'
#' Computes pairwise R-hat matrices for a set of parameters. If no parameters
#' are specified, matrices are computed for all parameters in the input.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters, or an rstan `stanfit` object.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#'   all parameters in `draws` are used.
#'
#' @returns A named list of square numeric matrices. Each matrix contains
#' pairwise R-hat values for one parameter.
#'
#' @export
#'
#' @examples
#' set.seed(20)
#'
#' draws <- array(
#'   rnorm(100 * 4 * 2),
#'   dim = c(100, 4, 2)
#' )
#'
#' dimnames(draws) <- list(
#'   NULL,
#'   paste0("chain", 1:4),
#'   c("alpha", "beta")
#' )
#'
#' pairwise_rhat_matrices(
#'   draws = draws,
#'   parameters = c("alpha", "beta")
#' )
pairwise_rhat_matrices <- function(draws, parameters = NULL) {
  draws <- as_draws_array(draws, parameter = parameters)
  parameters <- dimnames(draws)[[3]]
  matrices <- lapply(parameters, function(parameter) {
    pairwise_rhat_matrix(draws, parameter)
  })
  names(matrices) <- parameters
  matrices
}
