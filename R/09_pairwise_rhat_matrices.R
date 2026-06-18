#calculate matrices for all parameters
#if user does not specify parameters, use all parameter
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
