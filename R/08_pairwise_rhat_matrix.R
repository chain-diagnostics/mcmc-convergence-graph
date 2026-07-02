#' Compute a pairwise R-hat matrix for one parameter
#'
#' Compute pairwise R-hat value for all unique pairs of chains for one selected
#' parameter. The result is a square matrix whose rows and columns correspond to
#' chains.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#' iterations by chains by parameters.
#' @param parameter A single parameter name. The name must be present in the
#' third dimension of `draws`.
#'
#' @returns A square numeric matrix of pairwise R-hat values. The diagonal is set
#' to 1, and the off-diagonal entries contain pairwise R-hat values between
#' chains.
#'
#' @export


pairwise_rhat_matrix <- function(draws, parameter) {
  check_draws_array(draws)
  n_chains <- dim(draws)[2]
  chain_pairs <- get_chain_pairs(draws, parameter)
  rhat_matrix <- matrix(
    NA_real_,
    nrow = n_chains,
    ncol = n_chains
  )
  diag(rhat_matrix) <- 1
  for (pair in chain_pairs) {
    chain_i <- pair$chain_i_index
    chain_j <- pair$chain_j_index
    rhat_value <- pairwise_rhat(
      pair$chain_i_draws,
      pair$chain_j_draws
    )
    rhat_matrix[chain_i, chain_j] <- rhat_value
    rhat_matrix[chain_j, chain_i] <- rhat_value
  }
  chain_names <- dimnames(draws)[[2]]

  if (is.null(chain_names)) {
    chain_names <- paste0("chain", seq_len(n_chains))
  }

  rownames(rhat_matrix) <- chain_names
  colnames(rhat_matrix) <- chain_names
  rhat_matrix
}
