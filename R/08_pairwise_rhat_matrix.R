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
  rhat_matrix
}
