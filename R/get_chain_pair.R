# the function takes a full MCMC draws array
# find all unique chain pairs
# for each pair, it return the chain indices and the corresponding numberic draw values
# function got two input: draws and paramter
# give all combinations of 2 chains from n_chains

get_chain_pair <- function(draws, parameters) {
  if (!is.array(draws) || length(dim(draws)) != 3) {
    stop("'draws' must be a 3-dimensional array with dimentsions iteration, chain, and parameter.",
      call. = FALSE
    )
  }
  n_chains <- dim(draws)[2]
  parameter_names <- dimnames(draws)[3]
  if (n_chains < 2) {
    stop("`draws` must contain at least 2 chains.", call. = FALSE)
  }
  pairs <- utils::combn(n_chains, 2)
  lapply(seq_len(ncol(pairs)), function(k) {
    chain_i <- pairs[1, k]
    chain_j <- pairs[2, k]
    list(
      chain_i_index = chain_i,
      chain_j_index = chain_j,
      chain_i = draws[, chain_i, parameter],
      chain_j = draws[, chain_j, parameter]
    )
  })
}
