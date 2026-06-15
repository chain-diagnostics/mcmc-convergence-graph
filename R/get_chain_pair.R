# The function takes a full MCMC draws array.
# It finds all unique chain pairs.
# For each pair, it returns the chain indices and the corresponding numeric draw values.
# The function has two inputs: draws and parameter.
# `draws` should be a 3-dimensional array: iteration x chain x parameter.
# `parameter` should be one parameter name from dimnames(draws)[[3]].
# `pairs` gives all combinations of 2 chains from n_chains.
# `pair_id` is the number of the pair, for example:
#  pair_id = 1 means pair (1, 2), pair_id = 2 means pair (1, 3), etc.
# `chain_i` means the first chain in this pair.
# `chain_j` means the second chain in this pair.

get_chain_pair <- function(draws, parameter) {
  if (!is.array(draws) || length(dim(draws)) != 3) {
    stop(
      "`draws` must be a 3-dimensional array with dimensions iteration, chain, and parameter.",
      call. = FALSE
    )
  }

  n_chains <- dim(draws)[2]

  parameter_names <- dimnames(draws)[[3]]

  if (n_chains < 2) {
    stop("`draws` must contain at least 2 chains.", call. = FALSE)
  }

  if (is.null(parameter_names)) {
    stop("`draws` must have parameter names in `dimnames(draws)[[3]]`.", call. = FALSE)
  }

  if (!parameter %in% parameter_names) {
    stop("`parameter` must be one of the parameter names in `draws`.", call. = FALSE)
  }

  pairs <- utils::combn(n_chains, 2)

  lapply(seq_len(ncol(pairs)), function(pair_id) {
    chain_i <- pairs[1, pair_id]

    chain_j <- pairs[2, pair_id]

    list(
      chain_i_index = chain_i,
      chain_j_index = chain_j,
      chain_i_draws = draws[, chain_i, parameter],
      chain_j_draws = draws[, chain_j, parameter]
    )
  })
}
