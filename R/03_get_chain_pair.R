#' Get all chain pairs for one parameter
#'
#' Extracts all unique pairs of chains for a selected parameter from a
#' 'three dimensional draws array. For each pair, the function returns the chain
#' indices and the corresponding posterior draws
#'
#' @param draws A three dimensional array of posterior draws with dimensions
#' iterations by chains by parameters.
#' @param parameter A single parameter name. The name must be present in the
#' third dimension of 'draws'. This argument can not be 'NULL'.
#'
#' @returns A list of chain-pair objects. Each element contains the index of the
#' first chain, the index of the second chian and the corresponding draws for the
#' selected parameter.
#'
#' @keywords internal

get_chain_pairs <- function(draws, parameter) {
  check_draws_array(draws)
  n_chains <- dim(draws)[2]
  parameter_names <- dimnames(draws)[[3]]

  if (!parameter %in% parameter_names) {
    stop("`parameter` must be one of the parameter names in `draws`.", call. = FALSE)
  }
  pairs <- utils::combn(n_chains, 2)  #create all unique pair of chains
  lapply(seq_len(ncol(pairs)), function(pair_id) {
    chain_i <- pairs[1, pair_id]

    chain_j <- pairs[2, pair_id]

    list(
      chain_i_index = chain_i,
      chain_j_index = chain_j,
      chain_i_draws = draws[, chain_i, parameter], #eg: all iterations from chain 1 for alpha*
      chain_j_draws = draws[, chain_j, parameter]
    )
  })
}
