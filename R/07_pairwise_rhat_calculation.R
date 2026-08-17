#' Compute pairwise R-hat for two chains
#'
#' Computes the pairwise rank-normalized split R-hat statistic for two chains of
#' posterior draws from one parameter. The function also computes the folded
#' split R-hat and returns the larger of the rank-normalized and folded values.
#'
#' @param chain_i_draws A numeric vector of posterior draws from the first chain.
#' @param chain_j_draws A numeric vector of posterior draws from the second chain.
#'
#' @returns A single numeric value giving the pairwise R-hat between two chains.
#'
#' @export


pairwise_rhat <- function(chain_i_draws, chain_j_draws) {
  n_iter_i <- length(chain_i_draws)
  n_iter_j <- length(chain_j_draws)
  if (n_iter_i != n_iter_j) {
    stop(
      "`chain_i_draws` and `chain_j_draws` must have the same length.",
      call. = FALSE
    )
  }
  pooled_draws <- c(chain_i_draws, chain_j_draws)
  rank_draws <- rank_normalize(pooled_draws)
  rank_chain_i <- rank_draws[seq_len(n_iter_i)]
  rank_chain_j <- rank_draws[(n_iter_i + 1):(n_iter_i + n_iter_j)]
  split_i <- split_chain(rank_chain_i)
  split_j <- split_chain(rank_chain_j)
  rank_split_draws <- cbind(
    split_i$first,
    split_i$second,
    split_j$first,
    split_j$second
  )
  rank_rhat <- split_rhat(rank_split_draws)
  folded_draws <- abs(pooled_draws - stats::median(pooled_draws))
  folded_rank_draws <- rank_normalize(folded_draws)
  folded_rank_chain_i <- folded_rank_draws[seq_len(n_iter_i)]
  folded_rank_chain_j <- folded_rank_draws[(n_iter_i + 1):(n_iter_i + n_iter_j)]
  folded_split_i <- split_chain(folded_rank_chain_i)
  folded_split_j <- split_chain(folded_rank_chain_j)
  folded_split_draws <- cbind(
    folded_split_i$first,
    folded_split_i$second,
    folded_split_j$first,
    folded_split_j$second
  )
  folded_rhat <- split_rhat(folded_split_draws)
  max(rank_rhat, folded_rhat)
}
