#' Compute pairwise R-hat for two chains
#'
#' Computes the pairwise rank-normalized split R-hat and rank-normalised
#' split-R-hat statistic for two chains of posterior draws from one parameter,
#' and returns the larger of the rank-normalized and folded values.
#'
#' @param chain_i_draws A numeric vector of posterior draws from the first chain.
#' @param chain_j_draws A numeric vector of posterior draws from the second chain.
#'
#' @return A single numeric value giving the pairwise R-hat between two chains.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' chain_1 <- rnorm(100)
#' chain_2 <- rnorm(100)
#'
#' pairwise_rhat_calculation(chain_1, chain_2)
#'
pairwise_rhat_calculation <- function(chain_i_draws, chain_j_draws) {
  n_iter_i <- length(chain_i_draws)
  n_iter_j <- length(chain_j_draws)
  if (n_iter_i != n_iter_j) {
    stop(
      "`chain_i_draws` and `chain_j_draws` must have the same length.",
      call. = FALSE
    )
  }
  #split chains
  split_i <- split_chain(chain_i_draws)
  split_j <- split_chain(chain_j_draws)
  split_draws <- cbind(
    split_i$first,
    split_i$second,
    split_j$first,
    split_j$second
  )
  #calculate rank-normalised split-R-hat
  rank_z <- rank_normalize(as.vector(split_draws))
  rank_split_draws <- matrix(
    rank_z,
    nrow = nrow(split_draws),
    ncol = 4
  )
  rank_rhat <- split_rhat(rank_split_draws)
  #calculate rank-normalised folded-split-R-hat
  folded_draws <- abs(split_draws - stats::median(split_draws))
  folded_z <- rank_normalize(as.vector(folded_draws))
  folded_split_draws <- matrix(
    folded_z,
    nrow = nrow(split_draws),
    ncol = 4
  )
  folded_rhat <- split_rhat(folded_split_draws)
  #get the larger of two number
  max(rank_rhat, folded_rhat)
}
