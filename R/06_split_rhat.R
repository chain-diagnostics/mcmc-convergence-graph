#' Compute split R-hat from split chains
#'
#' Computes the split R-hat statistic from a matrix of split-chain draws. Each
#' column is treated as one split chain, and each tow corresponds to one draw
#' within the split chain. This function calculates how different are the split
#' chains from each other, compared with how variable each split chain is internally
#'
#' @param split_draws A numeric matrix of split-chain draws. Rows correspond to
#'  draws and columns correspond to split chains.
#'
#'  @returns A numeric value giving the split R-hat statistic.
#'
#'  @keywords internal


split_rhat <- function(split_draws) {
  n_iter <- nrow(split_draws)
  n_chains <- ncol(split_draws)
  chain_means <- colMeans(split_draws)
  chain_vars <- apply(split_draws, 2, function(x) {
    sum((x - mean(x))^2) / (length(x) - 1)
  })
  within_chain_var <- mean(chain_vars)
  overall_mean <- mean(chain_means)
  between_chain_var <- n_iter * sum((chain_means - overall_mean)^2 / (n_chains - 1))
  var_hat <- ((n_iter - 1) / n_iter) * within_chain_var + (1 / n_iter) * between_chain_var
  sqrt(var_hat / within_chain_var)
}
