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
  n <- nrow(split_draws)

  chain_means <- colMeans(split_draws)
  chain_vars <- apply(split_draws, 2, stats::var)

  within_chain_var <- mean(chain_vars)
  between_chain_var <- n * stats::var(chain_means)

  var_hat <- ((n - 1) / n) * within_chain_var +
    (1 / n) * between_chain_var

  sqrt(var_hat / within_chain_var)
}
