#' Compute split R-hat
#'
#' Stan split R-hat on an already split matrix: rows are draws, columns are
#' split chains.
#'
#' @param split_draws A numeric matrix. Rows are draws, columns are split chains.
#'
#' @return A single split R-hat value.
#'
#' @keywords internal


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
