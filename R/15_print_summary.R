#' Print an MCMC convergence graph summary
#'
#' Compact printer for objects returned by [mcmc_graph_summary()]
#' and [mcmcgraph()]. Shows, in order:
#'
#' 1. The pairwise R-hat matrix for each parameter.
#' 2. The parameter-level summary table (one row per per-dimension graph).
#'
#' @param x An `mcmcgraph` object.
#' @param digits Number of decimal digits used to display the pairwise R-hat
#'   matrices. The default is 4.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print_summary <- function(x, digits = 4, ...) {
  cat(
    "MCMC convergence graph (rho = ",
    format(x$rho),
    ")\n\n",
    sep = ""
  )

  cat("1. Pairwise R-hat matrices\n")
  cat(strrep("-", 26), "\n", sep = "")

  for (parameter in names(x$rhat_matrices)) {
    cat("\n$`", parameter, "`\n", sep = "")
    print(round(x$rhat_matrices[[parameter]], digits))
  }

  cat("\n")

  cat("2. Parameter summary\n")
  cat(strrep("-", 20), "\n", sep = "")

  summary_table <- x$summary
  names(summary_table)[names(summary_table) == "pairwise_rhat_value"] <-
    "largest pairwise R-hat value"
  row.names(summary_table) <- NULL

  print(summary_table)

  invisible(x)
}

#' @rdname print_summary
#' @export
print.mcmcgraph <- print_summary
