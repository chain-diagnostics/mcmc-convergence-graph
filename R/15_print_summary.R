#' Print an MCMC convergence graph summary
#'
#' Compact printer for objects returned by [mcmc_graph_summary()]
#' and [mcmcgraph()]. Shows, in order:
#'
#' 1. The pairwise R-hat matrix for each parameter.
#' 2. The top-N largest pairwise R-hat values (`N` = `pairwise_display_n` from
#'    the analysis call).
#' 3. The parameter-level summary table (one row per per-dimension graph).
#' 4. The intersection-graph summary (multivariate mode count from
#'    `G_intersection`).
#'
#' All other fields (`graphs`, `combined_graph`, `clusters`, `pairwise_values`,
#' `rho`, etc.) remain accessible via `$`; they are simply omitted from this
#' printed view.
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

  n_display <- nrow(x$pairwise_values_display)

  cat(
    "2. Top ",
    n_display,
    " largest pairwise R-hat values\n",
    sep = ""
  )
  cat(strrep("-", 36), "\n", sep = "")

  display_table <- x$pairwise_values_display
  row.names(display_table) <- NULL

  print(display_table)

  cat("\n")

  cat("3. Parameter summary\n")
  cat(strrep("-", 20), "\n", sep = "")

  summary_table <- x$summary
  row.names(summary_table) <- NULL

  print(summary_table)

  cat("\n")

  cat("4. Intersection summary (G_intersection)\n")
  cat(strrep("-", 40), "\n", sep = "")

  intersection_table <- x$intersection_summary
  row.names(intersection_table) <- NULL

  print(intersection_table)

  invisible(x)
}

#' @rdname print_summary
#' @export
print.mcmcgraph <- print_summary
