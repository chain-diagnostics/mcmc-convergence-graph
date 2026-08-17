#' Print a pairwiserhat result
#'
#' Compact printer for objects returned by [pairwise_rhat_parameter_summary()]
#' and [pairwiserhat()]. Shows, in order:
#'
#' 1. The pairwise R-hat matrix for each parameter.
#' 2. The top-N largest pairwise R-hat values (`N` = `pairwise_display_n` from
#'    the analysis call).
#' 3. The parameter-level summary table.
#'
#' All other fields (`graphs`, `combined_graph`, `clusters`, `pairwise_values`,
#' `rho`, etc.) remain accessible via `$`; they are simply omitted from this
#' printed view.
#'
#' @param x A `pairwiserhat` object.
#' @param digits Number of decimal digits used to display the pairwise R-hat
#'   matrices. The default is 4.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.pairwiserhat <- function(x, digits = 4, ...) {
  cat(
    "Pairwise R-hat result (rho = ",
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

  invisible(x)
}
