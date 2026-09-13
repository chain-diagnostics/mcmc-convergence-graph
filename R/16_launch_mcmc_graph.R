#' Run the MCMC convergence graph workflow
#'
#' Convenience wrapper around the MCMC convergence graph pipeline. Computes
#' parameter-specific pairwise R-hat matrices, per-parameter graphs, component
#' summaries, the combined graphs `G_union`, `G_intersection`, and
#' `G_union` minus `G_intersection`, and (by default) draws one graph:
#' the intersection layout with additional `G_union` minus `G_intersection`
#' edges overlaid. Optionally also writes the plot to a PDF file.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters, or an rstan `stanfit` object.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#'   all parameters in `draws` are used.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#' @param plot Logical. If `TRUE` (default), one combined graph is drawn:
#'   node positions come from `G_intersection`, black solid edges are
#'   `G_intersection`, and coloured dashed edges are the additional edges
#'   `G_union` minus `G_intersection`.
#' @param save_csv Logical. If `TRUE`, numerical outputs are saved as CSV files.
#'   The default is `FALSE`.
#' @param save_plot Logical. If `TRUE`, the combined intersection-plus-
#'   additional-edge graph is written to `combined_graph.pdf` inside
#'   `output_dir` (8 x 6 inches). Requires `output_dir`. The default is `FALSE`.
#' @param output_dir Optional character string giving the directory where CSV
#'   files and/or the PDF plot should be saved. Required when `save_csv = TRUE`
#'   or `save_plot = TRUE`.
#' @param pairwise_display_n Integer giving the number of largest pairwise
#'   R-hat values to include in `pairwise_values_display`. The default is 10.
#' @param ... Additional arguments passed to [plot_mcmc_graph()].
#'
#' @returns Invisibly returns the list produced by
#'   [mcmc_graph_summary()]. The fields are `summary`,
#'   `pairwise_values_display`, `pairwise_values`, `clusters`, `graphs`,
#'   `combined_graph` (the union graph `G_union`),
#'   `combined_graph_intersection` (the intersection graph `G_intersection`),
#'   `combined_graph_difference` (`G_union` minus `G_intersection`),
#'   `intersection_summary`, `rhat_matrices`, and `rho`.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' draws <- array(
#'   rnorm(100 * 4 * 2),
#'   dim = c(100, 4, 2)
#' )
#'
#' dimnames(draws) <- list(
#'   NULL,
#'   paste0("chain", 1:4),
#'   c("alpha", "beta")
#' )
#'
#' result <- mcmcgraph(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.05,
#'   plot = FALSE
#' )
#'
#' result$summary
launch_mcmc_graph <- function(
    draws,
    parameters = NULL,
    rho = 1.05,
    plot = TRUE,
    save_csv = FALSE,
    save_plot = FALSE,
    output_dir = NULL,
    pairwise_display_n = 10,
    ...
) {
  if (isTRUE(save_plot) && is.null(output_dir)) {
    stop(
      "`output_dir` must be provided when `save_plot = TRUE`.",
      call. = FALSE
    )
  }

  result <- mcmc_graph_summary(
    draws = draws,
    parameters = parameters,
    rho = rho,
    save_csv = save_csv,
    output_dir = output_dir,
    pairwise_display_n = pairwise_display_n
  )

  monitored_parameters <- names(result$rhat_matrices)

  intersection_layout <- NULL
  if (isTRUE(plot) || isTRUE(save_plot)) {
    intersection_layout <- layout_from_intersection(
      result$combined_graph_intersection,
      result$combined_graph
    )
  }

  graph_main <- if (length(monitored_parameters) > 1L) {
    "Multivariate MCMC convergence graph"
  } else {
    "MCMC convergence graph"
  }

  if (isTRUE(plot)) {
    plot_mcmc_graph(
      result$combined_graph,
      layout_matrix = intersection_layout,
      main = graph_main,
      ...
    )
  }

  if (isTRUE(save_plot)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    grDevices::pdf(
      file   = file.path(output_dir, "combined_graph.pdf"),
      width  = 8,
      height = 6
    )
    plot_mcmc_graph(
      result$combined_graph,
      layout_matrix = intersection_layout,
      main = graph_main,
      ...
    )
    grDevices::dev.off()
  }

  invisible(result)
}

#' @rdname launch_mcmc_graph
#' @export
mcmcgraph <- launch_mcmc_graph
