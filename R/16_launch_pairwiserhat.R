#' Run the full pairwise R-hat workflow
#'
#' Convenience wrapper around the pairwise R-hat pipeline. Computes
#' parameter-specific pairwise R-hat matrices, per-parameter graphs, cluster
#' summaries, both across-parameter combined graphs (union `G_union` and
#' intersection `G_intersection`), and (by default) draws the intersection
#' combined graph `G_intersection` and a 2D posterior-draws scatter for the
#' first two monitored parameters. Optionally also writes both plots to PDF
#' files.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters, or an rstan `stanfit` object.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#'   all parameters in `draws` are used.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#' @param plot Logical. If `TRUE` (default), the intersection combined graph
#'   `G_intersection` is drawn using [plot_pairwise_rhat_combined_graph()] on
#'   the current graphics device, followed by the posterior-draws scatter from
#'   [plot_posterior_draws()]. Pass `result$combined_graph` to
#'   [plot_pairwise_rhat_combined_graph()] manually to plot the union graph
#'   `G_union` instead.
#' @param save_csv Logical. If `TRUE`, numerical outputs are saved as CSV files.
#'   The default is `FALSE`.
#' @param save_plot Logical. If `TRUE`, the intersection combined graph
#'   `G_intersection` is written to `combined_graph.pdf` and the posterior-draws
#'   scatter is written to `posterior_draws.pdf` inside `output_dir`
#'   (both 8 x 6 inches). Requires `output_dir`. The default is `FALSE`.
#' @param output_dir Optional character string giving the directory where CSV
#'   files and/or the PDF plots should be saved. Required when `save_csv = TRUE`
#'   or `save_plot = TRUE`.
#' @param pairwise_display_n Integer giving the number of largest pairwise
#'   R-hat values to include in `pairwise_values_display`. The default is 10.
#' @param posterior_draws_parameters Optional character vector of length two
#'   giving the parameters used for the posterior-draws scatter. If `NULL`
#'   (the default), the first two monitored parameters are used. The
#'   posterior-draws plot is skipped, with a warning, if `ggplot2` is not
#'   installed or fewer than two monitored parameters are available.
#'
#' @returns Invisibly returns the list produced by
#'   [pairwise_rhat_parameter_summary()], augmented with
#'   `posterior_draws_plot` (a `ggplot` object, or `NULL` when the
#'   posterior-draws plot was skipped). The other fields are `summary`,
#'   `pairwise_values_display`, `pairwise_values`, `clusters`, `graphs`,
#'   `combined_graph` (the union graph `G_union`),
#'   `combined_graph_intersection` (the intersection graph `G_intersection`),
#'   `rhat_matrices`, and `rho`.
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
#' result <- pairwiserhat(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.015,
#'   plot = FALSE
#' )
#'
#' result$summary
pairwiserhat <- function(
    draws,
    parameters = NULL,
    rho = 1.01,
    plot = TRUE,
    save_csv = FALSE,
    save_plot = FALSE,
    output_dir = NULL,
    pairwise_display_n = 10,
    posterior_draws_parameters = NULL,
    ...
) {
  if (isTRUE(save_plot) && is.null(output_dir)) {
    stop(
      "`output_dir` must be provided when `save_plot = TRUE`.",
      call. = FALSE
    )
  }

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = parameters,
    rho = rho,
    save_csv = save_csv,
    output_dir = output_dir,
    pairwise_display_n = pairwise_display_n
  )

  monitored_parameters <- names(result$rhat_matrices)

  if (is.null(posterior_draws_parameters) && length(monitored_parameters) >= 2L) {
    posterior_draws_parameters <- monitored_parameters[1:2]
  }

  draws_plot <- NULL

  if (isTRUE(plot) || isTRUE(save_plot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning(
        "Package 'ggplot2' is not installed; skipping the posterior-draws plot.",
        call. = FALSE
      )
    } else if (is.null(posterior_draws_parameters) ||
               length(posterior_draws_parameters) < 2L) {
      warning(
        "Fewer than two monitored parameters available; skipping the ",
        "posterior-draws plot.",
        call. = FALSE
      )
    } else {
      draws_plot <- plot_posterior_draws(
        draws      = draws,
        parameters = posterior_draws_parameters
      )
    }
  }

  result$posterior_draws_plot <- draws_plot

  if (isTRUE(plot)) {
    plot_pairwise_rhat_combined_graph(result$combined_graph_intersection, ...)

    if (!is.null(draws_plot)) {
      print(draws_plot)
    }
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
    on.exit(grDevices::dev.off(), add = TRUE)

    plot_pairwise_rhat_combined_graph(result$combined_graph_intersection, ...)

    if (!is.null(draws_plot)) {
      ggplot2::ggsave(
        filename = file.path(output_dir, "posterior_draws.pdf"),
        plot     = draws_plot,
        width    = 8,
        height   = 6
      )
    }
  }

  invisible(result)
}
