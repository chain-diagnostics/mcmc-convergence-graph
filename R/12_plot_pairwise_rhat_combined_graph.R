#' PLot a combined pairwise R hat graph
#'
#' Plots a combined pairwise R hat graph created by
#' 'pairwise_rhat_combined_graph()' . Chains are shown as nodes, and edges are
#' colored and labeled according to the parameters that support each connection.
#'
#' @param graph A 'igraph' object created by 'pairwise_rhat_combined_graph()'
#' @param show_edge_labels If True, edge labels show the parameters.
#' @param show_legend If True, a legend showing parameter colors is added to the
#' plot.
#' @param layout_matrix  Optional numeric matrix giving node position.
#' if NULL, the function computes the layout automatically.
#' @param main Character string giving the plot title.
#'
#' @returns Invisible returns the layout matrix used for the plot
#'
#' @export



plot_pairwise_rhat_combined_graph <- function(
    graph,
    show_edge_labels = TRUE,
    show_legend = TRUE,
    layout_matrix = NULL,
    main = "Combined pairwise R-hat graph"
) {
  if (is.null(layout_matrix)) {
    layout_matrix <- igraph::layout_with_fr(graph)
  }

  edge_labels <- NA

  if (show_edge_labels) {
    edge_labels <- igraph::E(graph)$label
  }

  plot(
    graph,
    layout = layout_matrix,
    vertex.size = 30,
    vertex.color = igraph::V(graph)$color,
    vertex.label.color = "black",
    vertex.label.cex = 1,
    edge.color = igraph::E(graph)$color,
    edge.width = igraph::E(graph)$width,
    edge.curved = 0.2,
    edge.label = edge_labels,
    edge.label.cex = 0.7,
    main = main
  )

  parameter_colors <- igraph::graph_attr(
    graph,
    "parameter_colors"
  )

  has_shared_edges <- FALSE

  if (igraph::ecount(graph) > 0) {
    has_shared_edges <- any(
      igraph::E(graph)$n_parameters > 1
    )
  }

  if (show_legend && !is.null(parameter_colors)) {
    legend_labels <- names(parameter_colors)
    legend_colors <- parameter_colors

    if (has_shared_edges) {
      legend_labels <- c(
        legend_labels,
        "shared edge"
      )

      legend_colors <- c(
        legend_colors,
        "black"
      )
    }

    graphics::legend(
      "topleft",
      legend = legend_labels,
      col = legend_colors,
      lwd = 2,
      bty = "n",
      cex = 0.8
    )
  }

  invisible(layout_matrix)
}
