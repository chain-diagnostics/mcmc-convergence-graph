#' Plot a combined pairwise R-hat graph
#'
#' Plots a combined pairwise R-hat graph created by
#' `pairwise_rhat_combined_graph()`. Chains are shown as nodes, and edges are
#' colored according to the parameters that support each connection.
#'
#' @param graph An `igraph` object created by `pairwise_rhat_combined_graph()`.
#' @param show_edge_labels Logical. If `TRUE`, edge labels show the parameters
#'   supporting each edge. The default is `FALSE`.
#' @param show_legend Logical. If `TRUE`, a legend showing parameter colors is
#'   added to the plot.
#' @param layout_matrix Optional numeric matrix giving node positions. If `NULL`,
#'   the layout is computed automatically using `layout_type`.
#' @param layout_type Character string specifying the automatic layout to use
#'   when `layout_matrix` is `NULL`. Options are `"grid"`, `"fr"`, `"kk"`,
#'   `"nicely"`, `"circle"`, `"random"`, `"tree"`, and `"drl"`.
#' @param main Character string giving the plot title.
#' @param vertex_size Numeric value controlling node size.
#' @param vertex_label_cex Numeric value controlling chain-label size.
#' @param edge_label_cex Numeric value controlling edge-label size.
#' @param edge_curved Numeric value controlling edge curvature.
#' @param legend_position Character string giving the legend position.
#' @param legend_cex Numeric value controlling legend text size.
#' @param show_node_note Logical. If `TRUE`, adds a note saying that node labels
#'   represent chain indices.
#'
#' @returns Invisibly returns the layout matrix used for the plot.
#'
#' @export
plot_pairwise_rhat_combined_graph <- function(
  graph,
  show_edge_labels = FALSE,
  show_legend = TRUE,
  layout_matrix = NULL,
  layout_type = c("grid", "fr", "kk", "nicely", "circle", "random", "tree", "drl"),
  main = "Combined pairwise R-hat graph",
  vertex_size = 10,
  vertex_label_cex = 0.8,
  edge_label_cex = 0.8,
  edge_curved = 0.1,
  legend_position = "top",
  legend_cex = 0.8,
  show_node_note = TRUE
) {
  layout_type <- match.arg(layout_type)

  if (is.null(layout_matrix)) {
    layout_matrix <- switch(layout_type,
      grid = igraph::layout_on_grid(graph),
      fr = igraph::layout_with_fr(graph),
      kk = igraph::layout_with_kk(graph),
      nicely = igraph::layout_nicely(graph),
      circle = igraph::layout_in_circle(graph),
      random = igraph::layout_randomly(graph),
      tree = igraph::layout_as_tree(graph),
      drl = igraph::layout_with_drl(graph)
    )
  }

  vertex_labels <- igraph::V(graph)$name
  vertex_labels_clean <- gsub("[^0-9]", "", vertex_labels)

  if (any(vertex_labels_clean == "")) {
    vertex_labels_clean <- as.character(seq_along(vertex_labels))
  }

  vertex_labels <- vertex_labels_clean

  edge_labels <- NA

  if (show_edge_labels) {
    edge_labels <- igraph::E(graph)$label
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(
    mar = c(5, 1, 7, 1),
    xpd = NA
  )

  plot(
    graph,
    layout = layout_matrix,
    vertex.size = vertex_size,
    vertex.color = igraph::V(graph)$color,
    vertex.frame.color = "grey30",
    vertex.label = vertex_labels,
    vertex.label.color = "black",
    vertex.label.cex = vertex_label_cex,
    edge.color = igraph::E(graph)$color,
    edge.width = igraph::E(graph)$width,
    edge.curved = edge_curved,
    edge.label = edge_labels,
    edge.label.cex = edge_label_cex,
    edge.label.color = "black",
    main = main,
    asp = 0
  )

  if (show_node_note) {
    graphics::mtext(
      "Node labels indicate chain indices.",
      side = 1,
      line = 3,
      adj = 0,
      cex = 0.8
    )
  }

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

    if (legend_position == "top") {
      graphics::legend(
        "top",
        inset = c(0, -0.12),
        legend = legend_labels,
        col = legend_colors,
        lwd = 2,
        bty = "n",
        cex = legend_cex
      )
    } else {
      graphics::legend(
        legend_position,
        legend = legend_labels,
        col = legend_colors,
        lwd = 2,
        bty = "n",
        cex = legend_cex
      )
    }
  }

  invisible(layout_matrix)
}
