#' Plot a combined pairwise R-hat graph
#'
#' Plots a combined pairwise R-hat graph created by
#' [mcmc_graph_multi()]. Chains are shown as nodes. On a
#' multivariate graph, solid black edges are `G_intersection` (agree on
#' every dimension) and dashed grey edges are `G_union` minus
#' `G_intersection` (agree on some dimensions only), matching Fig. 2(c).
#'
#' @param graph An `igraph` object created by [mcmc_graph_multi()].
#'   The default `mcmcgraph()` figure passes the union graph so that
#'   shared (black) edges are `G_intersection` and coloured edges are the
#'   additional edges `G_union` minus `G_intersection`.
#' @param show_edge_labels Logical. If `TRUE`, edge labels show the parameters
#'   supporting each edge. The default is `FALSE`.
#' @param show_legend Logical. If `TRUE`, a legend showing parameter colors is
#'   added to the plot.
#' @param layout_matrix Optional numeric matrix giving node positions. If `NULL`,
#'   the layout is computed automatically using `layout_type`.
#' @param layout_type Character string specifying the automatic layout to use
#'   when `layout_matrix` is `NULL`. The default is `"circle"` so every chain
#'   sits on the ring and no node lies on an edge. Options are `"circle"`,
#'   `"grid"`, `"fr"`, `"kk"`, `"nicely"`, `"random"`, `"tree"`, and `"drl"`.
#' @param main Character string giving the plot title.
#' @param vertex_size Numeric value controlling node size.
#' @param vertex_label_cex Numeric value controlling chain-label size.
#' @param edge_label_cex Numeric value controlling edge-label size.
#' @param edge_curved Numeric curvature passed to igraph, or `NULL` (the
#'   default) for the paper style: a small same-sign bow
#'   `0.03 + 0.02 * d_n` that is almost straight and does not twist.
#'   Use `0` for perfectly straight chords, or a larger scalar (e.g. `0.2`)
#'   for a uniform bow on every edge.
#' @param legend_position Character string giving the legend position.
#' @param legend_cex Numeric value controlling legend text size.
#' @param show_node_note Logical. If `TRUE`, adds a note saying that node labels
#'   represent chain indices.
#'
#' @return Invisibly returns the layout matrix used for the plot.
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
#' graph <- mcmc_graph_multi(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.05
#' )
#'
#' plot_mcmc_graph(
#'   graph,
#'   layout_type = "circle"
#' )
plot_mcmc_graph <- function(
  graph,
  show_edge_labels = FALSE,
  show_legend = TRUE,
  layout_matrix = NULL,
  layout_type = c("circle", "grid", "fr", "kk", "nicely", "random", "tree", "drl"),
  main = "Combined pairwise R-hat graph",
  vertex_size = 20,
  vertex_label_cex = 1.8,
  edge_label_cex = 0.8,
  edge_curved = NULL,
  legend_position = "top",
  legend_cex = 0.8,
  show_node_note = FALSE
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

  if (is.null(edge_curved)) {
    edge_curved <- almost_straight_edge_curvature(graph, layout_matrix)
  }

  edge_labels <- NA

  if (show_edge_labels) {
    edge_labels <- igraph::E(graph)$label
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(
    mar = c(2.5, 1, 6.2, 1),
    pty = "s",
    xpd = NA,
    lend = "round",
    ljoin = "round"
  )

  plot(
    graph,
    layout = layout_matrix,
    vertex.size = vertex_size,
    vertex.color = igraph::V(graph)$color,
    vertex.frame.color = NA,
    vertex.label = vertex_labels,
    vertex.label.color = "black",
    vertex.label.cex = vertex_label_cex,
    edge.color = edge_color_from_graph(graph),
    edge.width = 1.6,
    edge.lty = edge_lty_from_graph(graph),
    edge.curved = edge_curved,
    edge.label = edge_labels,
    edge.label.cex = edge_label_cex,
    edge.label.color = "black",
    main = main,
    ylim = c(-1.2, 1.35),
    asp = 1
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

  has_intersection_edges <- FALSE
  has_additional_edges <- FALSE

  if (igraph::ecount(graph) > 0 && !is.null(igraph::E(graph)$n_parameters)) {
    n_monitored <- n_monitored_parameters(graph)
    has_intersection_edges <- n_monitored > 1 &&
      any(igraph::E(graph)$n_parameters == n_monitored)
    has_additional_edges <- n_monitored > 1 &&
      any(igraph::E(graph)$n_parameters < n_monitored)
  }

  if (show_legend && (has_intersection_edges || has_additional_edges)) {
    legend_labels <- character()
    legend_colors <- character()
    legend_lty <- integer()

    if (has_additional_edges) {
      legend_labels <- c(legend_labels, "agree on some dimensions only")
      legend_colors <- c(legend_colors, "grey50")
      legend_lty <- c(legend_lty, 2L)
    }

    if (has_intersection_edges) {
      legend_labels <- c(legend_labels, "agree on every dimension")
      legend_colors <- c(legend_colors, "black")
      legend_lty <- c(legend_lty, 1L)
    }

    horizontal_legend <- legend_position %in% c("top", "bottom")

    if (legend_position == "top") {
      graphics::legend(
        "top",
        inset  = c(0, -0.12),
        legend = legend_labels,
        col    = legend_colors,
        lty    = legend_lty,
        lwd    = 2,
        horiz  = horizontal_legend,
        bty    = "n",
        cex    = legend_cex
      )
    } else {
      graphics::legend(
        legend_position,
        legend = legend_labels,
        col    = legend_colors,
        lty    = legend_lty,
        lwd    = 2,
        horiz  = horizontal_legend,
        bty    = "n",
        cex    = legend_cex
      )
    }
  }

  invisible(layout_matrix)
}

#' Almost-straight same-sign edge bows used by the paper figures
#'
#' @keywords internal
almost_straight_edge_curvature <- function(graph, layout) {
  el <- igraph::as_edgelist(graph, names = FALSE)
  n_e <- nrow(el)
  if (n_e == 0L) {
    return(numeric())
  }

  from_xy <- layout[el[, 1L], , drop = FALSE]
  to_xy <- layout[el[, 2L], , drop = FALSE]
  d <- sqrt(rowSums((from_xy - to_xy)^2))
  d_n <- if (diff(range(d)) > 1e-8) {
    (d - min(d)) / diff(range(d))
  } else {
    rep(0.5, n_e)
  }

  0.03 + 0.02 * d_n
}

#' Number of monitored dimensions stored on a combined graph
#'
#' @keywords internal
n_monitored_parameters <- function(graph) {
  parameter_colors <- igraph::graph_attr(graph, "parameter_colors")

  if (!is.null(parameter_colors) && length(parameter_colors) > 0) {
    return(length(parameter_colors))
  }

  n_parameters <- igraph::E(graph)$n_parameters

  if (is.null(n_parameters) || length(n_parameters) == 0) {
    return(1L)
  }

  max(n_parameters)
}

#' Line type matching Fig. 2(c): solid intersection, dashed difference
#'
#' @keywords internal
edge_lty_from_graph <- function(graph) {
  if (igraph::ecount(graph) == 0) {
    return(integer())
  }

  n_parameters <- igraph::E(graph)$n_parameters

  if (is.null(n_parameters) || n_monitored_parameters(graph) <= 1) {
    return(rep(1L, igraph::ecount(graph)))
  }

  ifelse(n_parameters == n_monitored_parameters(graph), 1L, 2L)
}

#' Edge colour matching Fig. 2(c): black intersection, grey difference
#'
#' @keywords internal
edge_color_from_graph <- function(graph) {
  if (igraph::ecount(graph) == 0) {
    return(character())
  }

  n_parameters <- igraph::E(graph)$n_parameters

  if (is.null(n_parameters) || n_monitored_parameters(graph) <= 1) {
    return(rep("black", igraph::ecount(graph)))
  }

  ifelse(
    n_parameters == n_monitored_parameters(graph),
    "black",
    "grey50"
  )
}

#' Node layout from the intersection graph, aligned to another graph
#'
#' Default positions are a circle on `graph_intersection`: every chain sits
#' on the ring, so no node lies on a chord. Vertices from the same
#' `G_intersection` component occupy a contiguous arc. Other `layout_type`
#' values use the matching igraph algorithm. The matrix is then reordered to
#' match the vertex order of `graph_union`.
#'
#' @keywords internal
layout_from_intersection <- function(graph_intersection,
                                     graph_union,
                                     layout_type = "circle") {
  if (identical(layout_type, "circle")) {
    comps <- igraph::components(graph_intersection)
    names_int <- igraph::V(graph_intersection)$name
    order_ids <- order(comps$csize, decreasing = TRUE)
    grouped <- unlist(lapply(order_ids, function(id) {
      nm <- names_int[comps$membership == id]
      digits <- gsub("[^0-9]", "", nm)
      num <- suppressWarnings(as.integer(digits))
      if (anyNA(num) || any(digits == "")) {
        nm[order(nm)]
      } else {
        nm[order(num)]
      }
    }), use.names = FALSE)
    ord <- match(grouped, names_int)
    layout_matrix <- igraph::layout_in_circle(graph_intersection, order = ord)
  } else {
    layout_matrix <- switch(
      layout_type,
      grid = igraph::layout_on_grid(graph_intersection),
      fr = igraph::layout_with_fr(graph_intersection),
      kk = igraph::layout_with_kk(graph_intersection),
      nicely = igraph::layout_nicely(graph_intersection),
      random = igraph::layout_randomly(graph_intersection),
      tree = igraph::layout_as_tree(graph_intersection),
      drl = igraph::layout_with_drl(graph_intersection),
      igraph::layout_in_circle(graph_intersection)
    )
  }

  from_names <- igraph::V(graph_intersection)$name
  to_names <- igraph::V(graph_union)$name
  layout_matrix[match(to_names, from_names), , drop = FALSE]
}
