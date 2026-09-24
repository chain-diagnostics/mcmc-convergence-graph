#' Plot a combined pairwise R-hat graph
#'
#' Plots a combined pairwise R-hat graph. Chains are shown as nodes. On a
#' multivariate graph, solid black edges are the intersection graph
#' \eqn{G_{\cap}} (agree on every dimension) and dashed grey edges are
#' the set difference \eqn{G_{\cup} \setminus G_{\cap}}.
#'
#' @param graph An `igraph` object, usually `result$combined_graph` from
#'   [mcmcgraph()].
#'   The default `mcmcgraph()` figure passes the union graph \eqn{G_{\cup}}
#'   so that shared (black) edges are \eqn{G_{\cap}} and dashed grey edges are
#'   \eqn{G_{\cup} \setminus G_{\cap}}.
#' @param graph_intersection Optional intersection graph \eqn{G_{\cap}}. When
#'   `layout_matrix` is `NULL`, nodes from the same \eqn{G_{\cap}} component
#'   sit together on the circle.
#' @param show_edge_labels Logical. If `TRUE`, edge labels show the parameters
#'   supporting each edge. The default is `FALSE`.
#' @param show_legend Logical. If `TRUE` (default), a legend is added.
#' @param layout_matrix Optional numeric matrix giving node positions. If `NULL`,
#'   the layout is computed from `layout_type`.
#' @param layout_type Character string. The default is `"circle"`. Other
#'   options are `"grid"`, `"fr"`, `"kk"`, `"nicely"`, `"random"`,
#'   `"tree"`, and `"drl"`.
#' @param main Character string giving the plot title. The default is empty.
#' @param vertex_size Numeric value controlling node size.
#' @param vertex_label_cex Numeric value controlling chain-label size.
#' @param edge_label_cex Numeric value controlling edge-label size.
#' @param edge_curved Numeric curvature passed to igraph. The default is `0`
#'   (straight chords).
#' @param legend_position Character string giving the legend position.
#' @param legend_cex Numeric value controlling legend text size.
#' @param mar Numeric vector of four margins passed to [graphics::par()],
#'   in the order bottom, left, top, right.
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
#' result <- mcmcgraph(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.05,
#'   plot = FALSE
#' )
#'
#' plot_mcmc_graph(
#'   result$combined_graph,
#'   graph_intersection = result$combined_graph_intersection
#' )
plot_mcmc_graph <- function(
  graph,
  graph_intersection = NULL,
  show_edge_labels = FALSE,
  show_legend = TRUE,
  layout_matrix = NULL,
  layout_type = c("circle", "grid", "fr", "kk", "nicely", "random", "tree", "drl"),
  main = "",
  vertex_size = 20,
  vertex_label_cex = 1.8,
  edge_label_cex = 0.8,
  edge_curved = 0.1,
  legend_position = "top",
  legend_cex = 0.8,
  mar = c(0.4, 0.4, 0.4, 0.4)
) {
  layout_type <- match.arg(layout_type)

  if (is.null(layout_matrix)) {
    if (identical(layout_type, "circle") && !is.null(graph_intersection)) {
      #get a list from componets
      comps <- igraph::components(graph_intersection)
      #a vector of chain name
      names_int <- igraph::V(graph_intersection)$name
      order_ids <- order(comps$csize, decreasing = TRUE)
      grouped <- unlist(lapply(order_ids, function(id) {
        #names in the component
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
      from_names <- igraph::V(graph_intersection)$name
      to_names <- igraph::V(graph)$name
      layout_matrix <- layout_matrix[match(to_names, from_names), , drop = FALSE]
    } else {
      layout_matrix <- switch(
        layout_type,
        circle = igraph::layout_in_circle(graph),
        grid = igraph::layout_on_grid(graph),
        fr = igraph::layout_with_fr(graph),
        kk = igraph::layout_with_kk(graph),
        nicely = igraph::layout_nicely(graph),
        random = igraph::layout_randomly(graph),
        tree = igraph::layout_as_tree(graph),
        drl = igraph::layout_with_drl(graph)
      )
    }
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

  if (isTRUE(show_legend) && identical(legend_position, "top")) {
    mar[3] <- max(mar[3], 2.4)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(
    mar = mar,
    pty = "s",
    xpd = NA,
    family = "sans", #font
    lend = "round", #line ends are rounded
    ljoin = "round" #corners where lines meet are rounded
  )

  plot_args <- list(
    graph,
    layout = layout_matrix,
    vertex.size = vertex_size,
    vertex.color = igraph::V(graph)$color,
    vertex.frame.color = NA,
    vertex.label = vertex_labels,
    vertex.label.color = "black",
    vertex.label.cex = vertex_label_cex,
    vertex.label.family = "sans",
    vertex.label.font = 1,
    edge.color = edge_color_from_graph(graph),
    edge.width = 1.6,
    edge.lty = edge_lty_from_graph(graph),
    edge.curved = edge_curved,
    edge.label = edge_labels,
    edge.label.cex = edge_label_cex,
    edge.label.color = "black",
    main = if (is.null(main) || !nzchar(main)) NA else main,
    asp = 1
  )
  do.call(plot, plot_args)

  n_monitored <- n_monitored_parameters(graph)
  has_n_parameters <- igraph::ecount(graph) > 0 &&
    !is.null(igraph::E(graph)$n_parameters)
  has_intersection_edges <- has_n_parameters &&
    n_monitored > 1 &&
    any(igraph::E(graph)$n_parameters == n_monitored)
  has_additional_edges <- has_n_parameters &&
    n_monitored > 1 &&
    any(igraph::E(graph)$n_parameters < n_monitored)

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

#' Number of monitored dimensions stored on a combined graph
#'
#' @keywords internal
n_monitored_parameters <- function(graph) {
  parameter_names <- igraph::graph_attr(graph, "parameter_names")

  if (!is.null(parameter_names) && length(parameter_names) > 0) {
    return(length(parameter_names))
  }

  n_parameters <- igraph::E(graph)$n_parameters

  if (is.null(n_parameters) || length(n_parameters) == 0) {
    return(1L)
  }

  max(n_parameters)
}

#' Line type: solid intersection, dashed difference
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

#' Edge colour: black intersection, grey difference
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
