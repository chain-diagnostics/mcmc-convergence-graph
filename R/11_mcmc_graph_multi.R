#' Build a combined pairwise R-hat graph from per-dimension graphs
#'
#' Internal helper that constructs a combined graph from a named list of
#' per-dimension pairwise \eqn{\hat{R}} graphs \eqn{G_{\rho,s}}. Used by
#' [mcmc_graph_summary()].
#'
#' Two combined graphs are supported. With `mode = "union"`, an edge is
#' included whenever the pair agrees on at least one dimension, giving the
#' union graph \eqn{G_{\cup}}. With `mode = "intersection"`, an edge is
#' included only when the pair agrees on every dimension, giving the
#' intersection graph \eqn{G_{\cap}}.
#'
#' @param parameter_graphs A named list of `igraph` objects, one per dimension,
#'   each built with [mcmc_graph_uni()] as \eqn{G_{\rho,s}}.
#' @param rho Numeric threshold used to build the per-dimension graphs. Stored on
#'   the returned graph as a graph attribute. An edge in \eqn{G_{\rho,s}}
#'   requires \eqn{\hat{R}_{ij,s} < \rho}.
#' @param mode Character string. Either `"union"` or `"intersection"`.
#'
#' @returns An undirected `igraph` object with edge attributes `parameters`,
#'   `n_parameters`, and `label`, vertex attribute `color`, and graph
#'   attributes `parameter_names`, `rho`, and `mode`.
#'
#' @keywords internal
#'

mcmc_graph_multi <- function(parameter_graphs,
                             rho,
                             mode) {
  parameter_names <- names(parameter_graphs)
  n_all_parameters <- length(parameter_names)

  first_graph <- parameter_graphs[[1]]
  #all nodes of the graph
  chain_names <- igraph::V(first_graph)$name

  chain_pairs <- utils::combn(chain_names, 2)

  edge_list <- data.frame(
    from = character(),
    to = character(),
    parameters = character(),
    n_parameters = integer(),
    stringsAsFactors = FALSE
  )
  #outer loop: one chain pair
  for (pair_id in seq_len(ncol(chain_pairs))) {
    from_chain <- chain_pairs[1, pair_id]
    to_chain <- chain_pairs[2, pair_id]

    edge_parameters <- character()
    #inner loop: which dimensions connect that pair
    for (parameter in parameter_names) {
      graph <- parameter_graphs[[parameter]]
      #igraph::are_adjacent: is there already an edge between nodes a and b in this graph
      #output: one true or false
      is_connected <- igraph::are_adjacent(
        graph,
        from_chain,
        to_chain
      )

      if (is_connected) {
        edge_parameters <- c(edge_parameters, parameter)
      }
    }
    #switch: picks one expression by the string in mode
    #union: true if they agreed on at least one dimension
    #intersection: true only if they agreed on every dimension
    include_edge <- switch(
      mode,
      union = length(edge_parameters) > 0,
      intersection = length(edge_parameters) == n_all_parameters
    )

    if (include_edge) {
      edge_list <- rbind(
        edge_list,
        data.frame(
          from = from_chain,
          to = to_chain,
          parameters = paste(edge_parameters, collapse = ", "),
          n_parameters = length(edge_parameters),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  #turns a table of edges into a graph
  combined_graph <- igraph::graph_from_data_frame(
    d = edge_list,
    directed = FALSE,
    vertices = data.frame(
      name = chain_names,
      stringsAsFactors = FALSE
    )
  )

  igraph::E(combined_graph)$label <- igraph::E(combined_graph)$parameters
  igraph::V(combined_graph)$color <- "lightblue"

  combined_graph <- igraph::set_graph_attr(
    combined_graph,
    "parameter_names",
    parameter_names
  )

  combined_graph <- igraph::set_graph_attr(
    combined_graph,
    "mode",
    mode
  )

  combined_graph
}

#' Edges in the union graph that are absent from the intersection graph
#'
#' Corresponds to the set difference \eqn{G_{\cup} \setminus G_{\cap}}:
#' chain pairs that agree on some marginals while disagreeing on others.
#'
#' @param graph_union An `igraph` object built with `mode = "union"`
#'   (\eqn{G_{\cup}}).
#' @param n_all_parameters Integer number of monitored dimensions.
#'
#' @returns The union graph with shared (intersection) edges removed, giving
#'   \eqn{G_{\cup} \setminus G_{\cap}}, and graph attribute `mode` set to
#'   `"difference"`.
#'
#' @keywords internal

#input: union igraph, output one graph that is \eqn{G_{\cup} \setminus G_{\cap}}(dash lines)
combined_graph_set_difference <- function(graph_union, n_all_parameters) {
  difference_graph <- graph_union
  #how many edges
  if (igraph::ecount(difference_graph) > 0) {
    #which edges agreed on all dimensions
    is_shared <- igraph::E(difference_graph)$n_parameters == n_all_parameters
    #remove those shared edges
    difference_graph <- igraph::delete_edges(
      difference_graph,
      igraph::E(difference_graph)[is_shared]
    )
  }

  igraph::set_graph_attr(difference_graph, "mode", "difference")
}
