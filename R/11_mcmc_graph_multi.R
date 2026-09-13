#' Build a combined pairwise R-hat graph from parameter graphs
#'
#' Internal helper that constructs a combined graph from a named list of
#' parameter-specific pairwise R-hat graphs. Used by
#' [mcmc_graph_multi()] and [mcmc_graph_summary()].
#'
#' Two combined graphs are supported. With `mode = "union"`, an edge is
#' included whenever it is present in at least one parameter graph
#' (`G_union`). With `mode = "intersection"`, an edge is included only
#' when it is present in every parameter graph (`G_intersection`).
#'
#' @param parameter_graphs A named list of `igraph` objects, one per parameter,
#'   each built with [mcmc_graph_uni()].
#' @param rho Numeric threshold used to build the parameter graphs. Stored on
#'   the returned graph as a graph attribute.
#' @param mode Character string. Either `"union"` (default) or `"intersection"`,
#'   selecting how the parameter graphs are combined.
#'
#' @returns An undirected `igraph` object with edge attributes `parameters`,
#'   `n_parameters`, `color`, `width`, `label`, vertex attribute `color`, and
#'   graph attributes `parameter_colors`, `rho`, and `mode`.
#'
#' @keywords internal
build_combined_graph <- function(parameter_graphs,
                                 rho,
                                 mode = c("union", "intersection")) {
  mode <- match.arg(mode)

  parameter_names <- names(parameter_graphs)
  n_all_parameters <- length(parameter_names)

  first_graph <- parameter_graphs[[1]]

  chain_names <- igraph::V(first_graph)$name

  chain_pairs <- utils::combn(chain_names, 2)

  edge_list <- data.frame(
    from = character(),
    to = character(),
    parameters = character(),
    n_parameters = integer(),
    stringsAsFactors = FALSE
  )

  for (pair_id in seq_len(ncol(chain_pairs))) {
    from_chain <- chain_pairs[1, pair_id]
    to_chain <- chain_pairs[2, pair_id]

    edge_parameters <- character()

    for (parameter in parameter_names) {
      graph <- parameter_graphs[[parameter]]

      is_connected <- igraph::are_adjacent(
        graph,
        from_chain,
        to_chain
      )

      if (is_connected) {
        edge_parameters <- c(edge_parameters, parameter)
      }
    }

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

  combined_graph <- igraph::graph_from_data_frame(
    d = edge_list,
    directed = FALSE,
    vertices = data.frame(
      name = chain_names,
      stringsAsFactors = FALSE
    )
  )

  parameter_colors <- grDevices::rainbow(length(parameter_names))
  names(parameter_colors) <- parameter_names

  edge_colors <- character(igraph::ecount(combined_graph))

  for (edge_id in seq_len(igraph::ecount(combined_graph))) {
    edge_parameters <- strsplit(
      igraph::E(combined_graph)$parameters[edge_id],
      ", "
    )[[1]]

    if (length(edge_parameters) == 1) {
      edge_colors[edge_id] <- parameter_colors[edge_parameters]
    } else {
      edge_colors[edge_id] <- "black"
    }
  }

  igraph::E(combined_graph)$color <- edge_colors
  igraph::E(combined_graph)$width <- 1 + igraph::E(combined_graph)$n_parameters
  igraph::E(combined_graph)$label <- igraph::E(combined_graph)$parameters

  igraph::V(combined_graph)$color <- "lightblue"

  combined_graph <- igraph::set_graph_attr(
    combined_graph,
    "parameter_colors",
    parameter_colors
  )

  combined_graph <- igraph::set_graph_attr(
    combined_graph,
    "rho",
    rho
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
#' Corresponds to `G_union` minus `G_intersection`: chain pairs that agree on at
#' least one monitored dimension but not on every dimension.
#'
#' @param graph_union An `igraph` object built with `mode = "union"`.
#' @param n_all_parameters Integer number of monitored dimensions.
#'
#' @returns The union graph with shared (intersection) edges removed, and
#'   graph attribute `mode` set to `"difference"`.
#'
#' @keywords internal
combined_graph_set_difference <- function(graph_union, n_all_parameters) {
  difference_graph <- graph_union

  if (igraph::ecount(difference_graph) > 0) {
    is_shared <- igraph::E(difference_graph)$n_parameters == n_all_parameters
    difference_graph <- igraph::delete_edges(
      difference_graph,
      igraph::E(difference_graph)[is_shared]
    )
  }

  igraph::set_graph_attr(difference_graph, "mode", "difference")
}

#' Build a combined pairwise R-hat graph across parameters
#'
#' Builds a combined graph from parameter-specific pairwise R-hat graphs. Each
#' chain is represented as a node. The way edges are combined across parameters
#' is controlled by `mode`. With `mode = "union"` (default), an edge is added
#' between two chains if at least one parameter has a pairwise R-hat value less
#' than or equal to `rho` for that chain pair, corresponding to `G_union` in
#' the multivariate MCMC convergence graph formulation. With
#' `mode = "intersection"`, an edge is added only if every parameter has a
#' pairwise R-hat value less than or equal to `rho` for that chain pair,
#' corresponding to `G_intersection`. Edge labels record which parameters
#' support each edge, and edge widths increase with the number of supporting
#' parameters.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#' iterations by chains by parameter, or an rstan `stanfit` object.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#' all parameters in `draws` are used.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#' @param mode Character string. Either `"union"` (default) or `"intersection"`,
#'   selecting how the parameter graphs are combined.
#'
#' @returns An undirected `igraph` object. Nodes represent chains. Edges
#' represent chain pairs connected in at least one (`mode = "union"`) or every
#' (`mode = "intersection"`) parameter-specific graph.
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
#' graph_union <- mcmc_graph_multi(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.05
#' )
#'
#' graph_intersection <- mcmc_graph_multi(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.05,
#'   mode = "intersection"
#' )
mcmc_graph_multi <- function(draws,
                                         parameters = NULL,
                                         rho = 1.05,
                                         mode = c("union", "intersection")) {
  mode <- match.arg(mode)

  rhat_matrices <- pairwise_rhat_matrices(
    draws,
    parameters = parameters
  )

  parameter_names <- names(rhat_matrices)

  parameter_graphs <- lapply(rhat_matrices, function(rhat_matrix) {
    mcmc_graph_uni(rhat_matrix, rho = rho)
  })

  names(parameter_graphs) <- parameter_names

  build_combined_graph(parameter_graphs, rho = rho, mode = mode)
}
