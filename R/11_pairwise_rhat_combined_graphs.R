#'Build a combined pairwise R-hat graph across parameters
#'
#'Builds a combined graph from parameter-specific pairwsie R-hat graphs. Each
#'chain is represented as a node. An edge is added between two chains if at least
#'one parameter has a pairwise R-hat value less than or equal to 'rho' for that
#'chain pair. Edge labels record which parameters support each edge, and edge
#'widths increase with the number of supporting parameters.
#'
#'@param draws A three-dimensional array of posterior draws with dimensions
#'iterations by chains by parameter.
#'@param parameters Optional character vector of parameter names. If 'NULL',
#'all parameters in 'draws' are used.
#'@param rho Numeric threshold used to decide whether two chains are connectedx.
#'
#'@returns An undirected 'igrpah' ovject. Nodes represent chains. Edges represent
#'chain pairs connected in at least one parameter-specific graph.
#'
#'@export

# Compute pairwise R-hat matrices for selected parameters.
# Convert each matrix into a parameter-specific graph.
# Use the first graph to get chain names.
# List all possible chain pairs.
# For each chain pair, check which parameter graphs contain that edge.
# Store supported edges in an edge table.
# Convert the edge table into one combined igraph object.
# Add edge colors, widths, and labels.
# Store parameter colors and rho as graph attributes.


pairwise_rhat_combined_graph <- function(draws, parameters = NULL, rho = 1.01) {
  rhat_matrices <- pairwise_rhat_matrices(
    draws,
    parameters = parameters
  )

  parameter_names <- names(rhat_matrices)

  parameter_graphs <- lapply(rhat_matrices, function(rhat_matrix) {
    pairwise_rhat_graph(rhat_matrix, rho = rho)
  })

  names(parameter_graphs) <- parameter_names

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

    if (length(edge_parameters) > 0) {
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

  combined_graph
}
