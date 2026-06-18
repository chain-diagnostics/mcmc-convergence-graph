#get chain/node name
#list all the possible chain pairs
# check whether each chain pair appears as edge in each parameter graph
# store the result in an edge table
# turn that edge table into an igraph object
#add color widths labels
#V=vertices E=deges


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
