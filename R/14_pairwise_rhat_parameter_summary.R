pairwise_rhat_parameter_summary <- function(draws, parameters = NULL, rho = 1.01) {
  rhat_matrices <- pairwise_rhat_matrices(
    draws,
    parameters = parameters
  )

  parameter_names <- names(rhat_matrices)

  parameter_graphs <- lapply(rhat_matrices, function(rhat_matrix) {
    pairwise_rhat_graph(rhat_matrix, rho = rho)
  })

  names(parameter_graphs) <- parameter_names

  cluster_results <- lapply(parameter_graphs, function(graph) {
    pairwise_rhat_clusters(graph)
  })

  names(cluster_results) <- parameter_names

  summary_table <- data.frame(
    parameter = character(),
    n_clusters = integer(),
    n_isolated = integer(),
    isolated_chains = character(),
    clusters = character(),
    stringsAsFactors = FALSE
  )

  for (parameter in parameter_names) {
    cluster_result <- cluster_results[[parameter]]

    if (length(cluster_result$clusters) == 0) {
      cluster_text <- ""
    } else {
      cluster_text <- vapply(
        cluster_result$clusters,
        function(cluster) {
          paste(cluster, collapse = ", ")
        },
        character(1)
      )

      cluster_text <- paste(
        paste0("(", cluster_text, ")"),
        collapse = " | "
      )
    }

    if (length(cluster_result$isolated_chains) == 0) {
      isolated_text <- ""
    } else {
      isolated_text <- paste(
        cluster_result$isolated_chains,
        collapse = ", "
      )
    }

    summary_table <- rbind(
      summary_table,
      data.frame(
        parameter = parameter,
        n_clusters = cluster_result$n_clusters,
        n_isolated = cluster_result$n_isolated,
        isolated_chains = isolated_text,
        clusters = cluster_text,
        stringsAsFactors = FALSE
      )
    )
  }

  list(
    summary = summary_table,
    clusters = cluster_results,
    graphs = parameter_graphs,
    rhat_matrices = rhat_matrices,
    rho = rho
  )
}
