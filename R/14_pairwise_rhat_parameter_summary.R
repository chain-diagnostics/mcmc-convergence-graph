#' Summarize pairwise R-hat graph structure across parameters.
#'
#' Computes parameter-specific pairwise R-hat matrices, convert each matrix into
#' a graph, and summarizes the graph structure for each parameter. The summary reports
#' the number of multi-chain clusters and isolated chains.
#'
#' @param draws A three-dimesional array of posterior draws with dimensions iterations
#' by chains by parameters.
#' @param parameters If NULL, all parameters in 'draws' are used.
#' @param rho numeric threshold used to decide whether two chains are connected.
#'
#' @returns A list with fice elements
#' \describe{
#'   \item{summary}{A data frame summarizing clusters and isolated chains for each parameter.}
#'   \item{clusters}{A named list of detailed cluster results for each parameter.}
#'   \item{graphs}{A named list of parameter-specific `igraph` objects.}
#'   \item{rhat_matrices}{A named list of pairwise R-hat matrices.}
#'   \item{rho}{The threshold used to define graph edges.}
#' }
#'
#' @export

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
    max_pairwise_rhat = numeric(),
    stringsAsFactors = FALSE
  )

  pairwise_values <- data.frame(
    parameter = character(),
    chain_i = character(),
    chain_j = character(),
    pairwise_rhat = numeric(),
    stringsAsFactors = FALSE
  )

  for (parameter in parameter_names) {
    cluster_result <- cluster_results[[parameter]]
    rhat_matrix <- rhat_matrices[[parameter]]

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

    chain_names <- rownames(rhat_matrix)

    if (is.null(chain_names)) {
      chain_names <- colnames(rhat_matrix)
    }

    if (is.null(chain_names)) {
      chain_names <- paste0("chain", seq_len(nrow(rhat_matrix)))
    }

    chain_pairs <- utils::combn(chain_names, 2)

    parameter_pairwise_values <- data.frame(
      parameter = character(),
      chain_i = character(),
      chain_j = character(),
      pairwise_rhat = numeric(),
      stringsAsFactors = FALSE
    )

    for (pair_id in seq_len(ncol(chain_pairs))) {
      chain_i <- chain_pairs[1, pair_id]
      chain_j <- chain_pairs[2, pair_id]

      rhat_value <- rhat_matrix[chain_i, chain_j]

      parameter_pairwise_values <- rbind(
        parameter_pairwise_values,
        data.frame(
          parameter = parameter,
          chain_i = chain_i,
          chain_j = chain_j,
          pairwise_rhat = rhat_value,
          stringsAsFactors = FALSE
        )
      )
    }

    pairwise_values <- rbind(
      pairwise_values,
      parameter_pairwise_values
    )

    summary_table <- rbind(
      summary_table,
      data.frame(
        parameter = parameter,
        n_clusters = cluster_result$n_clusters,
        n_isolated = cluster_result$n_isolated,
        isolated_chains = isolated_text,
        clusters = cluster_text,
        max_pairwise_rhat = max(parameter_pairwise_values$pairwise_rhat),
        stringsAsFactors = FALSE
      )
    )
  }

  list(
    summary = summary_table,
    pairwise_values = pairwise_values,
    clusters = cluster_results,
    graphs = parameter_graphs,
    rhat_matrices = rhat_matrices,
    rho = rho
  )
}
