#' Summarize pairwise R-hat graph structure across parameters
#'
#' Computes parameter-specific pairwise R-hat matrices, converts each matrix into
#' a graph, and summarizes the graph structure for each parameter. The summary
#' reports the number of multi-chain clusters, the number of isolated chains,
#' the cluster structure, and the largest pairwise R-hat value for each
#' parameter.
#'
#' The function also returns a full table of pairwise R-hat values for every
#' parameter and chain pair. For convenient display, it additionally returns a
#' shortened table containing the largest pairwise R-hat values. By default, the
#' display table contains the top 10 values, and the maximum allowed display
#' size is 20.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters.
#' @param parameters Optional character vector of parameter names. If `NULL`,
#'   all parameters in `draws` are used.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#'   For each parameter, an edge is added when the pairwise R-hat value is less
#'   than or equal to `rho`.
#' @param save_csv Logical. If `TRUE`, numerical outputs are saved as CSV files.
#'   The default is `FALSE`.
#' @param output_dir Optional character string giving the directory where CSV
#'   files should be saved. This must be provided when `save_csv = TRUE`.
#' @param pairwise_display_n Integer giving the number of largest pairwise R-hat
#'   values to include in `pairwise_values_display`. The default is 10. Values
#'   larger than 20 are capped at 20.
#'
#' @returns A list with seven elements:
#' \describe{
#'   \item{summary}{A data frame summarizing clusters, isolated chains, and the maximum pairwise R-hat for each parameter.}
#'   \item{pairwise_values_display}{A shortened data frame containing the largest pairwise R-hat values for display.}
#'   \item{pairwise_values}{A full data frame containing one pairwise R-hat value for each parameter and chain pair.}
#'   \item{clusters}{A named list of detailed cluster results for each parameter.}
#'   \item{graphs}{A named list of parameter-specific `igraph` objects.}
#'   \item{rhat_matrices}{A named list of pairwise R-hat matrices.}
#'   \item{rho}{The threshold used to define graph edges.}
#' }
#'
#' @export


pairwise_rhat_parameter_summary <- function(
  draws,
  parameters = NULL,
  rho = 1.01,
  save_csv = FALSE,
  output_dir = NULL,
  pairwise_display_n = 10
) {
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

    chain_names <- rownames(rhat_matrix)

    if (is.null(chain_names)) {
      chain_names <- colnames(rhat_matrix)
    }

    if (is.null(chain_names)) {
      chain_names <- paste0("chain", seq_len(nrow(rhat_matrix)))
    }

    rownames(rhat_matrix) <- chain_names
    colnames(rhat_matrix) <- chain_names
    rhat_matrices[[parameter]] <- rhat_matrix

    chain_display_names <- gsub("[^0-9]", "", chain_names)

    if (any(chain_display_names == "")) {
      chain_display_names <- as.character(seq_along(chain_names))
    }

    chain_label_map <- stats::setNames(
      chain_display_names,
      chain_names
    )

    if (length(cluster_result$clusters) == 0) {
      cluster_text <- ""
    } else {
      cluster_text <- vapply(
        cluster_result$clusters,
        function(cluster) {
          paste(chain_label_map[cluster], collapse = ", ")
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
        chain_label_map[cluster_result$isolated_chains],
        collapse = ", "
      )
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
          chain_i = chain_label_map[[chain_i]],
          chain_j = chain_label_map[[chain_j]],
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

  if (!is.numeric(pairwise_display_n) || length(pairwise_display_n) != 1) {
    stop(
      "`pairwise_display_n` must be a single numeric value.",
      call. = FALSE
    )
  }

  pairwise_display_n <- as.integer(pairwise_display_n)

  if (pairwise_display_n < 0) {
    stop(
      "`pairwise_display_n` must be greater than or equal to 0.",
      call. = FALSE
    )
  }

  if (pairwise_display_n > 20) {
    warning(
      "`pairwise_display_n` is larger than 20. Only the top 20 values will be displayed.",
      call. = FALSE
    )

    pairwise_display_n <- 20
  }

  pairwise_values_display <- pairwise_values[
    order(pairwise_values$pairwise_rhat, decreasing = TRUE),
  ]

  pairwise_values_display <- utils::head(
    pairwise_values_display,
    pairwise_display_n
  )

  if (save_csv) {
    if (is.null(output_dir)) {
      stop(
        "`output_dir` must be provided when `save_csv = TRUE`.",
        call. = FALSE
      )
    }

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    utils::write.csv(
      summary_table,
      file = file.path(output_dir, "pairwise_rhat_parameter_summary.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      pairwise_values_display,
      file = file.path(output_dir, "pairwise_rhat_values_display.csv"),
      row.names = FALSE
    )

    utils::write.csv(
      pairwise_values,
      file = file.path(output_dir, "pairwise_rhat_values.csv"),
      row.names = FALSE
    )

    for (parameter in names(rhat_matrices)) {
      safe_parameter_name <- gsub(
        "[^A-Za-z0-9_]",
        "_",
        parameter
      )

      utils::write.csv(
        rhat_matrices[[parameter]],
        file = file.path(
          output_dir,
          paste0("pairwise_rhat_matrix_", safe_parameter_name, ".csv")
        ),
        row.names = TRUE
      )
    }
  }

  list(
    summary = summary_table,
    pairwise_values_display = pairwise_values_display,
    pairwise_values = pairwise_values,
    clusters = cluster_results,
    graphs = parameter_graphs,
    rhat_matrices = rhat_matrices,
    rho = rho
  )
}
