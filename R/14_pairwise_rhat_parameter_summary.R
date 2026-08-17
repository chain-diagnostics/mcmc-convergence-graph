#' Summarize pairwise R-hat graph structure across parameters
#'
#' Computes parameter-specific pairwise R-hat matrices, converts each matrix into
#' a graph, and summarizes the graph structure for each parameter. The summary
#' reports the number of multi-chain clusters, the number of isolated chains,
#' the cluster composition, and the pairwise R-hat value for each parameter.
#'
#' The reported `pairwise_rhat_value` is the largest pairwise R-hat over
#' (i) pairs that lie in the same multi-chain cluster of the parameter-specific
#' graph at threshold `rho`, and (ii) pairs that involve at least one isolated
#' chain. Pairs between two different multi-chain clusters are excluded, so a
#' cleanly separated multimodal posterior in which each mode is internally
#' coherent still yields a small `pairwise_rhat_value`, even though the
#' classical R-hat across all chains would be large. Isolated chains, by
#' contrast, are treated as a mixing problem and are included in the maximum,
#' so `pairwise_rhat_value` rises when `n_isolated > 0`. When a parameter has
#' fewer than two chains, `pairwise_rhat_value` is `NA`.
#'
#' The function also returns a full table of pairwise R-hat values for every
#' parameter and chain pair. For convenient display, it additionally returns a
#' shortened table containing the largest pairwise R-hat values. By default, the
#' display table contains the top 10 values, and the maximum allowed display
#' size is 20.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters, or an rstan `stanfit` object.
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
#' @returns A list with eight elements:
#' \describe{
#'   \item{summary}{A data frame with one row per parameter, giving the number of multi-chain clusters (`n_clusters`), the number of isolated chains (`n_isolated`), the cluster composition (`clusters`), the isolated chain identifiers (`isolated_chains`), and the largest pairwise R-hat over within-cluster pairs and pairs involving isolated chains (`pairwise_rhat_value`; `NA` when fewer than two chains are available).}
#'   \item{pairwise_values_display}{A shortened data frame containing the largest pairwise R-hat values for display.}
#'   \item{pairwise_values}{A full data frame containing one pairwise R-hat value for each parameter and chain pair.}
#'   \item{clusters}{A named list of detailed cluster results for each parameter.}
#'   \item{graphs}{A named list of parameter-specific `igraph` objects.}
#'   \item{combined_graph}{The union combined `igraph` object across parameters (`G_union`). An edge is present whenever the chain pair is connected in at least one parameter-specific graph. Pass to [plot_pairwise_rhat_combined_graph()] to visualise `G_union`.}
#'   \item{combined_graph_intersection}{The intersection combined `igraph` object across parameters (`G_intersection`). An edge is present only when the chain pair is connected in every parameter-specific graph. This is the graph plotted by default by [pairwiserhat()], and is the graph whose connected components carry the multivariate mode-counting guarantee.}
#'   \item{rhat_matrices}{A named list of pairwise R-hat matrices.}
#'   \item{rho}{The threshold used to define graph edges.}
#' }
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
#' result <- pairwise_rhat_parameter_summary(
#'   draws = draws,
#'   parameters = c("alpha", "beta"),
#'   rho = 1.015,
#'   save_csv = FALSE
#' )
#'
#' result$summary
#' result$pairwise_values_display
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

  combined_graph <- build_combined_graph(
    parameter_graphs,
    rho = rho,
    mode = "union"
  )

  combined_graph_intersection <- build_combined_graph(
    parameter_graphs,
    rho = rho,
    mode = "intersection"
  )

  summary_table <- data.frame(
    parameter = character(),
    n_clusters = integer(),
    n_isolated = integer(),
    isolated_chains = character(),
    clusters = character(),
    pairwise_rhat_value = numeric(),
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

    # Multi-chain cluster id for each chain; NA if the chain is isolated.
    multi_cluster_id <- stats::setNames(
      rep(NA_integer_, length(chain_names)),
      chain_names
    )
    for (cluster_idx in seq_along(cluster_result$clusters)) {
      multi_cluster_id[cluster_result$clusters[[cluster_idx]]] <- cluster_idx
    }
    isolated_set <- cluster_result$isolated_chains

    parameter_pairwise_values <- data.frame(
      parameter = character(),
      chain_i = character(),
      chain_j = character(),
      pairwise_rhat = numeric(),
      stringsAsFactors = FALSE
    )

    # Include: within-cluster pairs, and any pair involving an isolate.
    # Exclude: pairs between two different multi-chain clusters.
    included_values <- numeric(0)

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

      i_isolated <- chain_i %in% isolated_set
      j_isolated <- chain_j %in% isolated_set
      same_multi_cluster <- !i_isolated &&
        !j_isolated &&
        !is.na(multi_cluster_id[[chain_i]]) &&
        multi_cluster_id[[chain_i]] == multi_cluster_id[[chain_j]]

      if (same_multi_cluster || i_isolated || j_isolated) {
        included_values <- c(included_values, rhat_value)
      }
    }

    pairwise_values <- rbind(
      pairwise_values,
      parameter_pairwise_values
    )

    pairwise_rhat_value <- if (length(included_values) == 0) {
      NA_real_
    } else {
      max(included_values)
    }

    summary_table <- rbind(
      summary_table,
      data.frame(
        parameter = parameter,
        n_clusters = cluster_result$n_clusters,
        n_isolated = cluster_result$n_isolated,
        isolated_chains = isolated_text,
        clusters = cluster_text,
        pairwise_rhat_value = pairwise_rhat_value,
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

  structure(
    list(
      summary = summary_table,
      pairwise_values_display = pairwise_values_display,
      pairwise_values = pairwise_values,
      clusters = cluster_results,
      graphs = parameter_graphs,
      combined_graph = combined_graph,
      combined_graph_intersection = combined_graph_intersection,
      rhat_matrices = rhat_matrices,
      rho = rho
    ),
    class = "pairwiserhat"
  )
}
