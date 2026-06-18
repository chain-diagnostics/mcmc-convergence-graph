pairwise_rhat_graph <- function(rhat_matrix, rho = 1.01) {
  adjacency_matrix <- rhat_matrix <= rho
  diag(adjacency_matrix) <- FALSE
  chain_names <- rownames(rhat_matrix)
  if (is.null(chain_names)) {
    chain_names <- colnames(rhat_matrix)
  }
  if (is.null(chain_names)) {
    chain_names <- paste0("chain", seq_len(nrow(rhat_matrix)))
  }
  rownames(adjacency_matrix) <- chain_names
  colnames(adjacency_matrix) <- chain_names
  graph <- igraph::graph_from_adjacency_matrix(
  adjacency_matrix,
    mode = "undirected"
  )
  graph
}
