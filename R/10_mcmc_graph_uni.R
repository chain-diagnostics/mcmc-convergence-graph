#' Convert a pairwise R-hat matrix to a graph
#'
#' Converts a pairwsie R-hat matrix into an undirected graph. Each chain is a
#' node and an edge is added between two chains where their pairwise R-hat valuep
#' is less than or equal to 'rho'.
#'
#' @param rhat_matrix A square numeric matrix of pairwise R-hat values.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#'
#' @returns An undirected 'igraph' object whose nodes represent chains and whose
#' edges represent chain pairs with pairwise R-hat value below the threshold.
#'
#' @export
#'
#' @examples
#' rhat_matrix <- matrix(
#'   c(
#'     1.000, 1.010, 1.020,
#'     1.010, 1.000, 1.025,
#'     1.020, 1.025, 1.000
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#'
#' rownames(rhat_matrix) <- paste0("chain", 1:3)
#' colnames(rhat_matrix) <- paste0("chain", 1:3)
#'
#' graph <- mcmc_graph_uni(
#'   rhat_matrix = rhat_matrix,
#'   rho = 1.05
#' )
#'
#' graph
#' igraph::E(graph)
mcmc_graph_uni <- function(rhat_matrix, rho = 1.05) {
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
