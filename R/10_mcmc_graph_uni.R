#' Convert a pairwise R-hat matrix to a graph
#'
#' Converts a pairwise \eqn{\hat{R}} matrix into an undirected MCMC
#' convergence graph \eqn{G_\rho}. Each chain is a node, and two nodes
#' \eqn{i, j} are connected if and only if \eqn{\hat{R}_{ij} < \rho}.
#'
#' @param rhat_matrix A square numeric matrix of pairwise \eqn{\hat{R}} values.
#' @param rho Numeric threshold used to decide whether two chains are connected.
#'   An edge is added when \eqn{\hat{R}_{ij} < \rho}.
#'
#' @returns An undirected `igraph` object whose nodes represent chains and whose
#' edges represent chain pairs with pairwise \eqn{\hat{R}} strictly below `rho`.
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

#Input is one pairwise matrix from function pairwise_rhat_matrix
mcmc_graph_uni <- function(rhat_matrix, rho = 1.05) {
  #True/False matrix of the same size
  adjacency_matrix <- rhat_matrix < rho
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
  #Turn a TRUE/FALSE matrix into a graph object
  graph <- igraph::graph_from_adjacency_matrix(
    adjacency_matrix,
    mode = "undirected"
  )
  graph
}
