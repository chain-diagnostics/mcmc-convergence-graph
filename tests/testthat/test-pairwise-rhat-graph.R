# Tests for pairwise_rhat_graph().
#
# Main purpose:
#   Convert one pairwise R-hat matrix into an undirected graph.
#
# We test:
#   1. The output is an igraph object.
#   2. The graph has one vertex per chain.
#   3. Vertex names match chain names.
#   4. Edges are created when pairwise R-hat is below rho.
#   5. Edges are not created when pairwise R-hat is above rho.


test_that("pairwise_rhat_graph returns an igraph object", {
  rhat_matrix <- matrix(
    c(
      1.00, 1.01, 1.20,
      1.01, 1.00, 1.25,
      1.20, 1.25, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  rownames(rhat_matrix) <- paste0("chain", 1:3)
  colnames(rhat_matrix) <- paste0("chain", 1:3)

  graph <- pairwise_rhat_graph(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_s3_class(graph, "igraph")
})


test_that("pairwise_rhat_graph has one vertex per chain", {
  rhat_matrix <- matrix(
    c(
      1.00, 1.01, 1.20,
      1.01, 1.00, 1.25,
      1.20, 1.25, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  rownames(rhat_matrix) <- paste0("chain", 1:3)
  colnames(rhat_matrix) <- paste0("chain", 1:3)

  graph <- pairwise_rhat_graph(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(igraph::vcount(graph), 3)
})


test_that("pairwise_rhat_graph keeps chain names as vertex names", {
  rhat_matrix <- matrix(
    c(
      1.00, 1.01, 1.20,
      1.01, 1.00, 1.25,
      1.20, 1.25, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  rownames(rhat_matrix) <- paste0("chain", 1:3)
  colnames(rhat_matrix) <- paste0("chain", 1:3)

  graph <- pairwise_rhat_graph(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(
    igraph::V(graph)$name,
    paste0("chain", 1:3)
  )
})


test_that("pairwise_rhat_graph creates edges below the threshold", {
  rhat_matrix <- matrix(
    c(
      1.00, 1.01, 1.20,
      1.01, 1.00, 1.25,
      1.20, 1.25, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  rownames(rhat_matrix) <- paste0("chain", 1:3)
  colnames(rhat_matrix) <- paste0("chain", 1:3)

  graph <- pairwise_rhat_graph(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(igraph::ecount(graph), 1)

  edge_list <- igraph::as_edgelist(graph)

  expect_equal(
    edge_list[1, ],
    c("chain1", "chain2")
  )
})


test_that("pairwise_rhat_graph does not create edges above the threshold", {
  rhat_matrix <- matrix(
    c(
      1.00, 1.20, 1.30,
      1.20, 1.00, 1.25,
      1.30, 1.25, 1.00
    ),
    nrow = 3,
    byrow = TRUE
  )

  rownames(rhat_matrix) <- paste0("chain", 1:3)
  colnames(rhat_matrix) <- paste0("chain", 1:3)

  graph <- pairwise_rhat_graph(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(igraph::vcount(graph), 3)
  expect_equal(igraph::ecount(graph), 0)
})
