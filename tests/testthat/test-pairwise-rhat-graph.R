# Tests for mcmc_graph_uni().
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


test_that("mcmc_graph_uni returns an igraph object", {
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

  graph <- mcmc_graph_uni(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_s3_class(graph, "igraph")
})


test_that("mcmc_graph_uni has one vertex per chain", {
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

  graph <- mcmc_graph_uni(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(igraph::vcount(graph), 3)
})


test_that("mcmc_graph_uni keeps chain names as vertex names", {
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

  graph <- mcmc_graph_uni(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(
    igraph::V(graph)$name,
    paste0("chain", 1:3)
  )
})


test_that("mcmc_graph_uni creates edges below the threshold", {
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

  graph <- mcmc_graph_uni(
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


test_that("mcmc_graph_uni omits only the pairs above rho with Stan chain names", {
  rhat_matrix <- matrix(
    1.00,
    nrow = 4,
    ncol = 4
  )
  rhat_matrix[1, 4] <- 1.078
  rhat_matrix[4, 1] <- 1.078
  rhat_matrix[4, 3] <- 1.055
  rhat_matrix[3, 4] <- 1.055
  diag(rhat_matrix) <- 1
  rownames(rhat_matrix) <- paste0("chain:", 1:4)
  colnames(rhat_matrix) <- paste0("chain:", 1:4)

  graph <- mcmc_graph_uni(rhat_matrix, rho = 1.05)

  expect_false(igraph::are_adjacent(graph, "chain:1", "chain:4"))
  expect_false(igraph::are_adjacent(graph, "chain:4", "chain:3"))
  expect_true(igraph::are_adjacent(graph, "chain:1", "chain:2"))
  expect_equal(igraph::ecount(graph), choose(4, 2) - 2L)
})


test_that("mcmc_graph_uni does not create edges above the threshold", {
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

  graph <- mcmc_graph_uni(
    rhat_matrix = rhat_matrix,
    rho = 1.015
  )

  expect_equal(igraph::vcount(graph), 3)
  expect_equal(igraph::ecount(graph), 0)
})
