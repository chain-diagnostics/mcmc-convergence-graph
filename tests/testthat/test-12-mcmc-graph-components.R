# Tests for mcmc_graph_components().
#
# Main purpose:
#   Summarise the connected components of a pairwise R-hat graph.
#
# We test:
#   1. A fully connected graph has one multi-chain cluster.
#   2. A graph with two multi-chain components gives two clusters.
#   3. Single-chain components are reported as isolated chains, not clusters.


test_that("mcmc_graph_components detects one connected cluster", {
  graph <- igraph::make_graph(
    edges = c(
      "chain1", "chain2",
      "chain2", "chain3",
      "chain3", "chain4"
    ),
    directed = FALSE
  )

  result <- mcmc_graph_components(graph)

  expect_equal(result$n_clusters, 1)
  expect_equal(result$n_isolated, 0)
})


test_that("mcmc_graph_components detects two multi-chain components", {
  graph <- igraph::make_graph(
    edges = c(
      "chain1", "chain2",
      "chain3", "chain4"
    ),
    directed = FALSE
  )

  result <- mcmc_graph_components(graph)

  expect_equal(result$n_clusters, 2)
  expect_equal(result$n_isolated, 0)
})


test_that("mcmc_graph_components detects isolated chains", {
  graph <- igraph::make_empty_graph(
    n = 4,
    directed = FALSE
  )

  igraph::V(graph)$name <- paste0("chain", 1:4)

  graph <- igraph::add_edges(
    graph,
    c("chain1", "chain2")
  )

  result <- mcmc_graph_components(graph)

  expect_equal(result$n_clusters, 1)
  expect_equal(result$n_isolated, 2)
  expect_equal(
    result$isolated_chains,
    c("chain3", "chain4")
  )
})
