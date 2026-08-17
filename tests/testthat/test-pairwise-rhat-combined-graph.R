# Tests for pairwise_rhat_combined_graph().
#
# Main purpose:
#   Combine pairwise R-hat graph information across multiple parameters.
#
# We test:
#   1. The combined graph keeps parameter information on edges.
#   2. parameters = NULL can use all parameters.


test_that("combined graph keeps parameter information on edges", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- pairwise_rhat_combined_graph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 10
  )

  expect_true(igraph::ecount(graph) > 0)

  expect_true("parameters" %in% igraph::edge_attr_names(graph))
  expect_true("n_parameters" %in% igraph::edge_attr_names(graph))
})


test_that("combined graph uses all parameters when parameters is NULL", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta", "theta")
  )

  graph <- pairwise_rhat_combined_graph(
    draws = draws,
    parameters = NULL,
    rho = 10
  )

  parameter_colors <- igraph::graph_attr(graph, "parameter_colors")

  expect_equal(
    names(parameter_colors),
    c("alpha", "beta", "theta")
  )
})


test_that("intersection mode yields a subgraph of the union graph", {
  set.seed(2)

  n_iter <- 200

  draws <- array(
    NA_real_,
    dim = c(n_iter, 4, 2)
  )

  dimnames(draws) <- list(
    NULL,
    paste0("chain", 1:4),
    c("alpha", "beta")
  )

  draws[, "chain1", "alpha"] <- rnorm(n_iter, mean = 0)
  draws[, "chain2", "alpha"] <- rnorm(n_iter, mean = 0)
  draws[, "chain3", "alpha"] <- rnorm(n_iter, mean = 5)
  draws[, "chain4", "alpha"] <- rnorm(n_iter, mean = 5)

  draws[, , "beta"] <- rnorm(n_iter * 4)

  graph_union <- pairwise_rhat_combined_graph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.05,
    mode = "union"
  )

  graph_intersection <- pairwise_rhat_combined_graph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.05,
    mode = "intersection"
  )

  expect_equal(igraph::graph_attr(graph_union, "mode"), "union")
  expect_equal(igraph::graph_attr(graph_intersection, "mode"), "intersection")

  expect_true(
    igraph::ecount(graph_intersection) <= igraph::ecount(graph_union)
  )

  expect_true(
    igraph::ecount(graph_union) > igraph::ecount(graph_intersection)
  )

  if (igraph::ecount(graph_intersection) > 0) {
    expect_true(
      all(igraph::E(graph_intersection)$n_parameters == 2)
    )
  }
})
