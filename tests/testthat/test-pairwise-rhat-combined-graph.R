# Tests for the combined graph from mcmcgraph() / mcmc_graph_summary().
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

  graph <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 10,
    plot = FALSE
  )$combined_graph

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

  graph <- mcmcgraph(
    draws = draws,
    parameters = NULL,
    rho = 10,
    plot = FALSE
  )$combined_graph

  expect_equal(
    igraph::graph_attr(graph, "parameter_names"),
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

  result <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.05,
    plot = FALSE
  )
  graph_union <- result$combined_graph
  graph_intersection <- result$combined_graph_intersection

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


test_that("intersection summary counts modes on G_intersection, not per dimension", {
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

  result <- mcmc_graph_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.05,
    save_csv = FALSE
  )

  expect_equal(nrow(result$intersection_summary), 1L)
  expect_equal(result$intersection_summary$graph, "G_intersection")
  expect_equal(result$intersection_summary$n_clusters, 2L)
  expect_equal(result$intersection_summary$n_isolated, 0L)

  expect_equal(igraph::graph_attr(result$combined_graph_difference, "mode"), "difference")
  expect_gt(igraph::ecount(result$combined_graph_difference), 0)
  expect_true(
    all(igraph::E(result$combined_graph_difference)$n_parameters < 2)
  )
})


test_that("a one-parameter graph uses black edges and omits pairs above rho", {
  rhat_matrix <- matrix(
    1.00,
    nrow = 3,
    ncol = 3
  )
  rhat_matrix[1, 3] <- 1.08
  rhat_matrix[3, 1] <- 1.08
  diag(rhat_matrix) <- 1
  rownames(rhat_matrix) <- paste0("chain:", 1:3)
  colnames(rhat_matrix) <- paste0("chain:", 1:3)

  parameter_graphs <- list(b = mcmc_graph_uni(rhat_matrix, rho = 1.05))
  graph <- mcmc_graph_multi(
    parameter_graphs,
    rho = 1.05,
    mode = "union"
  )

  expect_false(igraph::are_adjacent(graph, "chain:1", "chain:3"))
  expect_equal(n_monitored_parameters(graph), 1L)
  expect_equal(
    edge_color_from_graph(graph),
    rep("black", igraph::ecount(graph))
  )
  expect_equal(
    edge_lty_from_graph(graph),
    rep(1L, igraph::ecount(graph))
  )
})

