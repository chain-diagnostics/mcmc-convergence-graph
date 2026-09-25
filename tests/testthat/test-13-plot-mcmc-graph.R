# Tests for plot_mcmc_graph().
#
# Main purpose:
#   Plot the combined pairwise R-hat graph.
#
# We test:
#   1. The plotting function runs without error.
#   2. The default circle layout works.
#   3. A graph with no edges can still be plotted.


test_that("plot_mcmc_graph runs without error", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.2,
    plot = FALSE
  )$combined_graph

  expect_no_error(
    plot_mcmc_graph(graph)
  )
})


test_that("plot_mcmc_graph works with the intersection circle layout", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.2,
    plot = FALSE
  )

  expect_no_error(
    plot_mcmc_graph(
      result$combined_graph,
      graph_intersection = result$combined_graph_intersection
    )
  )
})


test_that("plot_mcmc_graph works for a graph with no edges", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 0.5,
    plot = FALSE
  )$combined_graph

  expect_equal(igraph::ecount(graph), 0)

  expect_no_error(
    plot_mcmc_graph(graph)
  )
})
