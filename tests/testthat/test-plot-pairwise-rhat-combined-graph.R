# Tests for plot_mcmc_graph().
#
# Main purpose:
#   Plot the combined pairwise R-hat graph.
#
# We test:
#   1. The plotting function runs without error.
#   2. The default layout works.
#   3. Another layout option works.
#   4. A graph with no edges can still be plotted.


test_that("plot_mcmc_graph runs without error", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- mcmc_graph_multi(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.2
  )

  expect_no_error(
    plot_mcmc_graph(graph)
  )
})


test_that("plot_mcmc_graph works with grid layout", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- mcmc_graph_multi(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.2
  )

  expect_no_error(
    plot_mcmc_graph(
      graph,
      layout_type = "grid"
    )
  )
})


test_that("plot_mcmc_graph works with circle layout", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  graph <- mcmc_graph_multi(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.2
  )

  expect_no_error(
    plot_mcmc_graph(
      graph,
      layout_type = "circle"
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

  graph <- mcmc_graph_multi(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 0.5
  )

  expect_equal(igraph::ecount(graph), 0)

  expect_no_error(
    plot_mcmc_graph(graph)
  )
})
