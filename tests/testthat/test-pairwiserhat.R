# Tests for mcmcgraph().
#
# Main purpose:
#   End-to-end wrapper that computes pairwise R-hat matrices, per-parameter
#   graphs, cluster summaries, the combined graph, the parameter summary, and
#   (by default) draws the combined graph.
#
# We test:
#   1. The output is a list with all documented components.
#   2. The combined_graph component is an igraph object.
#   3. plot = TRUE runs without error (drawing into a temporary PDF device).
#   4. A stanfit-like object is accepted through rstan::extract (mocked; the
#      test is skipped when rstan is not installed).


test_that("mcmcgraph returns a list with all expected components", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    plot = FALSE
  )

  expect_type(result, "list")

  expect_true("summary" %in% names(result))
  expect_true("pairwise_values_display" %in% names(result))
  expect_true("pairwise_values" %in% names(result))
  expect_true("clusters" %in% names(result))
  expect_true("graphs" %in% names(result))
  expect_true("combined_graph" %in% names(result))
  expect_true("combined_graph_intersection" %in% names(result))
  expect_true("combined_graph_difference" %in% names(result))
  expect_true("intersection_summary" %in% names(result))
  expect_true("rhat_matrices" %in% names(result))
  expect_true("rho" %in% names(result))
})


test_that("mcmcgraph returns an igraph combined_graph", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    plot = FALSE
  )

  expect_s3_class(result$combined_graph, "igraph")
})


test_that("mcmcgraph plots when plot = TRUE", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    plot = TRUE
  )

  expect_s3_class(result$combined_graph, "igraph")
})


test_that("mcmcgraph writes combined_graph.pdf when save_plot = TRUE", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  output_dir <- tempfile("pairwiserhat_save_plot_")
  dir.create(output_dir)

  mcmcgraph(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    plot = FALSE,
    save_csv = FALSE,
    save_plot = TRUE,
    output_dir = output_dir
  )

  pdf_path <- file.path(output_dir, "combined_graph.pdf")

  expect_true(file.exists(pdf_path))
  expect_gt(file.info(pdf_path)$size, 0)
})


test_that("mcmcgraph errors when save_plot = TRUE without output_dir", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  expect_error(
    mcmcgraph(
      draws = draws,
      parameters = c("alpha", "beta"),
      rho = 1.015,
      plot = FALSE,
      save_plot = TRUE,
      output_dir = NULL
    ),
    "output_dir"
  )
})


test_that("mcmcgraph accepts a stanfit-like object via rstan::extract", {
  testthat::skip_if_not_installed("rstan")

  fake_fit <- structure(list(), class = "stanfit")

  fake_draws <- array(
    rnorm(100 * 4 * 2),
    dim = c(100, 4, 2),
    dimnames = list(
      NULL,
      paste0("chain", 1:4),
      c("alpha", "beta")
    )
  )

  testthat::local_mocked_bindings(
    extract = function(...) fake_draws,
    .package = "rstan"
  )

  result <- mcmcgraph(fake_fit, plot = FALSE)

  expect_type(result, "list")
  expect_equal(names(result$rhat_matrices), c("alpha", "beta"))
})
