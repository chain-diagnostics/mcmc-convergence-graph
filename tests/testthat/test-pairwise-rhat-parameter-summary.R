# Tests for pairwise_rhat_parameter_summary().
#
# Main purpose:
#   Produce the main parameter-level summary output for the pairwise R-hat diagnostic.
#
# Package workflow:
#   3D draws array
#     -> choose one or more parameters
#     -> compute pairwise R-hat matrices
#     -> build graphs
#     -> summarise clusters and isolated chains
#     -> return summary tables and supporting objects
#
# We test:
#   1. The output is a list.
#   2. The main expected components are returned.
#   3. The summary table has one row per selected parameter.
#   4. The pairwise display table is capped by pairwise_display_n.
#   5. The full pairwise table is still kept.
#   6. CSV files are saved when save_csv = TRUE.


test_that("pairwise_rhat_parameter_summary returns a list", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE
  )

  expect_type(result, "list")
})


test_that("pairwise_rhat_parameter_summary returns expected components", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE
  )

  expect_true("summary" %in% names(result))
  expect_true("pairwise_values_display" %in% names(result))
  expect_true("pairwise_values" %in% names(result))
  expect_true("clusters" %in% names(result))
  expect_true("graphs" %in% names(result))
  expect_true("combined_graph" %in% names(result))
  expect_true("rhat_matrices" %in% names(result))
  expect_true("rho" %in% names(result))
})


test_that("pairwise_rhat_parameter_summary returns an object of class pairwiserhat", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE
  )

  expect_s3_class(result, "pairwiserhat")
})


test_that("print(pairwiserhat) shows the three requested sections in order", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE,
    pairwise_display_n = 5
  )

  printed <- utils::capture.output(print(result))
  combined <- paste(printed, collapse = "\n")

  expect_match(combined, "1\\. Pairwise R-hat matrices")
  expect_match(combined, "2\\. Top 5 largest pairwise R-hat values")
  expect_match(combined, "3\\. Parameter summary")

  section_positions <- c(
    regexpr("1\\. Pairwise R-hat matrices", combined),
    regexpr("2\\. Top 5 largest pairwise R-hat values", combined),
    regexpr("3\\. Parameter summary", combined)
  )

  expect_true(all(diff(section_positions) > 0))

  expect_false(grepl("combined_graph", combined))
  expect_false(grepl("IGRAPH", combined))
  expect_false(grepl("\\$membership", combined))
})


test_that("pairwise_rhat_parameter_summary returns an igraph combined_graph", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE
  )

  expect_s3_class(result$combined_graph, "igraph")
})


test_that("summary table has one row per selected parameter", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = FALSE
  )

  expect_equal(nrow(result$summary), 2)
  expect_equal(result$summary$parameter, c("alpha", "beta"))
})


test_that("pairwise display table is capped by pairwise_display_n", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = "alpha",
    rho = 1.015,
    save_csv = FALSE,
    pairwise_display_n = 3
  )

  expect_true(nrow(result$pairwise_values_display) <= 3)
})


test_that("full pairwise values table is kept", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = "alpha",
    rho = 1.015,
    save_csv = FALSE,
    pairwise_display_n = 3
  )

  # For 4 chains and 1 parameter, there are 6 unique chain pairs.
  expect_equal(nrow(result$pairwise_values), 6)
})


test_that("clean multimodality keeps pairwise_rhat_value low", {
  set.seed(1)

  draws <- make_separated_draws(n_iter = 200)

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = "alpha",
    rho = 1.015,
    save_csv = FALSE
  )

  expect_equal(result$summary$n_clusters, 2)
  expect_equal(result$summary$n_isolated, 0)
  expect_true(is.finite(result$summary$pairwise_rhat_value))
  expect_lt(result$summary$pairwise_rhat_value, 1.05)

  # Cross-mode pairs are large, but excluded from pairwise_rhat_value.
  expect_gt(max(result$pairwise_values$pairwise_rhat), 1.2)
  expect_lt(
    result$summary$pairwise_rhat_value,
    max(result$pairwise_values$pairwise_rhat)
  )
})


test_that("isolated chains raise pairwise_rhat_value", {
  set.seed(1)

  draws <- make_isolated_draws(n_iter = 200)

  result <- pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = "alpha",
    rho = 1.015,
    save_csv = FALSE
  )

  expect_equal(result$summary$n_clusters, 1)
  expect_equal(result$summary$n_isolated, 1)
  expect_true(is.finite(result$summary$pairwise_rhat_value))
  expect_gt(result$summary$pairwise_rhat_value, 1.2)

  # The reported value matches the largest pair that involves the isolate
  # or lies inside the multi-chain cluster (here: any isolate pair).
  isolate_pairs <- result$pairwise_values[
    result$pairwise_values$chain_i == "4" |
      result$pairwise_values$chain_j == "4",
  ]
  expect_equal(
    result$summary$pairwise_rhat_value,
    max(isolate_pairs$pairwise_rhat)
  )
})


test_that("CSV files are saved when save_csv is TRUE", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  output_dir <- tempfile("pairwise_summary_test_")
  dir.create(output_dir)

  pairwise_rhat_parameter_summary(
    draws = draws,
    parameters = c("alpha", "beta"),
    rho = 1.015,
    save_csv = TRUE,
    output_dir = output_dir,
    pairwise_display_n = 3
  )

  expect_true(
    file.exists(
      file.path(output_dir, "pairwise_rhat_parameter_summary.csv")
    )
  )

  expect_true(
    file.exists(
      file.path(output_dir, "pairwise_rhat_values.csv")
    )
  )

  expect_false(
    file.exists(
      file.path(output_dir, "pairwise_rhat_values_display.csv")
    )
  )

  expect_true(
    file.exists(
      file.path(output_dir, "pairwise_rhat_matrix_alpha.csv")
    )
  )

  expect_true(
    file.exists(
      file.path(output_dir, "pairwise_rhat_matrix_beta.csv")
    )
  )
})
