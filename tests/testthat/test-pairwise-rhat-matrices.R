# Tests for pairwise_rhat_matrices().
#
# Main purpose:
#   Compute pairwise R-hat matrices for one or more parameters.
#
# We test:
#   1. If parameters = NULL, all parameters are used.
#   2. Missing parameter names are rejected.


test_that("pairwise_rhat_matrices uses all parameters when parameters is NULL", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta", "theta")
  )

  result <- pairwise_rhat_matrices(
    draws = draws,
    parameters = NULL
  )

  expect_length(result, 3)
  expect_equal(names(result), c("alpha", "beta", "theta"))
})


test_that("pairwise_rhat_matrices rejects missing parameter names", {
  draws <- make_test_draws(
    parameters = c("alpha", "beta")
  )

  expect_error(
    pairwise_rhat_matrices(
      draws = draws,
      parameters = c("alpha", "gamma")
    ),
    "parameter"
  )
})
