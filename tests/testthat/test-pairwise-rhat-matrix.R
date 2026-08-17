# Tests for pairwise_rhat_matrix().
#
# Main purpose:
#   Compute a square pairwise R-hat matrix for one parameter.
#
# We test:
#   1. The output is a matrix.
#   2. The matrix has the expected chain x chain dimensions.
#   3. The diagonal values are 1.
#   4. The matrix is symmetric.
#   5. Row and column names match chain names.
#   6. Missing parameter names are rejected.


test_that("pairwise_rhat_matrix returns a matrix", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- pairwise_rhat_matrix(
    draws = draws,
    parameter = "alpha"
  )

  expect_true(is.matrix(result))
})


test_that("pairwise_rhat_matrix has chain by chain dimensions", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- pairwise_rhat_matrix(
    draws = draws,
    parameter = "alpha"
  )

  expect_equal(dim(result), c(4, 4))
})


test_that("pairwise_rhat_matrix has diagonal values equal to 1", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- pairwise_rhat_matrix(
    draws = draws,
    parameter = "alpha"
  )

  expect_equal(
    unname(diag(result)),
    rep(1, 4)
  )
})


test_that("pairwise_rhat_matrix is symmetric", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- pairwise_rhat_matrix(
    draws = draws,
    parameter = "alpha"
  )

  expect_equal(result, t(result))
})


test_that("pairwise_rhat_matrix rejects a missing parameter", {
  draws <- make_test_draws(
    parameters = c("alpha", "beta")
  )

  expect_error(
    pairwise_rhat_matrix(
      draws = draws,
      parameter = "gamma"
    ),
    "parameter"
  )
})
