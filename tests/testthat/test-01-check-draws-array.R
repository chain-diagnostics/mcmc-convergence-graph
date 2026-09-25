# Main purpose:
#   Check whether an object is a valid 3D draws array.

# Expected valid format:
#   iterations x chains x parameters
#
# We test:
#   1. Valid 3D draws arrays are accepted.
#   2. Non-array inputs are rejected.
#   3. 2D matrices are rejected.
#   4. Arrays with too few iterations, chains, or parameters are rejected.

test_that("check_draws_array accepts a valid 3D draws arra", {
  draws <- make_test_draws()
  result <- check_draws_array(draws)
  expect_null(result)
})

test_that("check_draws_array rejects a numeric vector", {
  x <- rnorm(100)
  expect_error(
    check_draws_array(x),
    "3-dimensional"
  )
})

test_that("check_draws_array rejects a 2D matrix", {
  x <- matrix(
    rnorm(100),
    nrow = 10,
    ncol = 10
  )
  expect_error(
    check_draws_array(x),
    "3-dimensional"
  )
})

test_that("check_draws_array rejects arrays with fewer than 2 iterations", {
  draws <- array(
    rnorm(1 * 4 * 2),
    dim = c(1, 4, 2)
  )
  expect_error(
    check_draws_array(draws),
    "at least 2 iterations"
  )
})

test_that("check_draws_array rejects arrays with fewer than 2 chains", {
  draws <- array(
    rnorm(100 * 1 * 2),
    dim = c(100, 1, 2)
  )
  expect_error(
    check_draws_array(draws),
    "at least 2 chains"
  )
})

test_that("check_draws_array rejects arrays with fewer than 1 parameter", {
  draws <- array(
    numeric(100 * 4 * 0),
    dim = c(100, 4, 0)
  )

  expect_error(
    check_draws_array(draws),
    "at least 1 parameter"
  )
})
