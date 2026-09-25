# Tests for as_draws_array().
#
# Main purpose:
#   Convert supported input objects into the package's standard 3D draws array.
#
# We test:
#   1. Valid 3D arrays are accepted and returned unchanged.
#   2. Parameter selection keeps the output as a 3D array.
#   3. Invalid inputs such as vectors, data frames, and matrices are rejected.
#   4. Missing parameter names are rejected.
#
# Note:
#   Tests for rstan::stanfit objects can be added later as optional tests,
#   because they require the rstan package and a fitted Stan model object.


test_that("as_draws_array accepts a valid 3D draws array", {
  draws <- make_test_draws()
  result <- as_draws_array(draws)
  expect_true(is.array(result))
  expect_equal(dim(result), dim(draws))
  expect_equal(dimnames(result), dimnames(draws))
})

test_that("as_draws_array keeps a valid array when one parameter is provided", {
  draws <- make_test_draws(
    parameters = c("alpha", "beta")
  )
  result <- as_draws_array(
    draws,
    parameter = "alpha"
  )
  expect_true(is.array(result))
  expect_equal(dim(result), c(100, 4, 1))
  expect_equal(dimnames(result)[[2]], paste0("chain", 1:4))
  expect_equal(dimnames(result)[[3]], "alpha")
})

test_that("as_draws_array rejects a numeric vector", {
  x <- rnorm(100)
  expect_error(
    as_draws_array(x),
    "3-dimensional"
  )
})

test_that("as_draws_array rejects a data frame", {
  x <- data.frame(
    alpha = rnorm(100),
    beta = rnorm(100)
  )
  expect_error(
    as_draws_array(x),
    "3-dimensional"
  )
})

test_that("as_draws_array rejects a 2D matrix", {
  x <- matrix(
    rnorm(100),
    nrow = 10,
    ncol = 10
  )
  expect_error(
    as_draws_array(x),
    "3-dimensional"
  )
})

test_that("as_draws_array rejects a missing parameter name", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  expect_error(
    as_draws_array(draws, parameter = "gamma"),
    "Unknown parameter"
  )
})


test_that("as_draws_array accepts a stanfit-like object via rstan::extract", {
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

  result <- as_draws_array(fake_fit)

  expect_true(is.array(result))
  expect_equal(dim(result), c(100, 4, 2))
  expect_equal(dimnames(result)[[3]], c("alpha", "beta"))
})
