# Tests for get_chain_pairs().
#
# Main purpose:
#   Extract all unique chain pairs for one parameter from a 3D draws array.
#
# For 4 chains, the expected unique pairs are:
#   1-2, 1-3, 1-4, 2-3, 2-4, 3-4
#
# We test:
#   1. The correct number of chain pairs is returned.
#   2. Each pair has the expected list structure.
#   3. Chain pair indices are in the expected order.
#   4. The extracted chain draws match the original draws array.
#   5. Missing parameter names are rejected.

test_that("get_chain_pairs returns all unique chain pairs for one parameter", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
  )

  result <- get_chain_pairs(
    draws = draws,
    parameter = "alpha"
  )

  expect_type(result, "list")
  expect_length(result, 6)
})

test_that("each chain pair contains chain indices and chain draws", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- get_chain_pairs(
    draws = draws,
    parameter = "alpha"
  )

  first_pair <- result[[1]]

  expect_named(
    first_pair,
    c(
      "chain_i_index",
      "chain_j_index",
      "chain_i_draws",
      "chain_j_draws"
    )
  )

  expect_equal(first_pair$chain_i_index, 1)
  expect_equal(first_pair$chain_j_index, 2)

  expect_type(first_pair$chain_i_draws, "double")
  expect_type(first_pair$chain_j_draws, "double")

  expect_length(first_pair$chain_i_draws, 100)
  expect_length(first_pair$chain_j_draws, 100)
})

test_that("get_chain_pairs returns chain pairs in the expected order", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- get_chain_pairs(
    draws = draws,
    parameter = "alpha"
  )

  pair_indices <- vapply(
    result,
    function(x) {
      paste0(x$chain_i_index, "-", x$chain_j_index)
    },
    character(1)
  )

  expect_equal(
    pair_indices,
    c(
      "1-2",
      "1-3",
      "1-4",
      "2-3",
      "2-4",
      "3-4"
    )
  )
})

test_that("get_chain_pairs extracts the correct chain draws", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  result <- get_chain_pairs(
    draws = draws,
    parameter = "alpha"
  )

  first_pair <- result[[1]]

  expect_equal(
    first_pair$chain_i_draws,
    draws[, "chain1", "alpha"]
  )

  expect_equal(
    first_pair$chain_j_draws,
    draws[, "chain2", "alpha"]
  )
})

test_that("get_chain_pair reject a missing parameter", {
  draws <- make_test_draws(
    parameters = "alpha"
  )
  expect_error(
    get_chain_pairs(
      draws = draws,
      parameter = "gamma"
    ),
    "must be one of the parameter names"
  )
})
