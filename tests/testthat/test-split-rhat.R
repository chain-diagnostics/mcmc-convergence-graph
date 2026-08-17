# Tests for split_rhat().
#
# Main purpose:
#   Compute split R-hat from a matrix of split-chain draws.
#
# We test:
#   1. Valid split-chain input returns one value.
#   2. Similar chains give split R-hat close to 1.
#   3. The function works with split draws created from helper draws.


test_that("split_rhat returns one value for valid split-chain draws", {
  set.seed(1)

  split_draws <- make_test_split_draws()

  result <- split_rhat(split_draws)

  expect_length(result, 1)
})


test_that("split_rhat is close to 1 for similar chains", {
  set.seed(1)

  split_draws <- make_test_split_draws()

  result <- split_rhat(split_draws)

  expect_true(result > 0.9)
  expect_true(result < 1.2)
})


test_that("split_rhat works with split draws created from helper draws", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  split_draws <- make_test_split_draws(
    draws = draws,
    parameter = "alpha",
    chain_1 = "chain1",
    chain_2 = "chain2"
  )

  result <- split_rhat(split_draws)

  expect_length(result, 1)
})
