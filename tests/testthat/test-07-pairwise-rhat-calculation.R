# Tests for pairwise_rhat_calculation().
#
# Main purpose:
#   Compute pairwise R-hat between two MCMC chains.
#
# We test:
#   1. Valid chain input returns one value.
#   2. Similar chains give pairwise R-hat close to 1.
#   3. Clearly separated chains give larger pairwise R-hat.


test_that("pairwise_rhat_calculation returns one value for two valid chains", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  chain_1 <- get_test_chain(draws, "chain1", "alpha")
  chain_2 <- get_test_chain(draws, "chain2", "alpha")

  result <- pairwise_rhat_calculation(chain_1, chain_2)

  expect_length(result, 1)
})


test_that("pairwise_rhat_calculation is close to 1 for two similar chains", {
  set.seed(1)

  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  chain_1 <- get_test_chain(draws, "chain1", "alpha")
  chain_2 <- get_test_chain(draws, "chain2", "alpha")

  result <- pairwise_rhat_calculation(chain_1, chain_2)

  expect_true(result > 0.9)
  expect_true(result < 1.2)
})


test_that("pairwise_rhat_calculation is larger for two clearly separated chains", {
  set.seed(1)

  draws <- make_separated_draws(
    n_iter = 100
  )

  chain_1 <- get_test_chain(draws, "chain1", "alpha")
  chain_3 <- get_test_chain(draws, "chain3", "alpha")

  result <- pairwise_rhat_calculation(chain_1, chain_3)

  expect_true(result > 1.1)
})
