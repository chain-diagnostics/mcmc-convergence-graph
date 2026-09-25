# Tests for rank_normalize().
#
# Main purpose:
#   Convert one numeric vector of MCMC draws into rank-normalized values.
#
# We test:
#   1. The output has the same length as the input chain.
#   2. The function works with tied values.
#   3. The function works on a chain extracted from test draws.


test_that("rank_normalize returns a vector with the same length", {
  chain <- rnorm(100)

  result <- rank_normalize(chain)

  expect_length(result, length(chain))
})


test_that("rank_normalize works with tied values", {
  chain <- c(1, 1, 2, 2, 3, 3)

  result <- rank_normalize(chain)

  expect_length(result, length(chain))
})


test_that("rank_normalize works on a chain extracted from test draws", {
  draws <- make_test_draws(
    n_iter = 100,
    n_chains = 4,
    parameters = "alpha"
  )

  chain <- get_test_chain(
    draws = draws,
    chain = "chain1",
    parameter = "alpha"
  )

  result <- rank_normalize(chain)

  expect_length(result, 100)
})
