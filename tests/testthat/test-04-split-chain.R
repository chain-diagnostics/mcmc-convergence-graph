# Tests for split_chain().
#
# Main purpose:
#   Split one MCMC chain into two equal-length halves.
#
# We test:
#   1. Even-length chains are split exactly in half.
#   2. Odd-length chains drop the middle draw before splitting.
#   3. The function works on a chain extracted from test draws.


test_that("split_chain splits an even-length chain into two equal halves", {
  chain <- 1:10

  result <- split_chain(chain)

  expect_named(result, c("first", "second"))
  expect_equal(result$first, 1:5)
  expect_equal(result$second, 6:10)
})


test_that("split_chain drops the middle draw for an odd-length chain", {
  chain <- 1:11

  result <- split_chain(chain)

  expect_equal(result$first, 1:5)
  expect_equal(result$second, 7:11)
})


test_that("split_chain works on a chain extracted from test draws", {
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

  result <- split_chain(chain)

  expect_length(result$first, 50)
  expect_length(result$second, 50)
})
