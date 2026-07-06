# Helper functions for testthat tests.
#
# These functions create small fake MCMC draws objects.
# They are used only inside tests, not by package users.
#
# Main data format used in this package:
#   iterations x chains x parameters
#
# Example:
#   100 iterations, 4 chains, 2 parameters
#   dim(draws) = c(100, 4, 2)


make_test_draws <- function(
    n_iter = 100,
    n_chains = 4,
    parameters = c("alpha", "beta")
) {

  draws <- array(
    rnorm(n_iter * n_chains * length(parameters)),
    dim = c(n_iter, n_chains, length(parameters))
  )

  dimnames(draws) <- list(
    NULL,
    paste0("chain", seq_len(n_chains)),
    parameters
  )

  draws
}


make_separated_draws <- function(
    n_iter = 100
) {

  draws <- array(
    NA_real_,
    dim = c(n_iter, 4, 1)
  )

  dimnames(draws) <- list(
    NULL,
    paste0("chain", 1:4),
    "alpha"
  )

  draws[, "chain1", "alpha"] <- rnorm(n_iter, mean = 0)
  draws[, "chain2", "alpha"] <- rnorm(n_iter, mean = 0)
  draws[, "chain3", "alpha"] <- rnorm(n_iter, mean = 5)
  draws[, "chain4", "alpha"] <- rnorm(n_iter, mean = 5)

  draws
}


get_test_chain <- function(
    draws,
    chain = "chain1",
    parameter = "alpha"
) {

  draws[, chain, parameter]
}


make_test_split_draws <- function(
    draws = make_test_draws(n_iter = 100, n_chains = 2, parameters = "alpha"),
    parameter = "alpha",
    chain_1 = "chain1",
    chain_2 = "chain2"
) {
  split_1 <- split_chain(draws[, chain_1, parameter])
  split_2 <- split_chain(draws[, chain_2, parameter])

  split_draws <- cbind(
    split_1$first,
    split_1$second,
    split_2$first,
    split_2$second
  )

  colnames(split_draws) <- c(
    paste0(chain_1, "_first"),
    paste0(chain_1, "_second"),
    paste0(chain_2, "_first"),
    paste0(chain_2, "_second")
  )

  split_draws
}
