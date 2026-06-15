#create a function called split-chain, and the function take one input called x
# x is one chain of MCMC draws for one parameter
#counts how many values in x, the number of draws in this chain (parameter value)
#floor : rounds down
#draw =sampled value



split_chain <- function(x) {
    if (!is.numeric(x)) {
    stop("'x' must be a numberic vector.", call.= FALSE)
    }
  n <- length(x)
  if (n<2) {
    stop("'x' must contain at least 2 draws.", call. =FALSE)
  }
  half_n <- floor (n/2)
  list(
    first = x [seq_len(half_n)],
    second = x [(half_n+1):(2*half_n)]
  )
}
