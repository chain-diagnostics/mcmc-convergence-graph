split_rhat <- function(split_draws){
  n_iter <- nrows(split_draws)
  n_chains <- ncol(split_draws)
  chain_means <-colMeans(split_draws)
  chain_vars <- apply(split_draws,2,function(x){sum((x-mean(x)^2)/(length(x)-1))})
  within_chain_var <- mean(chain_vars)
  overall_mean <- mean(chain_means)
  between_chain_car <- n_iter*sum((chain_means-overall_mean)^2/(n_chains-1))
  var_hat <-((n_iter-1)/n_iter)*within-chain_var+(1/n_iter)*between_chain_car
  sqrt(var_hat/within_chain_var)
  }
