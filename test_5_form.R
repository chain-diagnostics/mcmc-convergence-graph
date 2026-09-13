library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

output_dir <-
  "/Users/chegu121/Documents/Phd-cici/r_package/result_5_form/cluster"

############################################################
## Fit Stan model
############################################################

fit <- stan(
  file = "/Users/chegu121/Documents/Phd-cici/pairwise r hat/stan_file/cluster.stan",
  data = list(d = 2L),
  chains = 6,
  iter = 2000,
  warmup = 1000,
  seed = 1234
)

############################################################
## Convert to standard draws array
############################################################

draws <- as_draws_array(fit)

############################################################
## Pairwise R-hat summary
############################################################

result <- mcmc_graph_summary(
  draws = draws,
  rho = 1.015,
  save_csv = TRUE,
  output_dir = output_dir
)

############################################################
## Combined graph
############################################################

combined_graph <- mcmc_graph_multi(
  draws = draws,
  rho = 1.015
)

pdf(
  file.path(output_dir, "combined_mcmc_graph_uni.pdf"),
  width = 8,
  height = 8
)

plot_mcmc_graph(
  combined_graph,
  layout_type = "fr",
  show_edge_labels = TRUE,
  show_legend = TRUE
)

dev.off()

############################################################
## Print results
############################################################

cat("\n=========================================\n")
cat("Parameter summary\n")
cat("=========================================\n\n")
print(result$summary)

cat("\n=========================================\n")
cat("Largest pairwise R-hat values\n")
cat("=========================================\n\n")
print(result$pairwise_values_display)

cat("\n=========================================\n")
cat("All pairwise R-hat values\n")
cat("=========================================\n\n")
print(result$pairwise_values)

cat("\n=========================================\n")
cat("Cluster results\n")
cat("=========================================\n\n")
print(result$clusters)

cat("\n=========================================\n")
cat("Pairwise R-hat matrices\n")
cat("=========================================\n\n")

for (parameter in names(result$rhat_matrices)) {
  cat("\n", parameter, "\n", sep = "")
  print(result$rhat_matrices[[parameter]])
}

cat("\n=========================================\n")
cat("Combined graph saved to:\n")
cat(file.path(output_dir, "combined_mcmc_graph_uni.pdf"))
cat("\n=========================================\n")
