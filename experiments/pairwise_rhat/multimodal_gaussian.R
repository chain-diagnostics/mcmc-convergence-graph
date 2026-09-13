library(rstan)
library(mcmcConvergenceGraph)
library(bayesplot)

library(ggplot2)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path  <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/multimodal_gaussian/multimodal_gaussian.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/multimodal_gaussian"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- mcmcgraph(
  fit,
  parameters         = c("x[1]", "x[2]"),
  rho                = 1.05,
  plot               = TRUE,
  save_csv           = TRUE,
  save_plot          = TRUE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)

print(result)
posterior <- as.matrix(
  fit,
)

posterior_plot <- mcmc_areas(posterior, prob = 0.8) +
  ggtitle("Posterior distributions", "Medians and 80% intervals")

print(posterior_plot)
ggsave(
  filename = file.path(output_dir, "posterior_areas.pdf"),
  plot = posterior_plot,
  width = 8,
  height = 6
)

