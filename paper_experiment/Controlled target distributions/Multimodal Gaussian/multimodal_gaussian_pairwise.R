library(rstan)
library(mcmcConvergenceGraph)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "output/multimodal_gaussian.rds"
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- mcmcgraph(
  fit,
  parameters         = c("x[1]", "x[2]"),
  rho                = 1.05,
  plot               = TRUE,
  save_csv           = TRUE,
  save_plot          = FALSE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)


print(result)

