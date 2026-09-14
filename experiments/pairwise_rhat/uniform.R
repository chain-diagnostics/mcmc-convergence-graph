library(rstan)
library(mcmcConvergenceGraph)

source(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments/pairwise_rhat/save_square_graph.R"
)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/uniform/uniform.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/uniform"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

result <- mcmcgraph(
  fit,
  parameters         = c("x[1]", "x[2]"),
  rho                = 1.05,
  plot               = FALSE,
  save_csv           = TRUE,
  save_plot          = FALSE,
  output_dir         = output_dir,
  pairwise_display_n = 10
)

save_square_graph(
  graph    = result$combined_graph_intersection,
  title    = "Uniform",
  pdf_path = file.path(output_dir, "square_graph.pdf")
)

print(result)
