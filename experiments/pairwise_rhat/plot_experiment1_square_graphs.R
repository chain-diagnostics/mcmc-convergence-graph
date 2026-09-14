# Experiment 1: four square graph PDFs in one row of the paper.
# Intersection graph only. Title is the experiment name. No legend.

library(rstan)
library(mcmcConvergenceGraph)

source(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments/pairwise_rhat/save_square_graph.R"
)

base_fit <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
base_out <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat"
common_dir <- file.path(base_out, "experiment1_square_graphs")
if (!dir.exists(common_dir)) dir.create(common_dir, recursive = TRUE)

experiments <- list(
  list(
    key   = "uniform",
    title = "Uniform",
    file  = "uniform.pdf"
  ),
  list(
    key   = "unimodal_gaussian",
    title = "Gaussian ball",
    file  = "gaussian_ball.pdf"
  ),
  list(
    key   = "anisotropic_gaussian",
    title = "Anisotropic Gaussian",
    file  = "anisotropic_gaussian.pdf"
  ),
  list(
    key   = "multimodal_gaussian",
    title = "Multimodal Gaussian",
    file  = "multimodal_gaussian.pdf"
  )
)

for (experiment in experiments) {
  fit_path <- file.path(base_fit, experiment$key, paste0(experiment$key, ".rds"))
  out_dir  <- file.path(base_out, experiment$key)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  fit <- readRDS(fit_path)

  result <- mcmcgraph(
    fit,
    parameters         = c("x[1]", "x[2]"),
    rho                = 1.05,
    plot               = FALSE,
    save_csv           = TRUE,
    save_plot          = FALSE,
    output_dir         = out_dir,
    pairwise_display_n = 10
  )

  square_path <- file.path(out_dir, "square_graph.pdf")
  common_path <- file.path(common_dir, experiment$file)

  save_square_graph(
    graph    = result$combined_graph_intersection,
    title    = experiment$title,
    pdf_path = square_path
  )
  file.copy(square_path, common_path, overwrite = TRUE)

  cat("wrote", square_path, "\n")
  cat("wrote", common_path, "\n")
}
