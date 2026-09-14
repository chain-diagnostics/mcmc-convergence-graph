# Four separate square PDFs for Overleaf, each with its panel title.

library(rstan)
if (requireNamespace("mcmcConvergenceGraph", quietly = TRUE)) {
  library(mcmcConvergenceGraph)
} else {
  pkgload::load_all("/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph", quiet = TRUE)
}

source(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments/pairwise_rhat/save_square_graph.R"
)

base_fit <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
docs_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs"
out_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/experiment1_square_graphs"

experiments <- list(
  list(key = "uniform",              file = "fig_experiment1_a_uniform.pdf",          title = "Uniform"),
  list(key = "unimodal_gaussian",    file = "fig_experiment1_b_gaussian_ball.pdf",    title = "Gaussian ball"),
  list(key = "anisotropic_gaussian", file = "fig_experiment1_c_anisotropic.pdf",      title = "Anisotropic Gaussian"),
  list(key = "multimodal_gaussian",  file = "fig_experiment1_d_multimodal.pdf",       title = "Multimodal Gaussian")
)

for (experiment in experiments) {
  fit <- readRDS(
    file.path(base_fit, experiment$key, paste0(experiment$key, ".rds"))
  )
  result <- mcmcgraph(
    fit,
    parameters = c("x[1]", "x[2]"),
    rho        = 1.05,
    plot       = FALSE
  )
  pdf_path <- file.path(docs_dir, experiment$file)
  save_square_graph(
    graph    = result$combined_graph_intersection,
    title    = experiment$title,
    pdf_path = pdf_path
  )
  file.copy(pdf_path, file.path(out_dir, experiment$file), overwrite = TRUE)
  cat("wrote", pdf_path, "\n")
}
