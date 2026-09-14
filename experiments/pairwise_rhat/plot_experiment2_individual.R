# Three separate square PDFs for Overleaf, same format as Experiment 1.

library(rstan)
if (requireNamespace("mcmcConvergenceGraph", quietly = TRUE)) {
  library(mcmcConvergenceGraph)
} else {
  pkgload::load_all("/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph", quiet = TRUE)
}
library(bayesplot)

source(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments/pairwise_rhat/save_square_graph.R"
)

base_fit <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
docs_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs"
out_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/experiment2_square_graphs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

experiments <- list(
  list(
    key   = "diff_slope_same_intercept",
    file  = "fig_experiment2_a_different_slopes.pdf",
    title = "Different slopes"
  ),
  list(
    key   = "same_slope_diff_intercept",
    file  = "fig_experiment2_b_different_intercepts.pdf",
    title = "Different intercepts"
  ),
  list(
    key   = "diff_slope_diff_intercept",
    file  = "fig_experiment2_c_different_slopes_and_intercepts.pdf",
    title = "Different slopes\nand intercepts"
  )
)

for (experiment in experiments) {
  fit <- readRDS(
    file.path(base_fit, experiment$key, paste0(experiment$key, ".rds"))
  )
  result <- mcmcgraph(
    fit,
    parameters = c("a", "b", "gamma"),
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

fit <- readRDS(
  file.path(base_fit, "diff_slope_diff_intercept", "diff_slope_diff_intercept.rds")
)
posterior_file <- "fig_experiment2_d_posterior_a_b.pdf"
posterior_path <- file.path(docs_dir, posterior_file)
grDevices::pdf(posterior_path, width = 5, height = 5)
print(mcmc_scatter(fit, pars = c("a", "b")))
grDevices::dev.off()
file.copy(posterior_path, file.path(out_dir, posterior_file), overwrite = TRUE)
cat("wrote", posterior_path, "\n")
