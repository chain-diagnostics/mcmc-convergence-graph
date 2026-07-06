rm(list = ls())

library(rstan)

# Load your local pairwiserhat package during development.
# If the package is later installed normally, you can replace this with:
# library(pairwiserhat)
devtools::load_all("/Users/chegu121/Documents/Phd-cici/r_package/pairwiserhat")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ============================================================
# Model settings
# ============================================================
posterior_key <- "B"
posterior_name <- "mix_same_b_diff_a"
stan_file <- "/Users/chegu121/Documents/Phd-cici/pairwise r hat/linear_stan_file/linreg_mix_same_b_diff_a.stan"
stan_data <- list(d = 2)

n_chains <- 8
iter_total <- 4000
warmup <- 2000
post_warmup_draws <- iter_total - warmup
rho <- 1.015
seed_value <- 1734

base_result_dir <- "/Users/chegu121/Documents/Phd-cici/pairwise r hat/8chain_final_results/linear"

experiment_name <- paste0(
  posterior_key, "_", posterior_name, "_",
  n_chains, "chain_",
  post_warmup_draws, "_postwarmup"
)

output_dir <- file.path(base_result_dir, experiment_name)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

config_df <- data.frame(
  parameter = c(
    "posterior_key",
    "posterior_name",
    "stan_file",
    "n_chains",
    "iter_total",
    "warmup",
    "post_warmup_draws",
    "rho",
    "seed"
  ),
  value = c(
    posterior_key,
    posterior_name,
    stan_file,
    n_chains,
    iter_total,
    warmup,
    post_warmup_draws,
    rho,
    seed_value
  )
)

utils::write.csv(
  config_df,
  file = file.path(output_dir, "experiment_config.csv"),
  row.names = FALSE
)

cat("\n====================================\n")
cat("Training model:\n")
cat(posterior_key, posterior_name, "\n")
cat("Output directory:\n")
cat(output_dir, "\n")
cat("====================================\n\n")

# ============================================================
# Train Stan model with 8 real chains
# No chain-selection grid, no calibration phase.
# ============================================================
fit <- rstan::stan(
  file = stan_file,
  data = stan_data,
  chains = n_chains,
  iter = iter_total,
  warmup = warmup,
  seed = seed_value,
  refresh = 100
)

saveRDS(
  fit,
  file = file.path(output_dir, "stan_fit_object.rds")
)

# ============================================================
# Choose parameters for pairwise R-hat
# Default: use theta parameters only.
# ============================================================
draws_all <- as_draws_array(fit)

all_parameters <- setdiff(
  dimnames(draws_all)[[3]],
  "lp__"
)

parameters_to_use <- grep(
  "^theta(\\[|$)",
  all_parameters,
  value = TRUE
)

if (length(parameters_to_use) == 0) {
  warning(
    "No parameters named theta found. Using all non-lp__ parameters instead.",
    call. = FALSE
  )

  parameters_to_use <- all_parameters
}

cat("\nParameters used for pairwise R-hat:\n")
print(parameters_to_use)

draws <- as_draws_array(
  fit,
  parameter = parameters_to_use
)

# ============================================================
# Run pairwiserhat package output
# ============================================================
summary_result <- pairwise_rhat_parameter_summary(
  draws = draws,
  parameters = parameters_to_use,
  rho = rho,
  save_csv = TRUE,
  output_dir = output_dir,
  pairwise_display_n = 10
)

rhat_matrices <- summary_result$rhat_matrices

combined_graph <- pairwise_rhat_combined_graph(
  draws = draws,
  parameters = parameters_to_use,
  rho = rho
)

# Save plot as PNG
png(
  filename = file.path(output_dir, "combined_pairwise_rhat_graph.png"),
  width = 1800,
  height = 1000,
  res = 150
)

plot_pairwise_rhat_combined_graph(
  combined_graph,
  layout_type = "grid",
  show_edge_labels = FALSE,
  legend_position = "top",
  vertex_size = 8,
  legend_cex = 0.8,
  edge_curved = 0
)

dev.off()

# Save plot as PDF
pdf(
  file = file.path(output_dir, "combined_pairwise_rhat_graph.pdf"),
  width = 12,
  height = 7
)

plot_pairwise_rhat_combined_graph(
  combined_graph,
  layout_type = "grid",
  show_edge_labels = FALSE,
  legend_position = "top",
  vertex_size = 8,
  legend_cex = 0.8,
  edge_curved = 0
)

dev.off()

saveRDS(
  summary_result,
  file = file.path(output_dir, "pairwise_summary_result.rds")
)

saveRDS(
  combined_graph,
  file = file.path(output_dir, "combined_pairwise_rhat_graph.rds")
)

capture.output(
  print(fit, pars = parameters_to_use),
  file = file.path(output_dir, "stan_summary_selected_parameters.txt")
)

cat("\nParameter-level summary:\n")
print(summary_result$summary)

cat("\nTop 10 pairwise R-hat values:\n")
print(summary_result$pairwise_values_display)

cat("\nCSV and graph outputs saved to:\n")
cat(output_dir, "\n")

cat("\n====================================\n")
cat("Finished:\n")
cat(posterior_key, posterior_name, "\n")
cat("====================================\n")
