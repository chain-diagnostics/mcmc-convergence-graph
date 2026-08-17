library(rstan)
library(pairwiserhat)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_path   <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk/three_compartment_pk.rds"
output_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/three_compartment_pk"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fit <- readRDS(fit_path)

rate_params <- c("k10", "k12", "k21", "k13", "k31")

result <- pairwiserhat(
  fit,
  parameters                 = rate_params,
  rho                        = 1.015,
  plot                       = FALSE,
  save_csv                   = TRUE,
  save_plot                  = TRUE,
  output_dir                 = output_dir,
  pairwise_display_n         = 10,
  posterior_draws_parameters = c("k12", "k13")
)

print(result)

pair_order <- list(
  c("k12", "k13"),
  c("k21", "k31"),
  c("k12", "k21"),
  c("k13", "k31"),
  c("k12", "k31"),
  c("k13", "k21"),
  c("k10", "k12"),
  c("k10", "k13"),
  c("k10", "k21"),
  c("k10", "k31")
)

grDevices::pdf(
  file    = file.path(output_dir, "posterior_draws_all_pairs.pdf"),
  width   = 8,
  height  = 6,
  onefile = TRUE
)

for (pair in pair_order) {
  p <- plot_posterior_draws(fit, parameters = pair)
  print(p)
}

grDevices::dev.off()

cat("wrote posterior_draws_all_pairs.pdf with", length(pair_order), "pages\n")
