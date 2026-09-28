library(rstan)
library(bayesplot)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_file  <- "Pharmcokinetic.stan"
data_file  <- "PK_reminfentanil.csv"
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

dat  <- read.csv(data_file)
dose <- dat[dat$DV == ".", ]
obs  <- dat[dat$DV != ".", ]

n_obs <- as.integer(table(obs$ID))
rate  <- as.numeric(dose$RATE)
tinf  <- as.numeric(dose$TINFCAT)

stan_data <- list(
  N_subj = nrow(dose),
  N_obs = nrow(obs),
  n_obs = n_obs,
  start = cumsum(c(1L, head(n_obs, -1L))),
  time_obs = obs$TIME,
  conc_obs = as.numeric(obs$DV),
  rate = rate,
  tinf = tinf,
  x_r_ode = cbind(rate, tinf)
)

init_fn <- function() {
  list(
    k10        = runif(1, 0, 2),
    k12        = runif(1, 0, 2),
    k21        = runif(1, 0, 2),
    k13        = runif(1, 0, 2),
    k31        = runif(1, 0, 2),
    VC         = runif(1, 0, 10),
    sigma_add  = runif(1, 0, 1),
    sigma_prop = runif(1, 0, 1)
  )
}

set.seed(123)
fit <- stan(
  file    = stan_file,
  data    = stan_data,
  chains  = 8,
  iter    = 2000,
  warmup  = 1000,
  seed    = 123,
  init    = init_fn
)

summary_matrix <- rstan::summary(fit)$summary
write.csv(
  summary_matrix,
  file = file.path(output_dir, "classical_rhat_summary.csv"),
  row.names = TRUE
)
saveRDS(stan_data, file.path(output_dir, "stan_data.rds"))
saveRDS(fit, file.path(output_dir, "three_compartment_pk_infer_vc.rds"))
print(round(summary_matrix, 4))

posterior <- as.matrix(
  fit,
  pars = c("k10", "k12", "k21", "k13", "k31", "VC", "sigma_add", "sigma_prop")
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
