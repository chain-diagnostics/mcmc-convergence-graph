# Per-component mean and 95% CI via rstan::monitor(), for Experiment 1 and 2.
# Chain groups come from the saved pairwise R-hat cluster summaries (rho = 1.05).
# Isolated chains are treated as extra components of size 1.

library(rstan)
library(mcmcConvergenceGraph)

baseline_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
pairwise_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat"
output_dir   <- pairwise_dir

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

experiments <- list(
  list(
    experiment = "experiment_1",
    dir        = "uniform",
    target     = "Uniform",
    parameters = c("x[1]", "x[2]")
  ),
  list(
    experiment = "experiment_1",
    dir        = "unimodal_gaussian",
    target     = "Unimodal Gaussian",
    parameters = c("x[1]", "x[2]")
  ),
  list(
    experiment = "experiment_1",
    dir        = "anisotropic_gaussian",
    target     = "Anisotropic Gaussian",
    parameters = c("x[1]", "x[2]")
  ),
  list(
    experiment = "experiment_1",
    dir        = "multimodal_gaussian",
    target     = "Multimodal Gaussian",
    parameters = c("x[1]", "x[2]")
  ),
  list(
    experiment = "experiment_2",
    dir        = "diff_slope_same_intercept",
    target     = "Different slope, same intercept",
    parameters = c("a", "b")
  ),
  list(
    experiment = "experiment_2",
    dir        = "same_slope_diff_intercept",
    target     = "Different intercept, same slope",
    parameters = c("a", "b")
  ),
  list(
    experiment = "experiment_2",
    dir        = "diff_slope_diff_intercept",
    target     = "Different intercept, different slope",
    parameters = c("a", "b")
  )
)

parse_chain_groups <- function(clusters, isolated_chains) {
  groups <- list()

  if (!is.null(clusters) && !is.na(clusters) && nzchar(clusters)) {
    tokens <- strsplit(clusters, "\\s*\\|\\s*")[[1]]
    for (token in tokens) {
      ids <- as.integer(strsplit(gsub("[() ]", "", token), ",")[[1]])
      ids <- ids[!is.na(ids)]
      if (length(ids) > 0L) {
        groups[[length(groups) + 1L]] <- ids
      }
    }
  }

  isolated_text <- as.character(isolated_chains)
  if (length(isolated_text) == 1L &&
      !is.na(isolated_text) &&
      nzchar(isolated_text) &&
      isolated_text != "NA") {
    iso <- as.integer(strsplit(isolated_text, ",\\s*")[[1]])
    iso <- iso[!is.na(iso)]
    for (id in iso) {
      groups[[length(groups) + 1L]] <- id
    }
  }

  groups
}

display_parameter <- function(parameter) {
  switch(
    parameter,
    "x[1]" = "x_1",
    "x[2]" = "x_2",
    "a"    = "x_1",
    "b"    = "x_2",
    parameter
  )
}

empty_row <- function() {
  data.frame(
    experiment       = character(),
    target           = character(),
    dir              = character(),
    parameter        = character(),
    display_parameter = character(),
    component        = integer(),
    n_c              = integer(),
    chains           = character(),
    mean             = numeric(),
    q2.5             = numeric(),
    q50              = numeric(),
    q97.5            = numeric(),
    monitor_rhat     = numeric(),
    stringsAsFactors = FALSE
  )
}

rows <- empty_row()

for (job in experiments) {
  fit_path <- file.path(baseline_dir, job$dir, paste0(job$dir, ".rds"))
  summary_path <- file.path(
    pairwise_dir,
    job$dir,
    "mcmc_graph_summary.csv"
  )

  if (!file.exists(fit_path)) {
    warning("Missing fit: ", fit_path)
    next
  }
  if (!file.exists(summary_path)) {
    warning("Missing cluster summary: ", summary_path)
    next
  }

  fit <- readRDS(fit_path)
  arr <- as.array(fit)
  cluster_table <- read.csv(summary_path, stringsAsFactors = FALSE)

  for (parameter in job$parameters) {
    cluster_row <- cluster_table[cluster_table$parameter == parameter, , drop = FALSE]
    if (nrow(cluster_row) == 0L) {
      warning("No cluster row for ", job$dir, " / ", parameter)
      next
    }

    isolated <- if ("isolated_chains" %in% names(cluster_row)) {
      cluster_row$isolated_chains[1]
    } else {
      ""
    }

    groups <- parse_chain_groups(cluster_row$clusters[1], isolated)
    if (length(groups) == 0L) {
      groups <- list(seq_len(dim(arr)[2]))
    }

    if (!parameter %in% dimnames(arr)[[3]]) {
      warning("Parameter ", parameter, " not in ", job$dir)
      next
    }

    for (k in seq_along(groups)) {
      chain_ids <- groups[[k]]
      monitored <- rstan::monitor(
        arr[, chain_ids, parameter, drop = FALSE],
        warmup = 0,
        probs  = c(0.025, 0.5, 0.975),
        print  = FALSE
      )
      mon <- as.data.frame(monitored)

      rows <- rbind(
        rows,
        data.frame(
          experiment        = job$experiment,
          target            = job$target,
          dir               = job$dir,
          parameter         = parameter,
          display_parameter = display_parameter(parameter),
          component         = k,
          n_c               = length(chain_ids),
          chains            = paste(chain_ids, collapse = ","),
          mean              = unname(mon$mean[1]),
          q2.5              = unname(mon[["2.5%"]][1]),
          q50               = unname(mon[["50%"]][1]),
          q97.5             = unname(mon[["97.5%"]][1]),
          monitor_rhat      = unname(mon$Rhat[1]),
          stringsAsFactors  = FALSE
        )
      )
    }
  }
}

fmt_signed <- function(x) {
  vapply(x, function(z) {
    s <- formatC(abs(z), format = "f", digits = 3)
    if (z < 0) {
      paste0("$-$", s)
    } else {
      paste0("$\\phantom{-}$", s)
    }
  }, character(1))
}

rows$latex_mean_ci <- paste0(
  fmt_signed(rows$mean),
  " [",
  fmt_signed(rows$q2.5),
  ", ",
  fmt_signed(rows$q97.5),
  "]"
)

out_csv <- file.path(output_dir, "component_monitor_summaries.csv")
write.csv(rows, out_csv, row.names = FALSE)

for (model_dir in unique(rows$dir)) {
  write.csv(
    rows[rows$dir == model_dir, , drop = FALSE],
    file.path(pairwise_dir, model_dir, "component_monitor.csv"),
    row.names = FALSE
  )
}

print(rows[, c(
  "experiment", "target", "display_parameter", "component", "n_c",
  "chains", "mean", "q2.5", "q97.5", "latex_mean_ci"
)])
cat("\nSaved ", out_csv, "\n", sep = "")
