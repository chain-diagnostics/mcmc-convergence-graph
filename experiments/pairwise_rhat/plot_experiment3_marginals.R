# Experiment 3: six square marginals for the rate parameters.
# Colour is fixed by G_intersection components, not by peak location.

library(rstan)
if (requireNamespace("mcmcConvergenceGraph", quietly = TRUE)) {
  library(mcmcConvergenceGraph)
} else {
  pkgload::load_all("/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph", quiet = TRUE)
}

fit_path <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk_infer_vc/three_compartment_pk_infer_vc.rds"
docs_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs"
out_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/experiment3_3pk"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fit <- readRDS(fit_path)
arr <- as.array(fit)

rate_params <- c("k10", "k12", "k21", "k13", "k31", "VC")

result <- mcmcgraph(
  fit,
  parameters = rate_params,
  rho        = 1.05,
  plot       = FALSE
)

chain_ids_from <- function(names) {
  ids <- as.integer(gsub("[^0-9]", "", names))
  if (anyNA(ids) || any(ids == 0L)) {
    ids <- seq_along(names)
  }
  ids
}

group_ids <- lapply(
  result$intersection_clusters$all_components,
  chain_ids_from
)
group_ids <- group_ids[order(vapply(group_ids, min, integer(1)))]

# ColorBrewer PuOr, 3-class (orange / purple). Same map on every parameter.
component_colours <- c("#f1a340", "#998ec3")

n_chain <- dim(arr)[2]
chain_group <- integer(n_chain)
for (g in seq_along(group_ids)) {
  chain_group[group_ids[[g]]] <- g
}

cat("orange #f1a340 : chains", paste(group_ids[[1]], collapse = ", "), "\n")
cat("purple #998ec3 : chains", paste(group_ids[[2]], collapse = ", "), "\n")

param_titles <- list(
  k10 = expression(italic(k)[10]),
  k12 = expression(italic(k)[12]),
  k21 = expression(italic(k)[21]),
  k13 = expression(italic(k)[13]),
  k31 = expression(italic(k)[31]),
  VC  = expression(italic(V)[C])
)

param_files <- c(
  k10 = "fig_experiment3_k10.pdf",
  k12 = "fig_experiment3_k12.pdf",
  k21 = "fig_experiment3_k21.pdf",
  k13 = "fig_experiment3_k13.pdf",
  k31 = "fig_experiment3_k31.pdf",
  VC  = "fig_experiment3_VC.pdf"
)

save_square_marginal <- function(arr, par, chain_group, colours, title, pdf_path) {
  values <- lapply(seq_along(colours), function(g) {
    as.vector(arr[, chain_group == g, par])
  })
  dens <- lapply(values, stats::density, adjust = 1.1)
  xlim <- range(unlist(lapply(dens, function(d) d$x)), finite = TRUE)
  ylim <- c(0, max(vapply(dens, function(d) max(d$y), numeric(1))))

  grDevices::pdf(pdf_path, width = 5, height = 5)
  graphics::par(
    mar = c(3.2, 3.2, 3.6, 0.6),
    mgp = c(2.0, 0.7, 0),
    pty = "s",
    xpd = NA,
    cex.main = 1.85,
    cex.axis = 0.95
  )
  graphics::plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    main = title,
    yaxs = "i",
    bty = "o"
  )
  for (g in seq_along(dens)) {
    graphics::polygon(
      c(dens[[g]]$x, rev(dens[[g]]$x)),
      c(dens[[g]]$y, rep(0, length(dens[[g]]$y))),
      col = grDevices::adjustcolor(colours[[g]], alpha.f = 0.45),
      border = colours[[g]],
      lwd = 1.6
    )
  }
  grDevices::dev.off()
}

for (par in rate_params) {
  pdf_path <- file.path(docs_dir, param_files[[par]])
  save_square_marginal(
    arr          = arr,
    par          = par,
    chain_group  = chain_group,
    colours      = component_colours,
    title        = param_titles[[par]],
    pdf_path     = pdf_path
  )
  file.copy(pdf_path, file.path(out_dir, param_files[[par]]), overwrite = TRUE)
  cat("wrote", pdf_path, "\n")
}
