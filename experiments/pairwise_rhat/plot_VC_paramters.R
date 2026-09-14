library(rstan)
if (requireNamespace("mcmcConvergenceGraph", quietly = TRUE)) {
  library(mcmcConvergenceGraph)
} else {
  pkgload::load_all("/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph", quiet = TRUE)
}

fit <- readRDS(
  "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat/three_compartment_pk_infer_vc/three_compartment_pk_infer_vc.rds"
)

# Shared split for all six panels: groups from k13's G_rho.
result <- mcmcgraph(
  fit,
  parameters = "k13",
  rho = 1.05,
  plot = FALSE
)

result$intersection_clusters$all_components

group_ids <- lapply(
  result$intersection_clusters$all_components,
  function(names) as.integer(gsub("[^0-9]", "", names))
)
group_ids <- group_ids[order(vapply(group_ids, min, integer(1)))]
chain_group <- integer(8)
chain_group[group_ids[[1]]] <- 1L #ORANGE
chain_group[group_ids[[2]]] <- 2L #PURPLE

colours <- c("#f1a340", "#998ec3")
arr <- as.array(fit)
draws_group1 <- as.vector(arr[, chain_group == 1, "VC"])
draws_group2 <- as.vector(arr[, chain_group == 2, "VC"])
d1 <- density(draws_group1, adjust = 1.1)
d2 <- density(draws_group2, adjust = 1.1)

docs_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs"
out_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/experiment3_3pk"
pdf_path <- file.path(docs_dir, "fig_experiment3_VC.pdf")

grDevices::pdf(pdf_path, width = 5, height = 5)
graphics::par(
  mar = c(3.2, 3.2, 3.6, 0.6),
  pty = "s",
  xpd = NA,
  cex.main = 1.85,
  cex.axis = 0.95
)
# VC is on a different scale from the five rate parameters.
xlim <- range(d1$x, d2$x)
ylim <- c(0, max(d1$y, d2$y))
plot(
  NA,
  xlim = xlim,
  ylim = ylim,
  xlab = "",
  ylab = "",
  main = expression(italic(V)[C]),
  yaxs = "i",
  bty = "l"
)
polygon(
  c(d1$x, rev(d1$x)),
  c(d1$y, rep(0, length(d1$y))),
  col = adjustcolor(colours[1], 0.45),
  border = NA
)
polygon(
  c(d2$x, rev(d2$x)),
  c(d2$y, rep(0, length(d2$y))),
  col = adjustcolor(colours[2], 0.45),
  border = NA
)
grDevices::dev.off()
file.copy(pdf_path, file.path(out_dir, "fig_experiment3_VC.pdf"), overwrite = TRUE)
cat("wrote", pdf_path, "\n")
