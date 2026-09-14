# One row of four square graphs, matching Fig. 2 printed size:
# width  = 16.00 cm = \textwidth
# height =  4.00 cm

library(rstan)
library(mcmcConvergenceGraph)

source(
  "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/experiments/pairwise_rhat/save_square_graph.R"
)

base_fit <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
out_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/experiment1_square_graphs"
docs_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

experiments <- list(
  list(key = "uniform",              letter = "(a)", title = "Uniform"),
  list(key = "unimodal_gaussian",    letter = "(b)", title = "Gaussian ball"),
  list(key = "anisotropic_gaussian", letter = "(c)", title = "Anisotropic Gaussian"),
  list(key = "multimodal_gaussian",  letter = "(d)", title = "Multimodal Gaussian")
)

graphs <- lapply(experiments, function(experiment) {
  fit <- readRDS(
    file.path(base_fit, experiment$key, paste0(experiment$key, ".rds"))
  )
  result <- mcmcgraph(
    fit,
    parameters = c("x[1]", "x[2]"),
    rho        = 1.05,
    plot       = FALSE
  )
  result$combined_graph_intersection
})

plot_one_panel <- function(graph, letter, title) {
  labels <- gsub("[^0-9]", "", igraph::V(graph)$name)
  if (any(labels == "")) {
    labels <- as.character(seq_along(igraph::V(graph)))
  }

  vertex_color <- igraph::V(graph)$color
  if (is.null(vertex_color)) {
    vertex_color <- "gray80"
  }

  igraph::plot.igraph(
    graph,
    layout             = layout_separated_components(graph),
    vertex.size        = 18,
    vertex.color       = vertex_color,
    vertex.frame.color = NA,
    vertex.label       = labels,
    vertex.label.color = "black",
    vertex.label.cex   = 1.15,
    edge.color         = "black",
    edge.width         = 1.2,
    edge.lty           = 1,
    edge.curved        = 0.2,
    edge.label         = NA,
    main               = paste(letter, title),
    asp                = 1
  )
}

row_pdf <- file.path(out_dir, "experiment1_one_row.pdf")
docs_pdf <- file.path(docs_dir, "fig_experiment1_graphs.pdf")

# Fig. 2: 16.00 cm by 4.00 cm
grDevices::pdf(
  row_pdf,
  width  = 16 / 2.54,
  height = 4 / 2.54
)
graphics::par(
  mfrow = c(1, 4),
  oma   = c(0.12, 0.12, 0.12, 0.12),
  mar   = c(0.12, 0.12, 1.15, 0.12),
  pty   = "s",
  xpd   = NA,
  lend  = "round",
  ljoin = "round"
)

for (i in seq_along(experiments)) {
  plot_one_panel(
    graphs[[i]],
    experiments[[i]]$letter,
    experiments[[i]]$title
  )
}

grDevices::dev.off()
file.copy(row_pdf, docs_pdf, overwrite = TRUE)

cat("wrote", row_pdf, "\n")
cat("wrote", docs_pdf, "\n")
