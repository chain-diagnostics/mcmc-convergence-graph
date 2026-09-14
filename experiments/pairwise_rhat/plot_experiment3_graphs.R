# Experiment 3 graphs.
#
# Logic (both panels):
#   1. readRDS(fit)
#   2. mcmcgraph(...) builds G_union, G_intersection, and per-parameter graphs
#   3. draw that graph with a few local settings only:
#        square PDF, no package title/legend on the all-parameter panel,
#        VC teal / k31 yellow on the selected panel.
#
# We do not call plot_mcmc_graph() itself: it forces asp = 0 (wide).
# We reuse its layout and edge rules, then set asp = 1.

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

teal <- "#2E6C6D"
yellow <- "#EFC743"

layout_from_intersection <- getFromNamespace(
  "layout_from_intersection",
  "mcmcConvergenceGraph"
)
edge_color_from_graph <- getFromNamespace(
  "edge_color_from_graph",
  "mcmcConvergenceGraph"
)
edge_lty_from_graph <- getFromNamespace(
  "edge_lty_from_graph",
  "mcmcConvergenceGraph"
)

chain_labels <- function(graph) {
  labels <- gsub("[^0-9]", "", igraph::V(graph)$name)
  if (any(labels == "")) {
    labels <- as.character(seq_along(igraph::V(graph)))
  }
  labels
}

open_square_pdf <- function(pdf_path, top_mar = 0.4) {
  if (!dir.exists(dirname(pdf_path))) {
    dir.create(dirname(pdf_path), recursive = TRUE)
  }
  grDevices::pdf(pdf_path, width = 5, height = 5)
  graphics::par(
    mar = c(0.4, 0.4, top_mar, 0.4),
    pty = "s",
    xpd = NA,
    cex.main = 1.85,
    lend = "round",
    ljoin = "round"
  )
}

# Package default drawing, square, no title, no legend.
save_square_all_parameters <- function(fit, pdf_path) {
  result <- mcmcgraph(
    fit,
    parameters = c("k10", "k12", "k21", "k13", "k31", "VC"),
    rho        = 1.05,
    plot       = FALSE
  )
  graph <- result$combined_graph
  layout_matrix <- layout_from_intersection(
    result$combined_graph_intersection,
    graph
  )
  open_square_pdf(pdf_path)
  igraph::plot.igraph(
    graph,
    layout              = layout_matrix,
    vertex.size         = 22,
    vertex.color        = igraph::V(graph)$color,
    vertex.frame.color  = "grey30",
    vertex.label        = chain_labels(graph),
    vertex.label.color  = "black",
    vertex.label.cex    = 1.8,
    edge.color          = edge_color_from_graph(graph),
    edge.width          = 1.6,
    edge.lty            = edge_lty_from_graph(graph),
    edge.curved         = 0.2,
    edge.label          = NA,
    main                = "",
    asp                 = 1
  )
  grDevices::dev.off()
}

pair_key <- function(from, to) {
  paste(pmin(from, to), pmax(from, to), sep = "--")
}

edges_from_graph <- function(graph) {
  if (igraph::ecount(graph) == 0L) {
    return(data.frame(from = character(), to = character(), key = character()))
  }
  el <- igraph::as_edgelist(graph, names = TRUE)
  data.frame(
    from = el[, 1],
    to   = el[, 2],
    key  = pair_key(el[, 1], el[, 2]),
    stringsAsFactors = FALSE
  )
}

# Two parameters only: keep solid edges that exist in both G_VC and G_k31.
# One-parameter (dashed) chords are omitted; they do not change the grouping.
# Colour: VC teal, k31 yellow, both drawn solid; no black if both agree.
save_square_k31_vc <- function(fit, pdf_path) {
  result <- mcmcgraph(
    fit,
    parameters = c("k31", "VC"),
    rho        = 1.05,
    plot       = FALSE
  )
  layout_matrix <- layout_from_intersection(
    result$combined_graph_intersection,
    result$combined_graph
  )
  vc_edges <- edges_from_graph(result$graphs[["VC"]])
  k31_edges <- edges_from_graph(result$graphs[["k31"]])
  shared <- intersect(vc_edges$key, k31_edges$key)
  vc_edges <- vc_edges[vc_edges$key %in% shared, , drop = FALSE]
  k31_edges <- k31_edges[k31_edges$key %in% shared, , drop = FALSE]

  as_draw <- function(edges, colour, curve) {
    if (nrow(edges) == 0L) {
      return(data.frame(
        from = character(), to = character(),
        colour = character(), curve = numeric(), lty = integer()
      ))
    }
    data.frame(
      from   = edges$from,
      to     = edges$to,
      colour = colour,
      curve  = curve,
      lty    = 1L,
      stringsAsFactors = FALSE
    )
  }

  vc_draw <- as_draw(vc_edges, teal, -0.22)
  k31_draw <- as_draw(k31_edges, yellow, 0.22)
  draw <- rbind(vc_draw, k31_draw)

  vertices <- igraph::V(result$combined_graph)$name
  overlay <- igraph::graph_from_data_frame(
    d = draw[, c("from", "to")],
    directed = FALSE,
    vertices = data.frame(name = vertices, stringsAsFactors = FALSE)
  )
  igraph::V(overlay)$color <- "lightblue"

  open_square_pdf(pdf_path, top_mar = 2.8)
  igraph::plot.igraph(
    overlay,
    layout              = layout_matrix,
    vertex.size         = 22,
    vertex.color        = igraph::V(overlay)$color,
    vertex.frame.color  = "grey30",
    vertex.label        = chain_labels(overlay),
    vertex.label.color  = "black",
    vertex.label.cex    = 1.8,
    edge.color          = draw$colour,
    edge.width          = 1.8,
    edge.lty            = draw$lty,
    edge.curved         = draw$curve,
    edge.label          = NA,
    main                = "",
    asp                 = 1
  )
  graphics::legend(
    "top",
    inset  = c(0, -0.02),
    legend = c(expression(italic(V)[C]), expression(italic(k)[31])),
    col    = c(teal, yellow),
    lty    = 1,
    lwd    = 2,
    horiz  = TRUE,
    bty    = "n",
    cex    = 1
  )
  grDevices::dev.off()
}

file_all <- "fig_experiment3_a_all_parameters.pdf"
path_all <- file.path(docs_dir, file_all)
save_square_all_parameters(fit, path_all)
file.copy(path_all, file.path(out_dir, file_all), overwrite = TRUE)
cat("wrote", path_all, "\n")

file_sel <- "fig_experiment3_b_k31_VC.pdf"
path_sel <- file.path(docs_dir, file_sel)
save_square_k31_vc(fit, path_sel)
file.copy(path_sel, file.path(out_dir, file_sel), overwrite = TRUE)
cat("wrote", path_sel, "\n")
