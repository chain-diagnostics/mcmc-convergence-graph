# Plot the per-parameter graphs G_a and G_b with the original
# igraph::components() clustering (file 13).

library(igraph)
library(mcmcConvergenceGraph)

matrix_dir <- "/Users/chegu121/Documents/Phd-cici/r_package/result_pairwise_rhat/same_slope_diff_intercept"
out_png <- file.path(matrix_dir, "parameter_graphs_a_and_b.png")
out_pdf <- file.path(matrix_dir, "parameter_graphs_a_and_b.pdf")

# Same threshold you were discussing with the table.
rho <- 1.05

read_rhat_matrix <- function(path) {
  mat <- as.matrix(read.csv(path, row.names = 1))
  storage.mode(mat) <- "numeric"
  mat
}

plot_one <- function(graph, title) {
  cl <- mcmc_graph_components(graph)
  labels <- gsub("[^0-9]", "", igraph::V(graph)$name)
  igraph::plot.igraph(
    graph,
    layout = igraph::layout_in_circle(graph),
    vertex.size = 28,
    vertex.color = "white",
    vertex.frame.color = "black",
    vertex.label = labels,
    vertex.label.color = "black",
    vertex.label.cex = 1.1,
    edge.color = "black",
    edge.width = 1.4,
    main = title
  )
  mtext(
    paste0(
      "K = ", cl$n_clusters,
      ", isolated = ", cl$n_isolated,
      if (cl$n_clusters > 0) {
        paste0(" | ", paste(vapply(cl$clusters, function(x) {
          paste0("(", paste(gsub("[^0-9]", "", x), collapse = ","), ")")
        }, character(1)), collapse = " | "))
      } else {
        ""
      }
    ),
    side = 1,
    line = 0.2,
    cex = 0.75
  )
}

mat_a <- read_rhat_matrix(file.path(matrix_dir, "pairwise_rhat_matrix_a.csv"))
mat_b <- read_rhat_matrix(file.path(matrix_dir, "pairwise_rhat_matrix_b.csv"))

g_a <- mcmc_graph_uni(mat_a, rho = rho)
g_b <- mcmc_graph_uni(mat_b, rho = rho)

png(out_png, width = 2200, height = 1100, res = 180)
par(mfrow = c(1, 2), mar = c(3.2, 1, 3.2, 1))
plot_one(g_a, paste0("Parameter a  (rho = ", rho, ")"))
plot_one(g_b, paste0("Parameter b  (rho = ", rho, ")"))
dev.off()

pdf(out_pdf, width = 10, height = 5)
par(mfrow = c(1, 2), mar = c(3.2, 1, 3.2, 1))
plot_one(g_a, paste0("Parameter a  (rho = ", rho, ")"))
plot_one(g_b, paste0("Parameter b  (rho = ", rho, ")"))
dev.off()

cat("a: missing edges (R-hat > rho)\n")
print(which(mat_a > rho & row(mat_a) < col(mat_a), arr.ind = TRUE))
cat("\nb: missing edges (R-hat > rho)\n")
print(which(mat_b > rho & row(mat_b) < col(mat_b), arr.ind = TRUE))
cat("\nwrote\n", out_png, "\n", out_pdf, "\n", sep = "")
