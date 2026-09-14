# Square, single-graph PDF: G_intersection only, no multivariate legend.

layout_separated_components <- function(graph) {
  comps <- igraph::components(graph)
  n_comp <- comps$no

  if (n_comp <= 1L) {
    return(igraph::layout_in_circle(graph))
  }

  sizes <- comps$csize
  order_ids <- order(sizes, decreasing = TRUE)

  if (n_comp == 3L) {
    centres <- rbind(
      c(0.00, -0.42),
      c(-0.52, 0.36),
      c(0.52, 0.36)
    )
  } else {
    centres <- igraph::layout_in_circle(igraph::make_empty_graph(n_comp))
    centres <- centres * 0.90
  }

  layout <- matrix(NA_real_, nrow = igraph::vcount(graph), ncol = 2L)

  for (rank in seq_along(order_ids)) {
    component_id <- order_ids[[rank]]
    index <- which(comps$membership == component_id)
    n_k <- length(index)

    if (n_k == 1L) {
      local <- matrix(c(0, 0), nrow = 1L)
    } else if (n_k == 2L) {
      local <- matrix(c(-0.22, 0, 0.22, 0), nrow = 2L, byrow = TRUE)
    } else {
      local_scale <- if (n_comp == 3L && rank == 1L) 0.40 else 0.30
      local <- igraph::layout_in_circle(igraph::make_empty_graph(n_k))
      local <- local * local_scale
    }

    layout[index, ] <- sweep(local, 2L, centres[rank, ], "+")
  }

  layout
}

save_square_graph <- function(graph, title, pdf_path) {
  labels <- gsub("[^0-9]", "", igraph::V(graph)$name)
  if (any(labels == "")) {
    labels <- as.character(seq_along(igraph::V(graph)))
  }

  vertex_color <- igraph::V(graph)$color
  if (is.null(vertex_color)) {
    vertex_color <- "gray80"
  }

  if (!dir.exists(dirname(pdf_path))) {
    dir.create(dirname(pdf_path), recursive = TRUE)
  }

  grDevices::pdf(pdf_path, width = 5, height = 5)
  graphics::par(
    mar = c(0.4, 0.4, 3.6, 0.4),
    pty = "s",
    xpd = NA,
    cex.main = 1.85,
    lend = "round",
    ljoin = "round"
  )
  igraph::plot.igraph(
    graph,
    layout              = layout_separated_components(graph),
    vertex.size         = 22,
    vertex.color        = vertex_color,
    vertex.frame.color  = NA,
    vertex.label        = labels,
    vertex.label.color  = "black",
    vertex.label.cex    = 1.8,
    edge.color          = "black",
    edge.width          = 1.6,
    edge.lty            = 1,
    edge.curved         = 0.2,
    edge.label          = NA,
    main                = title,
    asp                 = 1
  )
  grDevices::dev.off()

  invisible(pdf_path)
}

save_square_posterior_ab <- function(fit, title, pdf_path) {
  draws <- as.array(fit)
  a <- as.numeric(draws[, , "a"])
  b <- as.numeric(draws[, , "b"])

  if (!dir.exists(dirname(pdf_path))) {
    dir.create(dirname(pdf_path), recursive = TRUE)
  }

  grDevices::pdf(pdf_path, width = 5, height = 5)
  graphics::par(
    mar = c(3.2, 3.2, 3.6, 0.6),
    pty = "s",
    xpd = NA,
    cex.main = 1.85,
    cex.lab = 1.15,
    cex.axis = 0.95
  )
  graphics::plot(
    a,
    b,
    pch = 16,
    cex = 0.35,
    col = "gray25",
    xlab = expression(italic(a)),
    ylab = expression(italic(b)),
    main = title,
    asp = 1
  )
  grDevices::dev.off()

  invisible(pdf_path)
}
