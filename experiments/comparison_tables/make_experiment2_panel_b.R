# Build PANEL (b) TikZ from Experiment 2 data and posterior draws.
#
# Each panel: observed (x, y) by mixture component, a vertical reference
# at x = 0, faint posterior draws of the single fitted line, and its
# posterior-median line.

base <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
out  <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs/fig_experiment2_panel_b.tex"

pad_l <- 0.30
pad_b <- 0.30
span  <- 1.72
fmt   <- function(z) sprintf("%.3f", z)

map_xy <- function(x, y, xlim, ylim) {
  data.frame(
    xx = pad_l + span * (x - xlim[1]) / diff(xlim),
    yy = pad_b + span * (y - ylim[1]) / diff(ylim)
  )
}

expand_range <- function(z, extra = 0.12) {
  r <- range(z)
  m <- max(diff(r), 1e-6)
  c(r[1] - extra * m, r[2] + extra * m)
}

line_ends <- function(a, b, xlim) {
  data.frame(x = xlim, y = a * xlim + b)
}

nodes <- function(pts, style) {
  paste0(
    "    \\node[", style, "] at (", fmt(pts$xx), ",", fmt(pts$yy), ") {};",
    collapse = "\n"
  )
}

draw_line <- function(pts, style) {
  paste0(
    "  \\draw[", style, "]\n    (",
    fmt(pts$xx[1]), ",", fmt(pts$yy[1]),
    ")\n    --\n    (",
    fmt(pts$xx[2]), ",", fmt(pts$yy[2]),
    ");"
  )
}

posterior_lines <- function(fit, xlim, ylim, n_draw = 12) {
  post <- as.data.frame(rstan::extract(fit, permuted = TRUE))
  set.seed(2026)
  idx <- sort(sample.int(nrow(post), n_draw))

  med <- line_ends(stats::median(post$a), stats::median(post$b), xlim)
  draws <- lapply(idx, function(i) line_ends(post$a[i], post$b[i], xlim))

  list(
    med   = map_xy(med$x, med$y, xlim, ylim),
    draws = lapply(draws, function(d) map_xy(d$x, d$y, xlim, ylim))
  )
}

panel_tex <- function(shift, title, dgp, fit, kind) {
  xlim <- c(-1, 1)
  y_lines <- c(
    dgp$y,
    dgp$x *  1 +  1,
    dgp$x * -1 + -1
  )
  ylim <- expand_range(dgp$y, extra = 0.18)

  g1 <- map_xy(dgp$x[dgp$component == 1], dgp$y[dgp$component == 1], xlim, ylim)
  g2 <- map_xy(dgp$x[dgp$component == 2], dgp$y[dgp$component == 2], xlim, ylim)
  v0 <- map_xy(c(0, 0), ylim, xlim, ylim)
  post <- posterior_lines(fit, xlim, ylim)

  draw_spag <- function(lst, style) {
    paste(vapply(lst, function(d) draw_line(d, style), character(1)), collapse = "\n")
  }

  c(
    paste0("\\begin{scope}[shift={", shift, "}]"),
    "",
    "  \\node[",
    "    font=\\scriptsize\\bfseries,",
    "    anchor=south",
    "  ] at ({0.5*\\mw},\\btitley)",
    "  {",
    paste0("    ", title),
    "  };",
    "",
    "  \\draw[axisLine] (0.30,0.30) -- (2.08,0.30);",
    "  \\draw[axisLine] (0.30,0.30) -- (0.30,2.08);",
    "  \\node[font=\\scriptsize, anchor=west]  at (2.10,0.30) {$x$};",
    "  \\node[font=\\scriptsize, anchor=south] at (0.30,2.10) {$y$};",
    "",
    paste0(
      "  \\draw[vref]\n    (",
      fmt(v0$xx[1]), ",", fmt(v0$yy[1]),
      ")\n    --\n    (",
      fmt(v0$xx[2]), ",", fmt(v0$yy[2]),
      ");"
    ),
    "",
    draw_spag(post$draws, "postSolid"),
    "",
    draw_line(post$med, "lineSolid"),
    "",
    nodes(g1, "dataA"),
    nodes(g2, "dataB"),
    "",
    "\\end{scope}"
  )
}

models <- list(
  list(
    dir   = "same_slope_diff_intercept",
    shift = "(1.10,\\my)",
    title = "Different intercepts",
    kind  = "same_slope"
  ),
  list(
    dir   = "diff_slope_same_intercept",
    shift = "(5.51,\\my)",
    title = "Different slopes",
    kind  = "same_intercept"
  ),
  list(
    dir   = "diff_slope_diff_intercept",
    shift = "(9.92,\\my)",
    title = "Different slopes and intercepts",
    kind  = "free"
  )
)

header <- c(
  "% =========================================================",
  "% PANEL (b): three Gaussian linear-mixture variants",
  "% Data: result_baseline_rhat/*/data_generating_samples.csv",
  "% Lines: faint posterior draws and the posterior-median",
  "%        fitted line (one line, not two).",
  "% =========================================================",
  "",
  "\\tikzset{",
  "  dataA/.style={",
  "    circle, fill=gray!82, draw=none,",
  "    inner sep=0pt, minimum size=0.85mm",
  "  },",
  "  dataB/.style={",
  "    circle, fill=gray!42, draw=none,",
  "    inner sep=0pt, minimum size=0.85mm",
  "  },",
  "  lineSolid/.style={",
  "    draw=black, line width=0.75pt",
  "  },",
  "  lineDash/.style={",
  "    draw=gray!55, line width=0.75pt, dashed",
  "  },",
  "  postSolid/.style={",
  "    draw=black, line width=0.22pt, opacity=0.12",
  "  },",
  "  postDash/.style={",
  "    draw=gray!55, line width=0.22pt, dashed, opacity=0.18",
  "  },",
  "  vref/.style={",
  "    draw=gray!45, line width=0.45pt, dashed",
  "  },",
  "  axisLine/.style={",
  "    draw=black, line width=0.45pt, ->",
  "  }",
  "}",
  "",
  "\\def\\mw{2.18}",
  "\\def\\mh{2.00}",
  "\\def\\my{3.78}",
  "\\def\\btitley{2.18}",
  ""
)

body <- lapply(models, function(m) {
  dgp <- read.csv(file.path(base, m$dir, "data_generating_samples.csv"))
  fit <- readRDS(file.path(base, m$dir, paste0(m$dir, ".rds")))
  c("", panel_tex(m$shift, m$title, dgp, fit, m$kind), "")
})

tex <- c(header, unlist(body, use.names = FALSE))
writeLines(tex, out, useBytes = TRUE)
cat("Wrote", out, "lines", length(tex), "\n")
