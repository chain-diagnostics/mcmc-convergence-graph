# Build PANEL (a) TikZ from the saved Experiment 1 datasets.

base <- "/Users/chegu121/Documents/Phd-cici/r_package/result_baseline_rhat"
out  <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs/fig_experiment1_panel_a.tex"

pad  <- 0.16
span <- 1.86
fmt  <- function(z) sprintf("%.3f", z)

map_pts <- function(x, y, xlim, ylim) {
  data.frame(
    xx = pad + span * (x - xlim[1]) / diff(xlim),
    yy = pad + span * (y - ylim[1]) / diff(ylim)
  )
}

expand_range <- function(z, extra = 0.08) {
  r <- range(z)
  m <- diff(r)
  if (m == 0) m <- 1
  c(r[1] - extra * m, r[2] + extra * m)
}

equal_lims <- function(x, y, extra = 0.12) {
  xr <- range(x)
  yr <- range(y)
  m <- max(diff(xr), diff(yr)) * (1 + extra)
  list(
    xlim = mean(xr) + c(-1, 1) * m / 2,
    ylim = mean(yr) + c(-1, 1) * m / 2
  )
}

nodes <- function(pts) {
  paste0(
    "    \\node[dataPoint] at (", fmt(pts$xx), ",", fmt(pts$yy), ") {};",
    collapse = "\n"
  )
}

# Plot a fixed subset of the real draws so a 2 cm panel stays a
# schematic rather than a solid grey block. Linear already has N = 60.
set.seed(2026)
n_plot <- 80

u <- read.csv(file.path(base, "uniform/data_generating_samples.csv"))
u_idx <- sort(sample.int(nrow(u), n_plot))
u_pts <- map_pts(u$theta1[u_idx], u$theta2[u_idx], c(-2, 2), c(-2, 2))

g <- read.csv(file.path(base, "gaussian_ball/data_generating_samples.csv"))
g_idx <- sort(sample.int(nrow(g), n_plot))
g_r <- max(3, max(abs(c(g$theta1, g$theta2))) * 1.05)
g_pts <- map_pts(g$theta1[g_idx], g$theta2[g_idx], c(-g_r, g_r), c(-g_r, g_r))

ln <- read.csv(file.path(base, "linear_with_noise/data_generating_samples.csv"))
xlim_ln <- expand_range(ln$x, 0.06)
ylim_ln <- expand_range(ln$y, 0.10)
ln_pts <- map_pts(ln$x, ln$y, xlim_ln, ylim_ln)
x_line <- xlim_ln
y_line <- 1.5 * x_line + 0.5
line_pts <- map_pts(x_line, y_line, xlim_ln, ylim_ln)

tc <- read.csv(file.path(base, "three_cluster/data_generating_samples.csv"))
tc_idx <- sort(sample.int(nrow(tc), n_plot))
tcl <- equal_lims(tc$theta1, tc$theta2, extra = 0.14)
tc_pts <- map_pts(tc$theta1[tc_idx], tc$theta2[tc_idx], tcl$xlim, tcl$ylim)

tex <- c(
  "% =========================================================",
  "% PANEL (a): four simulated distributions",
  "% Points are the saved Experiment 1 datasets in",
  "% result_baseline_rhat/*/data_generating_samples.csv",
  "% (seed 2026).",
  "% =========================================================",
  "",
  "\\tikzset{",
  "  dataPoint/.style={",
  "    circle,",
  "    fill=gray!88,",
  "    draw=none,",
  "    inner sep=0pt,",
  "    minimum size=0.72mm",
  "  },",
  "  trueLine/.style={",
  "    draw=gray!55,",
  "    line width=0.70pt",
  "  }",
  "}",
  "",
  "\\def\\pw{2.18}",
  "\\def\\ph{2.30}",
  "\\def\\py{3.66}",
  "\\def\\titley{2.48}",
  "",
  "",
  "% =========================================================",
  "% 1. UNIFORM",
  "% y ~ Uniform(-2,2)^2   (80 of 1000 saved draws)",
  "% =========================================================",
  "\\begin{scope}[shift={(0.36,\\py)}]",
  "",
  "  \\node[",
  "    font=\\scriptsize\\bfseries,",
  "    anchor=south",
  "  ] at ({0.5*\\pw},\\titley)",
  "  {",
  "    Uniform",
  "  };",
  "",
  nodes(u_pts),
  "",
  "\\end{scope}",
  "",
  "",
  "% =========================================================",
  "% 2. GAUSSIAN BALL",
  "% y ~ N(0, I_2)   (80 of 1000 saved draws)",
  "% =========================================================",
  "\\begin{scope}[shift={(3.42,\\py)}]",
  "",
  "  \\node[",
  "    font=\\scriptsize\\bfseries,",
  "    anchor=south",
  "  ] at ({0.5*\\pw},\\titley)",
  "  {",
  "    One cluster",
  "  };",
  "",
  nodes(g_pts),
  "",
  "\\end{scope}",
  "",
  "",
  "% =========================================================",
  "% 3. LINEAR DATA WITH GAUSSIAN NOISE",
  "% x ~ Uniform(-2, 2),  y = 1.5 x + 0.5 + N(0, 0.5^2)",
  "% N = 60. Grey line is the true mean 1.5 x + 0.5.",
  "% =========================================================",
  "\\begin{scope}[shift={(6.48,\\py)}]",
  "",
  "  \\node[",
  "    font=\\scriptsize\\bfseries,",
  "    anchor=south",
  "  ] at ({0.5*\\pw},\\titley)",
  "  {",
  "    Line with noise",
  "  };",
  "",
  "  % Underlying linear relation y = 1.5 x + 0.5",
  paste0(
    "  \\draw[trueLine]\n    (",
    fmt(line_pts$xx[1]), ",", fmt(line_pts$yy[1]),
    ")\n    --\n    (",
    fmt(line_pts$xx[2]), ",", fmt(line_pts$yy[2]),
    ");"
  ),
  "",
  nodes(ln_pts),
  "",
  "\\end{scope}",
  "",
  "",
  "% =========================================================",
  "% 4. THREE GAUSSIAN CLUSTERS",
  "% Mixture centres (-6,-5), (6,0), (0,6); variance 0.25",
  "% 80 of 1000 saved draws. Aspect ratio preserved.",
  "% =========================================================",
  "\\begin{scope}[shift={(9.54,\\py)}]",
  "",
  "  \\node[",
  "    font=\\scriptsize\\bfseries,",
  "    anchor=south",
  "  ] at ({0.5*\\pw},\\titley)",
  "  {",
  "    Three clusters",
  "  };",
  "",
  nodes(tc_pts),
  "",
  "\\end{scope}"
)

dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
writeLines(tex, out, useBytes = TRUE)
cat("Wrote", out, "\n")
cat("lines", length(readLines(out)), "\n")
cat("points", nrow(u_pts), nrow(g_pts), nrow(ln_pts), nrow(tc_pts), "\n")
