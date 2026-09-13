# Why capping gamma at 0.17 changes the result.

out <- "/Users/chegu121/Documents/Phd-cici/r_package/mcmcConvergenceGraph/docs/fig_cauchy_gamma_settings.png"

set.seed(2026)
N <- 200L
x <- runif(N, -1, 1)
y <- c(0.5 * x[1:100] + 0.2, 0.5 * x[101:200] - 0.3) + rnorm(N, 0, 0.05)
comp <- rep(1:2, each = 100)

log_cauchy <- function(r, g) log(g) - log(g^2 + r^2) - log(pi)
g <- seq(0.02, 0.40, length.out = 400)
n <- 1000
ll_middle <- n * log_cauchy(0.25, g)
ll_lock   <- (n / 2) * log_cauchy(0.05, g) + (n / 2) * log_cauchy(0.50, g)

png(out, width = 2400, height = 1050, res = 220)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3.2, 1.1), family = "sans")

plot(
  x, y, pch = 16, cex = 0.45, col = c(rep("gray25", 100), rep("gray65", 100)),
  xlim = c(-1, 1), ylim = c(-1, 1),
  xlab = "x", ylab = "y",
  main = "What each gamma can cover"
)
xg <- c(-1, 1)
# Middle line, Sara unconstrained width gamma = 0.24
ym <- 0.5 * xg - 0.05
polygon(c(xg, rev(xg)), c(ym + 0.24, rev(ym - 0.24)), col = rgb(0.6, 0.6, 0.6, 0.35), border = NA)
lines(xg, ym, lwd = 2.2, lty = 3, col = "gray30")
# Upper line, cap width gamma = 0.17
yu <- 0.5 * xg + 0.2
polygon(c(xg, rev(xg)), c(yu + 0.17, rev(yu - 0.17)), col = rgb(0, 0, 0, 0.12), border = NA)
lines(xg, yu, lwd = 2.4, col = "black")
legend(
  "bottomleft",
  legend = c(
    "Sara fit: middle line, gamma = 0.24",
    "cap: lock on upper line, gamma = 0.17"
  ),
  col = c("gray30", "black"),
  lty = c(3, 1),
  lwd = 2.2,
  bty = "n",
  cex = 0.8
)

plot(
  g, ll_lock,
  type = "l", lwd = 2.4, col = "black",
  ylim = range(c(ll_lock, ll_middle)),
  xlab = expression(gamma),
  ylab = "log likelihood of the 1000 points",
  main = "Which explanation gamma prefers"
)
lines(g, ll_middle, lwd = 2.4, col = "gray40")
abline(v = 0.17, lty = 2, col = "black")
abline(v = 0.24, lty = 3, col = "gray40")
legend(
  "bottomright",
  legend = c(
    "lock onto one line",
    "one line in the middle",
    "your cap 0.17",
    "Sara unconstrained 0.24"
  ),
  col = c("black", "gray40", "black", "gray40"),
  lty = c(1, 1, 2, 3),
  lwd = c(2.4, 2.4, 1.2, 1.2),
  bty = "n",
  cex = 0.8
)

dev.off()
cat("wrote", out, "\n")
