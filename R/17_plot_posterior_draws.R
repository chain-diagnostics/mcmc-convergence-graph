utils::globalVariables(c("x", "y", "chain"))

#' Plot a 2D posterior-draws scatter coloured by chain
#'
#' Draws a two-dimensional scatter of posterior samples for a chosen pair of
#' parameters, with points coloured by MCMC chain. This is intended as a
#' visual companion to the pairwise R-hat diagnostic: when chains fail to mix,
#' the scatter shows them as distinctly coloured point clouds sitting on
#' different posterior modes.
#'
#' @param draws A three-dimensional array of posterior draws with dimensions
#'   iterations by chains by parameters, or an rstan `stanfit` object.
#' @param parameters A character vector of length two giving the names of the
#'   two parameters to scatter. The names must match the parameter dimnames of
#'   `draws`. For vector parameters, use Stan's `theta[1]`, `theta[2]`
#'   convention as reported by `rstan`.
#' @param alpha Numeric point transparency in `[0, 1]`. The default is `0.5`.
#' @param size Numeric point size. The default is `1.2`.
#' @param colours Character vector of hex colours, one per chain. Defaults to
#'   a six-colour pastel palette (baby blue, baby pink, baby green, baby
#'   yellow, baby purple, baby peach). If the number of chains exceeds the
#'   number of supplied colours, the palette is recycled with a warning.
#'
#' @return A `ggplot` object that can be printed, saved with
#'   `ggplot2::ggsave()`, or extended with additional layers.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' draws <- array(
#'   c(
#'     rnorm(100 * 2, mean = 0),
#'     rnorm(100 * 2, mean = 5)
#'   ),
#'   dim = c(100, 4, 2)
#' )
#'
#' dimnames(draws) <- list(
#'   NULL,
#'   paste0("chain", 1:4),
#'   c("alpha", "beta")
#' )
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_posterior_draws(
#'     draws      = draws,
#'     parameters = c("alpha", "beta")
#'   )
#' }
plot_posterior_draws <- function(
    draws,
    parameters,
    alpha   = 0.5,
    size    = 1.2,
    colours = c(
      "#A6D8F0",
      "#F8C8DC",
      "#C1E1C1",
      "#FDFD96",
      "#C3B1E1",
      "#FFDAB9"
    )
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plot_posterior_draws(). ",
      "Install it with install.packages('ggplot2').",
      call. = FALSE
    )
  }

  draws <- as_draws_array(draws)

  if (!is.character(parameters) || length(parameters) != 2L) {
    stop(
      "`parameters` must be a character vector of length 2.",
      call. = FALSE
    )
  }

  parameter_names <- dimnames(draws)[[3]]

  missing_parameters <- setdiff(parameters, parameter_names)

  if (length(missing_parameters) > 0L) {
    stop(
      "The following parameters were not found in `draws`: ",
      paste(missing_parameters, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  n_iterations <- dim(draws)[1]
  n_chains     <- dim(draws)[2]

  chain_labels <- dimnames(draws)[[2]]

  if (is.null(chain_labels)) {
    chain_labels <- paste0("chain", seq_len(n_chains))
  }

  if (length(colours) < n_chains) {
    warning(
      "Fewer colours (", length(colours),
      ") than chains (", n_chains,
      "); recycling the palette.",
      call. = FALSE
    )
    colours <- rep_len(colours, n_chains)
  }

  chain_colours        <- colours[seq_len(n_chains)]
  names(chain_colours) <- chain_labels

  plot_data <- data.frame(
    x     = as.vector(draws[, , parameters[1]]),
    y     = as.vector(draws[, , parameters[2]]),
    chain = factor(
      rep(chain_labels, each = n_iterations),
      levels = chain_labels
    )
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y, colour = chain)
  ) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::scale_colour_manual(values = chain_colours, name = "Chain") +
    ggplot2::labs(x = parameters[1], y = parameters[2]) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right")
}
