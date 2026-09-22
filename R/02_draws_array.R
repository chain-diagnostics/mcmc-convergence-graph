#' Convert input to a draws array
#'
#' Converts an input object to the three-dimensional draws array format used by
#' the package. If `x` is an rstan `stanfit` object, posterior draws are
#' extracted with chains preserved. If `x` is already a three-dimensional array,
#' it is used directly.
#'
#' @param x A three-dimensional array of posterior draws, or an rstan
#'   `stanfit` object.
#' @param parameter Optional character vector of parameter names to retain.
#'   If `NULL`, all model parameters are used (excluding Stan's `lp__`).
#'
#' @returns A checked three-dimensional array of posterior draws with dimensions
#' iterations × chains × parameters.
#'
#' @keywords internal
as_draws_array <- function(x, parameter = NULL) {

  if (inherits(x, "stanfit")) {

    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop(
        "Package 'rstan' is required to convert a stanfit object.",
        call. = FALSE
      )
    }

    draws <- rstan::extract(
      x,
      permuted = FALSE
    )

  } else if (is.array(x) && length(dim(x)) == 3) {

    draws <- x

  } else {

    stop(
      "`x` must be either a 3-dimensional draws array or an rstan stanfit object.",
      call. = FALSE
    )

  }

  dn <- dimnames(draws)
  if (is.null(dn)) {
    dn <- vector("list", 3L)
  }
  if (is.null(dn[[2]])) {
    dn[[2]] <- paste0("chain", seq_len(dim(draws)[2]))
  }
  if (is.null(dn[[3]])) {
    dn[[3]] <- paste0("param", seq_len(dim(draws)[3]))
  }
  dimnames(draws) <- dn

  parameter_names <- dimnames(draws)[[3]]

  if (!is.null(parameter_names)) {

    ## Remove Stan log-posterior
    keep <- parameter_names != "lp__"

    draws <- draws[, , keep, drop = FALSE]

    parameter_names <- dimnames(draws)[[3]]

    ## Keep selected parameters if requested
    if (!is.null(parameter)) {

      missing_parameters <- setdiff(
        parameter,
        parameter_names
      )

      if (length(missing_parameters) > 0) {
        stop(
          "Unknown parameter(s): ",
          paste(missing_parameters, collapse = ", "),
          call. = FALSE
        )
      }

      draws <- draws[, , parameter, drop = FALSE]

    }

  }

  check_draws_array(draws)

  draws
}
