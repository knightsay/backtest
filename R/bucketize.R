#' Constructs a summary data frame
#' 
#' \code{bucketize} constructs a summary data frame; it divides the values of "x"
#' into rows by \code{y.factor} and columns by \code{x.factor}, and performs the
#' \code{compute} function for each group
#' 
#' @param x is a numeric vector
#' @param x.factor is a factor for \code{x}
#' @param y.factor is a factor for \code{x}
#' @param compute compute
#' @param ... additional arguments to be passed on
#' 
#' @return a two-dimensional array of the results, with the levels of \code{x.factor} 
#'         and \code{y.factor} as dimnames.

bucketize <- function(x, x.factor, y.factor, compute, ...){

  stopifnot(
            is.numeric(x),
            is.factor(x.factor),
            is.factor(y.factor),
            all.equal(length(x), length(x.factor)),
            all.equal(length(x),length(y.factor)),
            is.function(compute)
            )

  data <- tapply(x, list(y.factor, x.factor), compute, ...)

  invisible(data)
}
