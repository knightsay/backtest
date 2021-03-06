% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/counts.R
\docType{methods}
\name{counts,backtest-method}
\alias{counts,backtest-method}
\title{\code{counts} Method}
\usage{
\S4method{counts}{backtest}(object)
}
\arguments{
\item{object}{is the backtest object to work on}
}
\value{
returns a list of matrices, with one matrix for each \code{in.var}
}
\description{
The \code{counts} method returns a list of matrices, with one matrix for each \code{in.var},
where the value of each cell is the number of observations for that \code{in.var} and
\code{by.var} combination.
}
\examples{
data(starmine)
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
counts(bt)
}
\seealso{
\code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
}

