% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/means.R
\docType{methods}
\name{means,backtest-method}
\alias{means,backtest-method}
\title{\code{means} Method}
\usage{
\S4method{means}{backtest}(object)
}
\arguments{
\item{object}{is the backtest object we want mean of returns of}
}
\value{
a list of matrices, with one matrix for each \code{in.var}, where the
        value of each cell is the mean of the returns for that \code{in.var} and \code{by.var}
        combination.
}
\description{
The \code{means} method returns a list of matrices which show the mean of returns
}
\examples{
data(starmine)
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
means(bt)
}
\seealso{
\code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
}

