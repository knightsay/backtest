% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/plot.R
\docType{methods}
\name{plot,backtest,missing-method}
\alias{plot,backtest,missing-method}
\title{\code{plot} method}
\usage{
\S4method{plot}{backtest,missing}(x, type = "return", ...)
}
\arguments{
\item{x}{is the backtest object we want summary of}

\item{type}{is a character string to indicate which type of plot we want. Options are
\code{return}, \code{cumreturn}, or \code{turnover}, which correspond to
plot of returns, cumulative returns, or turnover, respectively.}

\item{...}{additional value to pass on}
}
\value{
a plot of returns, or cumulative returns, or turnovers, as the user specifies
}
\description{
The \code{plot} method returns plots of returns, or cumulative returns, or turnovers for a
\code{backtest} object with \code{natural = TRUE}.
}
\note{
\code{plot} method can only plot natural backtest objects (backtest object with
      \code{natural = TRUE})
}
\examples{
data(starmine)
## A natural backtest
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m",
               date.var = "date", id.var = "id", natural = TRUE, by.period = FALSE)
plot(bt)
}
\seealso{
\code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
}

