% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/totalCounts.R
\docType{methods}
\name{totalCounts,backtest-method}
\alias{totalCounts,backtest-method}
\title{\code{totalCounts} Method}
\usage{
\S4method{totalCounts}{backtest}(object, low.high.only = FALSE)
}
\arguments{
\item{object}{is the backtest object to work on}

\item{low.high.only}{is a boolean value for whether to only sum the counts of high and low buckets}
}
\value{
a data frame containing sum of total counts for all buckets
}
\description{
The \code{totalCounts} method returns a data frame in the same format as the speads data frame
returned by \code{summaryStats}: contains the sum of counts for all buckets of non-NA
\code{in.var} values that went into the spread calculations. It is different from counts because
it displays the sum of counts from all buckets (or lowest and highest only), thus
allowing for output that matches the format of spreads output.
}
\seealso{
\code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
}

