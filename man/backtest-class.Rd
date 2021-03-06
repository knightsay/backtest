% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/backtest.R
\docType{class}
\name{backtest-class}
\alias{backtest-class}
\title{Class \code{backtest}}
\description{
\code{backtest} class contains results from the backtest function. Objects can be created by calls
to the function \code{backtest(data,in.var, ret.var, ...)}.
}
\details{
The primary method for accessing the \code{backtest} results is through
         the \code{summary} method.  \code{summary} provides different displays
         depending on the type of \code{backtest} object.  These displays are
         shown in the examples section.  Accessor methods such as \code{means},
         \code{counts}, \code{marginals}, \code{naCounts}, \code{turnover}, and
         \code{ci} may be used to extract other types of information from the object.
         A \code{backtest} object with a \code{natural} value of \code{TRUE} may be
         graphed by calling the \code{plot} method.  The default \code{plot}
         method graphs return plot.  The other plots, turnover and
         cumulative return, must be explicitly specified as \code{plot(object,
         type = "turnover")} or \code{plot(object, type = "cumreturn")}.
         The \code{backtest} object does not store the data frame used to create
         the \code{backtest.}  It only stores the results and the names of the
         vectors used in calculating these results.
         The results of a \code{backtest} are stored in a 5-dimensional array,
         \code{results}.  The 1st dimension contains one value for every element
         of \code{ret.var}.  The 2nd dimension contains one value for
         every element of \code{in.var}.  The 3rd dimension contains one value
         for every element in \code{1:buckets[1]}, a vector from 1 through the
         number of \code{by.var} buckets.  The 4th dimension contains one value
         for every element in \code{1:buckets[2]}, a vector from 1 through the
         number of \code{in.var} buckets.  The 5th dimension contains 4
         elements: \code{means}, \code{counts}, \code{trim.means}, and \code{NAs}.
}
\section{Slots}{

\describe{
\item{\code{in.var}}{is a character string which specifies the \code{in.var} values for this backtest.}

\item{\code{ret.var}}{is a character string that contains the \code{ret.var} values for this backtest.}

\item{\code{by.var}}{is a character string which contains the \code{by.var}, if specified, for this backtest.}

\item{\code{date.var}}{is a character string containing the \code{date.var}, if specified, for this backtest.}

\item{\code{buckets}}{is a numeric type containing the number(s) of buckets used to create quantiles from
the \code{in.var} and \code{by.var} values.}

\item{\code{results}}{is a 5-dimensional \code{vector} containing the results of the backtest.}

\item{\code{ret.stats}}{is a vector containing the returned statistics for the backtest.}

\item{\code{turnover}}{is a vector containing turnover statistics for the backtest.}

\item{\code{natural}}{is a boolean value which states whether the intervals between observations,
as specified by \code{date.var}, and returns, as specified by \code{ret.var}, match.
If the interval between dates is one month, the interval between returns should also be one
month.}

\item{\code{do.spread}}{is a boolean value. If \code{TRUE}, the \code{summary} method displays information
about the spread between the extreme quantiles. If \code{FALSE}, this information is
suppressed.  Defaults to \code{TRUE}.}

\item{\code{by.period}}{is a boolean value. If \code{TRUE}, the quantiles are
recalculated within each date period.  If \code{FALSE}, the quantiles are
calculated all at once.  Defaults to \code{TRUE}.}

\item{\code{overlaps}}{is a numeric type which specifies
      the number of prior periods to include in the current period's
      portfolio weights calculation. If \code{overlaps} is the default of \code{1},
      backtest behaves as usual and only uses a periods own data to
      determine its portfolio.  If \code{overlaps} is set to
      \code{n > 1}, a period's portfolio comprises the weighted mean of
      portfolio weights from the previous n periods, with period \code{n}
      having a weight of \code{1/n}.}
}}
\examples{
data(starmine)
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)

## Summary for a pooled backtest
summary(bt)

## A natural backtest
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m",
               date.var = "date", id.var = "id", natural = TRUE, by.period = FALSE)

## Summary for a natural backtest
summary(bt)

## Other access methods
means(bt)
counts(bt)
marginals(bt)
naCounts(bt)

## Plotting methods
plot(bt, type = "turnover")
plot(bt, type = "return")
plot(bt, type = "cumreturn")
}
\seealso{
\code{\link{backtest}} \code{\link{summary}}
}

