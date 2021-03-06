% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/backtest.function.R
\name{backtest}
\alias{backtest}
\title{Creating an Object of Class Backtest}
\usage{
backtest(x, in.var, ret.var, universe, by.var = NULL, date.var = NULL,
  id.var = NULL, buckets = 5, natural = FALSE, do.spread = TRUE,
  by.period = TRUE, overlaps = 1)
}
\arguments{
\item{x}{is a data frame containing the data to be analysed in the backtest. See \code{details}
for what this data frame must contain}

\item{in.var}{is a character vector which indicates the name of the column or columns in \code{x}
to be used as input variables}

\item{ret.var}{is a character vector which indicates the name of the column or columns in \code{x}
to be used as return variables}

\item{universe}{is an optional expression for selecting a subset of \code{x}. See \code{details}
for how \code{universe} may be constructed}

\item{by.var}{is an optional character value, specifying a second variable in \code{x} to be used
for categorising the data. See \code{details} for explanation on how categories are created}

\item{date.var}{is an optional character vector which indicates the name of the column in \code{x}
to be used as a date for each observation. \code{date.var} must be specified if
\code{natural} is \code{TRUE}.  In order to call \code{plot}, the contents of \code{date.var}
must be of class \code{Date} or be coercible to an object of class \code{Date} via
\code{as.Date}}

\item{id.var}{is an optional character value which indicates the name of the column in \code{x}
containing a unique identifier for each observation. \code{id.var} must be specified if
\code{natural} is \code{TRUE}}

\item{buckets}{is an optional numeric vector which specifies how many quantiles to create according
to \code{in.var} and \code{by.var}}

\item{natural}{is an optional boolean value.  If \code{TRUE}, the \code{summary} method returns
additional information and the backtest object may be plotted. See details for explanations
on how a natural backtest differs from a pooled backtest}

\item{do.spread}{is a boolean value. If \code{TRUE}, the \code{summary} method displays information
about the spread between the extreme quantiles. If \code{FALSE}, this information is
suppressed. Defaults to \code{TRUE}.}

\item{by.period}{is a boolean value. If \code{TRUE}, the quantiles are recalculated within each
date period. If \code{FALSE}, the quantiles are calculated all at once. Defaults to \code{TRUE}.}

\item{overlaps}{is a numeric value which specifies the number of prior periods to include in the
       current period's portfolio weights calculation. If \code{overlaps} is the default of 1,
       backtest behaves as usual and only uses a period's own data to determine its portfolio.
       If \code{overlaps} is set to \code{n > 1}, a period's portfolio comprises the weighted mean
       of portfolio weights from the previous \code{n} periods, with period \code{n} having a
       weight of \code{1/n}.}
}
\value{
an object of class \code{backtest}. The functions \code{show} and \code{summary} are used
        to obtain and print a short description and longer summary of the results of the
        \code{backtest}.  The accessor functions \code{counts}, \code{totalCounts}, \code{marginals},
        \code{means}, \code{naCounts}, and \code{turnover} extract different parts of the value
        returned by \code{backtest}.
}
\description{
\code{backtest} conducts a backtest and returns the results as an object of class \code{backtest}
}
\details{
Data frames for \code{backtest} must, at a minimum, contain a column of class
         numeric to be referenced by the \code{in.var} and \code{ret.var} arguments.
         The \code{in.var} is the primary variable by which the backtest categorises
         observations.  It must reference a numeric column in \code{x}.  Using the
         values in \code{x}, \code{backtest} breaks the values into equal sized
         quantiles, or \code{buckets}.
         The \code{by.var} is the secondary variable by which the backtest categorises
         observations.  When specifying both \code{in.var} and \code{by.var}, \code{backtest}
         organises the observations into a \code{n} by \code{j} matrix where \code{n} is the
         number of quantiles or categories created for the \code{by.var} and \code{j} is
         the number of quantiles created for the \code{in.var}.  By default,
         \code{backtest} creates \code{5} quantiles.
         If \code{natural} is \code{TRUE}, the data and arguments must meet certain
         requirements.  First, the frequency of the observations and \code{ret.var}
         must be the same.  Second, an \code{id.var} and \code{date.var} are
         required.  Third, a \code{by.var} is not allowed.  Note that the code
         does not verify that the backtest is truly natural; \code{backtest}
         accepts the value passed by the user as valid.
}
\examples{
data(starmine)

## Backtest with 1 'in.var' and 1 'ret.var'
bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
summary(bt)

## Backtest with 2 'in.var' values, 1 'ret.var', and a 'by.var'
bt <- backtest(starmine, in.var = c("smi", "cap.usd"), ret.var = "ret.0.1.m", by.var = "sector",
               by.period = FALSE)
summary(bt)

## Backtest with 1 'in.var', 1 'by.var', and 1 'ret.var'.  Number of
## buckets changed from default of 5 to 4.  Change in number of buckets
## only affects the 'in.var' because the 'by.var' column in 'starmine'
## contains character data. For each value in this column there is a
## unique category.

bt <- backtest(starmine, in.var = "smi", by.var = "sector",
               ret.var = "ret.0.1.m", buckets = 4, by.period = FALSE)
summary(bt)

## Backtest with 1 'in.var', multiple 'ret.var', and a
## universe restriction

bt <- backtest(starmine, in.var = "smi",
ret.var = c("ret.0.1.m", "ret.0.6.m"),
universe = sector == "HiTec", by.period = FALSE)
summary(bt)

## Running a natural backtest with 2 'in.vars', 1 'ret.var' 10 buckets
bt <- backtest(starmine, in.var = c("smi","cap.usd"),
               ret.var = "ret.0.1.m", date.var = "date",
               id.var = "id", buckets = 10,
               natural = TRUE, by.period = FALSE)
summary(bt)

## The same backtest, but calculating quantiles within periods.
bt <- backtest(starmine, in.var = c("smi","cap.usd"),
               ret.var = "ret.0.1.m", date.var = "date",
               id.var = "id", buckets = 10,
               natural = TRUE, by.period = TRUE)
summary(bt)

plot(bt, type = "turnover")
plot(bt, type = "return")
plot(bt, type = "cumreturn")
plot(bt, type = "cumreturn.split")
}
\seealso{
\code{\link{backtest-class}}
}

