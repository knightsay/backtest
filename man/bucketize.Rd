% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/bucketize.R
\name{bucketize}
\alias{bucketize}
\title{Constructs a summary data frame}
\usage{
bucketize(xr, x.factor, y.factor, compute, ...)
}
\arguments{
\item{x.factor}{is a factor for \code{x}}

\item{y.factor}{is a factor for \code{x}}

\item{compute}{is a function to apply on the list}

\item{...}{additional arguments to be passed on}

\item{x}{is a numeric vector}
}
\value{
a two-dimensional array of the results, with the levels of \code{x.factor}
        and \code{y.factor} as dimnames.
}
\description{
\code{bucketize} constructs a summary data frame; it divides the values of "x"
into rows by \code{y.factor} and columns by \code{x.factor}, and performs the
\code{compute} function for each group
}

