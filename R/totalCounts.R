#' \code{totalCounts} Method
#' 
#' The \code{totalCounts} method returns a data frame in the same format as the speads data frame 
#' returned by \code{summaryStats}: contains the sum of counts for all buckets of non-NA
#' \code{in.var} values that went into the spread calculations. It is different from counts because 
#' it displays the sum of counts from all buckets (or lowest and highest only), thus
#' allowing for output that matches the format of spreads output.
#' 
#' @param object is the backtest object to work on
#' @param low.high.only is a boolean value for whether to only sum the counts of high and low buckets
#' 
#' @return a data frame containing sum of total counts for all buckets
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname totalCounts-methods

setMethod("totalCounts",
          signature(object = "backtest"),
          function(object, low.high.only = FALSE){
            
            counts <- data.frame(do.call("cbind",
                                         lapply(counts(object), function(x){
                                           if(isTRUE(low.high.only))
                                             x <- x[c("low", "high")]
                                           rowSums(x)
                                         })))
            
            counts
          }
)