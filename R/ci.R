#' \code{ci} Method for Confidence Interval
#' 
#' The \code{ci} method returns a matrix of confidence intervals for spreads
#' 
#' @param object is the backtest object to work on
#' 
#' @return a matrix of confidence intervals for spreads
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname ci-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' ci(bt)


setMethod("ci",
          signature(object = "backtest"),
          function(object){
            
            ##          array(dim = c(1, length(object@ret.var), length(object@in.var)))
            
            if((length(object@in.var) == 1 && length(object@ret.var) == 1)
               || (length(object@in.var) == 1 && length(object@ret.var > 1))){
              
              summaryStats(object)[, c("CI(low)", "spread", "CI(high)")]
            }
            
          }
)