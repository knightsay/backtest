#' \code{Show} method
#' 
#' The \code{show} method prints a list of \code{.var} variables used in this backtest
#' 
#' @inheritParam summary
#' 
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname show-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' show(bt)

setMethod("show",
          signature(object = "backtest"),
          function(object){
            
            cat("An object of class backtest:\nin.vars: ",
                paste(object@in.var, collapse = ", "), " (buckets: ",
                object@buckets[1], ")\n",
                sep = "")
            
            if(length(object@by.var) > 0){
              cat("by.var: ", object@by.var, " (buckets: ",
                  object@buckets[2], ")\n", sep = "")
            }
            
            if(length(object@date.var) > 0){
              cat("date.var: ", object@date.var, " (buckets: ",
                  object@buckets[2], ")\n", sep = "")
            }
            
            cat("ret.vars: ", paste(object@ret.var, collapse = ", "),
                "\n", sep = "")
          }
)
