#' \code{counts} Method
#' 
#' The \code{counts} method returns a list of matrices, with one matrix for each \code{in.var}, 
#' where the value of each cell is the number of observations for that \code{in.var} and 
#' \code{by.var} combination.
#' 
#' @param object is the backtest object to work on
#' 
#' @return returns a list of matrices, with one matrix for each \code{in.var}
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname counts-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' counts(bt)

setMethod("counts",
          signature(object = "backtest"),
          function(object){
            
            count.list <- list()
            
            for(i in object@in.var){
              count.list <- append(count.list,
                                   list(object@results[1,i, , ,"counts"]))
            }
            
            names(count.list) <- object@in.var
            
            count.list
          }
)