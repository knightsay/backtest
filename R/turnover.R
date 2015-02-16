#' \code{turnover} Method
#' 
#' The \code{turnover} method returns a \code{data.frame} of the turnovers if the \code{backtest} 
#' is \code{natural}. 
#' 
#' @param object is the backtest object to work on
#' @param mean is a boolean value for whether append the mean of the turnover(s) as the last row of matrix
#' 
#' @return a \code{data.frame} of the turnovers if the \code{backtest} 
#'         is \code{natural}
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname turnover-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE, natural = TRUE,
#'                date.var = "date", id.var = "id")
#' turnover(bt)

setMethod("turnover",
          signature(object = "backtest"),
          function(object, mean = FALSE){
            
            if(!isTRUE(object@natural)){
              stop("Cannot calculate turnover if not a natural backtest.")
            }
            
            if(isTRUE(mean)){
              return(rbind(object@turnover, .bt.mean(turnover(object))))
            }
            
            object@turnover
            
          }
)