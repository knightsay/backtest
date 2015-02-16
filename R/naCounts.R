#' \code{naCounts} Method
#' 
#' The \code{naCounts} method returns a list of matrices, with one matrix for each \code{in.var}, 
#' where the value of each cell is the number of NA observations for that \code{in.var} and 
#' \code{by.var} combination.
#' 
#' @param object is the backtest object to work on
#' 
#' @return a list of matrices containing the number of NA observations
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname naCounts-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' naCounts(bt)

setMethod("naCounts",
          signature(object = "backtest"),
          function(object){
            
            na.list <- list()
            
            for(i in object@in.var){
              na.list <- append(na.list, list(object@results[ ,i, , ,"NAs"]))
            } 
            
            names(na.list) <- object@in.var
            
            na.list
          }
)