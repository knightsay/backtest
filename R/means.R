#' \code{means} Method
#' 
#' The \code{means} method returns a list of matrices which show the mean of returns
#' 
#' @param object is the backtest object we want mean of returns of
#' 
#' @return a list of matrices of mean of returns
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname means-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' means(bt)

#' The means method returns a list of matrices, with one matrix for each \code{in.var}, where the
#' value of each cell is the mean of the returns for that \code{in.var} and \code{by.var} combination.
#' @export

setMethod("means",
          signature(object = "backtest"),
          function(object){
            
            mean.list <- list()
            
            for(i in object@in.var){
              mean.list <- append(mean.list,
                                  list(object@results[ ,i, , ,"means"]))
            }
            
            names(mean.list) <- object@in.var
            
            mean.list
          }
)
