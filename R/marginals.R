#' \code{marginals} Method
#' 
#' The \code{marginals} method returns the value of observations for \code{in.var} and 
#' \code{by.var} combination. Different from \code{counts} because the marginal sums have been 
#' appended to the matrices.
#' 
#' @param object is the backtest object to work on
#' 
#' @return \code{marginals} method returns a list of matrices, one matrix for each \code{in.var}
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @export
#' @docType methods
#' @rdname marginals-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' marginals(bt)
#' 
#' @export

setMethod("marginals",
          signature(object = "backtest"),
          function(object){
            
            body <- counts(object)
            
            for(i in 1:length(body)){
              
              if(is.null(dim(body[[i]]))){
                total <- sum(body[[i]], na.rm = TRUE)
                body[[i]] <- append(body[[i]], total)
                names(body[[i]])[length(body[[i]])] <- "TOTAL"
              }
              else{
                total <- rowSums(body[[i]], na.rm = TRUE)
                body[[i]] <- cbind(body[[i]], TOTAL = total)  
                
                total <- colSums(body[[i]], na.rm = TRUE)
                body[[i]] <- rbind(body[[i]], TOTAL = total)
              }
            }
            
            body
          }
)
