#' \code{summaryStats} method
#' 
#' The \code{summaryStats} method returns a data frame with spreads for each \code{date.var} value and each
#' \code{in.var}.
#' 
#' @param object is the backtest object we want summary of
#' @param mean is a boolean value for whether to return a table of means by calling \code{.bt.mean}
#' 
#' @return a data frame summarizing the results of the backtest.  The entries of the data frame 
#' contain means in cases 1, 2, and 4, and spreads in cases 3 and 5. 
#' 
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}} and \code{\link{summary}}
#' 
#' @note When a table of means is returned, the \code{.bt.spread} function is called and summary spread 
#'       data is attached to the right side of the data frame.  If a date.var is used, 
#'       \code{.bt.mean} is called and mean summary data are attached to bottom of the data frame.
#' 
#' @export
#' @docType methods
#' @rdname summaryStats-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' summaryStats(bt)

setMethod("summaryStats",
          signature(object = "backtest"),
          function(object, mean = FALSE){
            
            num.in <- length(object@in.var)
            num.ret <- length(object@ret.var)
            
            if(length(object@by.var > 0)){
              by.present <- TRUE
              date.present <- FALSE
            }
            else{
              by.present <- FALSE
              if(length(object@date.var > 0)){
                date.present <- TRUE
              }
              else{
                date.present <- FALSE
              }
            }
            
            ## Case 1: 1 in.var, 1 ret.var
            
            if(num.in == 1 && num.ret == 1){
              
              output <- object@results[1,1, , ,"means"]
              
              n <- object@results[1,1, , ,"counts"]
              
              ## When one dimension of a 2D matrix has length 1,
              ## the subsets above return vectors. 
              
              if(is.null(dim(output))){
                output <- array(output, dim = c(1, length(output)),
                                dimnames = list("pooled", names(output)))
                
                n <- array(n, dim = dim(output),
                           dimnames = dimnames(output))
              }
              if(isTRUE(object@do.spread)){
                spread <- .bt.spread(output, n, object@ret.stats[1,"sd"])
                output <- cbind(output, spread)
              }
              if(object@natural){
                turnover <- object@turnover
                dimnames(turnover)[[2]] <- "TURNOVER"
                output <- cbind(output, turnover)
                
                output <- rbind(output, .bt.mean(output))
              }
              
            }
            
            ## Case 2: multiple in.vars, no by.var
            
            if(num.in > 1 && num.ret == 1 && !by.present &&
                 !date.present){
              
              output <- object@results[1, ,1, ,"means"]
              
              n <- object@results[1, ,1, ,"counts"]
              
              spread <- .bt.spread(output, n, object@ret.stats[1,"sd"])
              
              output <- cbind(output, spread)
            }
            
            ## Case 3: multiple in.vars, with by.var or date.var
            
            if(num.in > 1 && num.ret == 1 && (by.present || date.present)){
              
              output <- t(object@results[1, , ,dim(object@results)[4],1] -
                            object@results[1, , ,1,1])
              
              
              
              if(date.present && mean){
                output <- rbind(output, .bt.mean(output))
              }
              
            }
            
            ## Case 4: single in.var, multiple ret.vars
            
            if(num.in == 1 && num.ret > 1){
              
              output <- object@results[ ,1,1, ,"means"]
              
              n <- object@results[ ,1,1, ,"counts"]
              if(isTRUE(object@do.spread)){
                spread <- .bt.spread(output, n, object@ret.stats[ ,"sd"])
                
                output <- cbind(output, spread)
              }
            }
            
            ## Case 5: multiple in.vars and ret.vars
            
            if(num.in > 1 && num.ret > 1){
              
              output <- object@results[ , ,1,dim(object@results)[4],1] -
                object@results[ , ,1,1,1]
            }
            
            x <- data.frame(output)
            names(x) <- dimnames(output)[[2]]
            x
          }
)