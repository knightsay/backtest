#' Class \code{backtest}
#' 
#' \code{backtest} class contains results from the backtest function. Objects can be created by calls
#' to the function \code{backtest(data,in.var, ret.var, ...)}.
#' 
#' @slot in.var is a character string which specifies the \code{in.var} values for this backtest.
#' @slot ret.var is a character string that contains the \code{ret.var} values for this backtest.
#' @slot by.var is a character string which contains the \code{by.var}, if specified, for this backtest. 
#' @slot date.var is a character string containing the \code{date.var}, if specified, for this backtest. 
#' @slot buckets is a numeric type containing the number(s) of buckets used to create quantiles from
#'       the \code{in.var} and \code{by.var} values.
#' @slot results is a 5-dimensional \code{vector} containing the results of the backtest.
#' @slot ret.stats is a vector containing the returned statistics for the backtest.  
#' @slot turnover is a vector containing turnover statistics for the backtest.
#' @slot natural is a boolean value which states whether the intervals between observations, 
#'       as specified by \code{date.var}, and returns, as specified by \code{ret.var}, match.  
#'       If the interval between dates is one month, the interval between returns should also be one 
#'       month.
#' @slot do.spread is a boolean value. If \code{TRUE}, the \code{summary} method displays information
#'       about the spread between the extreme quantiles. If \code{FALSE}, this information is 
#'       suppressed.  Defaults to \code{TRUE}.
#' @slot by.period is a boolean value. If \code{TRUE}, the quantiles are
#'       recalculated within each date period.  If \code{FALSE}, the quantiles are
#'       calculated all at once.  Defaults to \code{TRUE}.
#' @slot overlaps is a numeric type which specifies
#'       the number of prior periods to include in the current period's
#'       portfolio weights calculation. If \code{overlaps} is the default of \code{1},
#'       backtest behaves as usual and only uses a periods own data to
#'       determine its portfolio.  If \code{overlaps} is set to
#'       \code{n > 1}, a period's portfolio comprises the weighted mean of
#'       portfolio weights from the previous n periods, with period \code{n}
#'       having a weight of \code{1/n}.
#' 
#' @details The primary method for accessing the \code{backtest} results is through
#'          the \code{summary} method.  \code{summary} provides different displays
#'          depending on the type of \code{backtest} object.  These displays are
#'          shown in the examples section.  Accessor methods such as \code{means},
#'          \code{counts}, \code{marginals}, \code{naCounts}, \code{turnover}, and
#'          \code{ci} may be used to extract other types of information from the object.
#'          A \code{backtest} object with a \code{natural} value of \code{TRUE} may be
#'          graphed by calling the \code{plot} method.  The default \code{plot}
#'          method graphs return plot.  The other plots, turnover and
#'          cumulative return, must be explicitly specified as \code{plot(object,
#'          type = "turnover")} or \code{plot(object, type = "cumreturn")}.
#'          The \code{backtest} object does not store the data frame used to create
#'          the \code{backtest.}  It only stores the results and the names of the
#'          vectors used in calculating these results.
#'          The results of a \code{backtest} are stored in a 5-dimensional array,
#'          \code{results}.  The 1st dimension contains one value for every element
#'          of \code{ret.var}.  The 2nd dimension contains one value for
#'          every element of \code{in.var}.  The 3rd dimension contains one value
#'          for every element in \code{1:buckets[1]}, a vector from 1 through the
#'          number of \code{by.var} buckets.  The 4th dimension contains one value
#'          for every element in \code{1:buckets[2]}, a vector from 1 through the
#'          number of \code{in.var} buckets.  The 5th dimension contains 4
#'          elements: \code{means}, \code{counts}, \code{trim.means}, and \code{NAs}.  
#'          
#' @seealso \code{\link{backtest}} \code{\link{summary}}
#' @export
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' 
#' ## Summary for a pooled backtest
#' summary(bt)
#' 
#' ## A natural backtest
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m",
#'                date.var = "date", id.var = "id", natural = TRUE, by.period = FALSE)
#'                
#' ## Summary for a natural backtest
#' summary(bt)
#' 
#' ## Other access methods
#' means(bt)
#' counts(bt)
#' marginals(bt)
#' naCounts(bt)
#' 
#' ## Plotting methods
#' plot(bt, type = "turnover")
#' plot(bt, type = "return")
#' plot(bt, type = "cumreturn")

setClass("backtest", representation(in.var        = "character",
                                    ret.var       = "character",
                                    by.var        = "character",
                                    date.var      = "character",
                                    buckets       = "numeric",
                                    results       = "array",
                                    ret.stats     = "array",
                                    turnover      = "array",
                                    natural       = "logical",
                                    do.spread     = "logical",
                                    by.period     = "logical",
                                    overlaps      = "numeric"),
         
         prototype(in.var        = "in.var",
                   ret.var       = "ret.var",
                   by.var        = character(),
                   date.var      = character(),
                   buckets       = numeric(),
                   results       = array(dim = c(1,1,1,1,1)),
                   ret.stats     = array(dim = c(1,6)),
                   turnover      = array(),
                   natural       = FALSE,
                   do.spread     = TRUE,
                   by.period     = TRUE,
                   overlaps      = 1),

         validity = function(object){

           if(length(object@in.var) == 0){
             return("in.var not specified")
           }

           if(length(object@ret.var) == 0){
             return("ret.var not specified")
           }

           if(length(object@by.var) > 0 && length(object@date.var) > 0){
             return("Both by.var and date.var specified")
           }

           if(length(dim(object@results)) != 5){
             return("Incorrect number of dimensions in results slot")
           }

           return(TRUE)
         }
         )

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



#' The \code{counts} method returns a list of matrices, with one matrix for each \code{in.var}, 
#' where the value of each cell is the number of observations for that \code{in.var} and 
#' \code{by.var} combination.
#' @export

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


#' The \code{totalCounts} method returns a data frame in the same format as the speads data frame 
#' returned by \code{summaryStats}: contains the sum of counts for all buckets (or high and 
#' low buckets if argument \code{low.high.only} is set to TRUE) of non-NA
#' \code{in.var} values that went into the spread calculations. It is different from counts because 
#' it displays the sum of counts from all buckets (or lowest and highest only), thus
#' allowing for output that matches the format of spreads output.
#' @export

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



#' The \code{marginals} method returns a list of matrices, one matrix for each \code{in.var}, 
#' where the value of each cell is the number of observations for that \code{in.var} and 
#' \code{by.var} combination. Different from \code{counts} because the marginal sums have been 
#' appended to the matrices.
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



#' The \code{naCounts} method returns a list of matrices, with one matrix for each \code{in.var}, 
#' where the value of each cell is the number of NA observations for that \code{in.var} and 
#' \code{by.var} combination.
#' @export

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



#' The \code{turnover} method returns a \code{data.frame} of the turnovers if the \code{backtest} 
#' is \code{natural}. Passing a \code{mean} argument will append the mean of the turnover(s) as the 
#' last row of the matrix
#' @export

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