#' \code{Summary} method
#'          
#' The \code{summary} method prints the results of the backtest. It first prints a header listing 
#' the \code{.vars} variables, then prints the table returned by the \code{spreads} method. 
#' 
#' @note \code{summary} provides different displays depending on the type of \code{backtest} object.
#'
#' @param object is the backtest object we want summary of
#' @param ... is the additional arguments to pass on
#' 
#' @seealso \code{\link{backtest}} and \code{\link{backtest-class}}
#' 
#' @export
#' @docType methods
#' @rdname summary-methods
#'
#' @examples
#' data(starmine)
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' summary(bt)

setMethod("summary",
          signature(object = "backtest"),
          function(object, ...){
            
            ## Header
            
            cat("Backtest conducted with:\n\n",
                length(object@in.var), ifelse(length(object@in.var) == 1,
                                              " in.var: ", " in.vars: "),
                paste(object@in.var, collapse = ", "), ";\n",
                length(object@ret.var), ifelse(length(object@ret.var) == 1,
                                               " ret.var: ", " ret.vars: "),
                paste(object@ret.var, collapse = ", "), ";\nand ",
                ifelse(length(object@by.var) > 0,
                       paste("by.var: ", object@by.var, sep = ""), "no by.var"), ";\n",
                ifelse(isTRUE(object@do.spread),
                       "do.spread: TRUE", "do.spread: FALSE"), ";\n",
                ifelse(isTRUE(object@by.period),
                       "by.period: TRUE", "by.period: FALSE"),
                ".\n\n", sep = "")
            
            ## Matrix
            
            print(summaryStats(object)[, !names(summaryStats(object))
                                       %in% c("CI(low)",
                                              "CI(high)",
                                              "TURNOVER")])
            cat("\n")
            
            if(isTRUE(object@do.spread)){
              if(object@natural){
                if(length(object@in.var) == 1){
                  
                  cat("average turnover: ", .bt.mean(turnover(object)),
                      "\nmean spread: ", mean(summaryStats(object)[, "spread"]),
                      "\nsd spread: ", sd(summaryStats(object)[, "spread"]),
                      "\nraw sharpe ratio: ", .bt.sharpe(object), "\n\n",
                      sep = "")
                }
                else{
                  
                  for(i in object@in.var){
                    
                    x <- summaryStats(object)
                    cat("summary stats for in.var = ", i,
                        ":\n\naverage turnover: ",
                        .bt.mean(turnover(object))[1,i],
                        "\nmean spread: ",
                        colMeans(summaryStats(object), na.rm = TRUE)[i],
                        "\nsd spread: ", sd(x[,i], na.rm = TRUE),
                        "\nraw sharpe ratio: ", .bt.sharpe(object)[,i],
                        "\n\n", sep = "")
                  }
                }
              }
            }
            
          }
)