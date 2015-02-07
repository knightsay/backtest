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
#'          A \code{backtest} object with a \code{natural} value of TRUE may be
#'          graphed by calling the \code{plot} method.  The default \code{plot}
#'          method graphs return.  The other plots, turnover and
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
#' @seealso \code{\link{backtest}}
#' 
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

## Lists the .vars used in the backtest



#' The show method prints the variables used in this backtest.

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

## Prints a header listing the .vars, then prints the table returned
## by the "spreads" method


#' The summary method prints the results of the backtest.

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


## Returns a data frame summarizing the results of the backtest.  The
## entries of the data frame contain means in cases 1, 2, and 4, and
## spreads in cases 3 and 5.  When a table of means is returned, the
## ".bt.spread" function is called and summary spread data is attached
## to the right side of the data frame.  If a date.var is used,
## ".bt.mean" is called and mean summary data are attached to bottom
## of the data frame.


#' The summaryStats method returns a data frame with spreads for each \code{date.var} value and each
#' \code{in.var}.

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


#' The means method returns a list of matrices, with one matrix for each \code{in.var}, where the
#' value of each cell is the mean of the returns for that \code{in.var} and \code{by.var} combination.

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



#' The \code{ci} method returns a matrix of confidence intervals for spreads

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




#' The \code{plot} method plots returns, cumulative returns, or turnover, when passed a \code{type} 
#' argument of \code{return}, \code{cumreturn}, or \code{turnover}, respectively.

setMethod("plot",
          signature(x = "backtest", y = "missing"),
          function(x, type = "return", ...){

            if(!x@natural){
              stop("Can only plot natural backtests")
            }

            if(type == "return"){
              btplot <- .plot.return(x, ...)
            }
            else if(type == "turnover"){
              btplot <- .plot.turnover(x, ...)
            }
            else if(type == "cumreturn"){
              btplot <- .plot.cumreturn(x, ...)
            }
            else if(type == "cumreturn.split"){
              btplot <- .plot.cumreturn.split(x, ...)
            }
            else{
              stop("Unknown type specified.")
            }

            invisible(btplot)

          }
          )

## Plots turnover for each period specified by "date.var"

.plot.turnover <- function(object, main = "Turnover",
                          xlab = "Date", ylab = "Turnover (%)", ...){
  
  turnovers <- object@turnover[-1,]

  turnovers <- 100 * as.data.frame(turnovers)

  ## Stop informatively if we can't call as.Date on what was the
  ## contents of the date.var column.
  
  if(inherits(try(as.Date(rownames(turnovers)), silent = TRUE), "try-error")){
    stop(paste("Error converting dates: Please ensure contents of date.var",
               "column cate be converted to Date's via as.Date."))
  }
  datevar <- as.Date(rownames(turnovers))
  
  invars <- names(turnovers)[1]
  
  if(length(names(turnovers)) > 1){
    for(i in 2:length(names(turnovers))){
      invars <- paste(invars, "+",  names(turnovers)[i])
    }
  }
  
  form <- as.formula(paste(invars, "~", "datevar"))
  
  btplot <- xyplot(form, turnovers, type = "b", horizontal = FALSE,
                  auto.key = TRUE, scales = list(tick.number = 10),
          ylim = c(0, 100), main = main, xlab = xlab, ylab = ylab, ...)

  print(btplot)

  return(btplot)
}

## Plots returns for each period specified by "date.var"

.plot.return <- function(object, main = "Spread Return",
                         xlab = "Date", ylab = "Return (%)", ...){
  
  spreads <- object@results[1, , ,dim(object@results)[4],1] -
    object@results[1, , ,1,1]

  ## Fix for 1D array
  
  if(is.null(dim(spreads))){
    spreads <- array(spreads, dim = c(1, length(spreads)),
                     dimnames = list(object@in.var,
                       names(spreads)))
  }

  spreads <- 100 * data.frame(t(spreads))  
  invars <- names(spreads)[1]
  
  if(length(names(spreads)) > 1){
    for(i in 2:length(names(spreads))){
      invars <- paste(invars, "+",  names(spreads)[i])
    }
  }
  
  form <- as.formula(paste(invars, "~", "date"))


  spreads$date <- rownames(spreads)

  btplot <- barchart(form, spreads, horizontal = FALSE, stack = FALSE,
                     auto.key = TRUE,
                     scale = list(
                       y = list(tick.number = 10),
                       x = list(tick.number = 10, rot = 90)),
                     origin = 0,
                     main = main, xlab = xlab, ylab = ylab, ...)
  
  print(btplot)

  return(btplot)
  
}

.plot.cumreturn <- function(object, main = "Backtest Fanplot", xlab = "Date",
                            ylab = "Cumulative Returns", ...){
  
  ## Function to compute cumsums of a matrix, by columns
  
  cum.matrix <- function(x){
    for(i in 1:dim(x)[2]){
      x[ ,i] <- cumsum(x[ ,i])
    }
    x
  }
    
  returns <- means(object)
  
  ## Calculate cumulative returns
  
  returns <- lapply(returns, cum.matrix)
  
  new.returns <- data.frame()
  
  for(i in 1:length(returns)){

    this.returns <- data.frame(returns[[i]])
    
    ## We need quantile names that don't begin with a number so that
    ## the formula used with lattice graphics works.  But, the "Xn"
    ## naming scheme that is data.frame's default needs to get changed
    ## to "Qn".
    
    names(this.returns) <- gsub("X([1-9]+)", "Q\\1",
                                names(this.returns),
                                perl = TRUE)

    this.returns$group <- names(returns)[i]

    if(inherits(try(as.Date(rownames(this.returns)), silent = TRUE), "try-error")){
      stop(paste("Error converting dates: Please ensure contents of date.var",
                 "column cate be converted to Date's via as.Date."))
    }

    this.returns$date <- as.Date(rownames(this.returns))
    new.returns <- rbind(new.returns, this.returns)
  }

  invars <- names(new.returns)[1:(length(names(new.returns)) - 2)]
  lhs    <- paste(rev(invars), collapse = " + ")
  form   <- as.formula(paste(lhs, "~ date | group"))
  
  btplot <- xyplot(form, new.returns,
                   type  = "l", horizontal = FALSE,
                   auto.key = list(
                     space     = "right",
                     points    = FALSE,
                     lines     = TRUE,
                     cex       = 0.75,
                     title     = "quantile",
                     cex.title = 1),
                   as.table = TRUE,
                   main     = "Cumulative Quantile Return",
                   xlab     = xlab,
                   ylab     = ylab, ...)

  print(btplot)
  
  return(btplot)
  
}

.plot.cumreturn.split <- function(object,
                                  xlab = "Date",
                                  ylab = "Return (%)", ...){

  ## No notion of spread return without 2 or more buckets.

  stopifnot(isTRUE(object@buckets[1] >= 2))

  ## Data setup.  Calculate cumulative returns and spread returns
  ## before plotting.
  
  ## Function to compute the cumulative return of each quantile, and
  ## the spread cumulative return.  Since we're dealing with portfolio
  ## return and fixed theoretical assets, we take a cumulative sum
  ## without compounding.

  ## Also, convert return into percent.
  
  cumulate <- function(x){
    x <- data.frame(x)

    for(i in 1:ncol(x)){
      x[[i]] <- 100 * cumsum(x[[i]])
    }

    x$spread <- x[[ncol(x)]] - x[[1]]
    
    x
  }

  returns <- means(object)

  ## Calculate cumulative returns
  
  returns <- lapply(returns, cumulate)
  
  new.returns <- data.frame()
  
  for(i in 1:length(returns)){
    this.returns <- returns[[i]]

    ## We need quantile names that don't begin with a number so that
    ## the formula used with lattice graphics works.  But, the "Xn"
    ## naming scheme that is data.frame's default needs to get changed
    ## to "Qn".
    
    names(this.returns) <- gsub("X([1-9]+)", "Q\\1",
                                names(this.returns),
                                perl = TRUE)
    
    this.returns$group <- names(returns)[i]

    if(inherits(try(as.Date(rownames(this.returns)), silent = TRUE), "try-error")){
      stop(paste("Error converting dates: Please ensure contents of date.var",
                 "column cate be converted to Date's via as.Date."))
    }

    this.returns$date <- as.Date(rownames(this.returns))
    new.returns <- rbind(new.returns, this.returns)
  }

  ## Plotting
  ##
  ## There are two regions to this plot, the top plot of spread return
  ## and the bottom, larger plot of cumulative quantile return.

  ## Compute ylim as 50% more space than we would need normally.

  y.max <- max(new.returns$spread)
  y.min <- min(new.returns$spread)
  y.dist <- y.max - y.min
  ylim <- c(y.min - 0.5 * y.dist, y.max + 0.05 * y.dist)

  ## Panel function
  
  top.panel <- function(x, y, subscripts, ...){


    
    op <- options(digits = 2)

    ## Annotate panel with total return.

    pushViewport(viewport(x = 0.05, y = 0.95))
    grid.text(paste("Total return:", format(tail(y, n = 1))),
              hjust = 0, vjust = 1, gp = gpar(cex = 0.75))
    popViewport()

    ## Annotate panel with worst drawdown.  Remember, y is now in pct.
    
    dd       <- 1 - (y/100 + 1)/cummax(y/100 + 1)
    dd.max   <- max(dd)

    ## If there are two troughs of equal value the below makes the
    ## largest drawdown extend to the furthest trough.

    dd.end.idx   <- max(which.max(dd), length(dd) - which.max(rev(dd)) + 1)
    dd.end       <- x[dd.end.idx]
    dd.start.idx <- max(which(dd == 0 & seq(dd) < dd.end.idx))
    dd.start     <- x[dd.start.idx]
    dd.ret       <- y[dd.end.idx] - y[dd.start.idx]

    if(all(dd == 0)) dd.ret <- "--"
    
    pushViewport(viewport(x = 0.05, y = 0.05))
    grid.text(paste("Worst drawdown:", format(dd.ret)),
              hjust = 0, vjust = 0, gp = gpar(cex = 0.75))
    popViewport()

    panel.xyplot(x, y, col.line = "black", ...)

    if(!all(dd == 0)){
      panel.lines(x[dd.start.idx:dd.end.idx], y = y[dd.start.idx:dd.end.idx],
                  col ="red", default.units = "native",...)
    }

    options(op)
  }
  
  ## I'm fixing the layout of the top and bottom plots to have one row
  ## of panels.  The layout of date labels doesn't seem to fare very
  ## well using a layout other than the default, however.  That said,
  ## this plot is most useful with a couple in.var's.  I should allow
  ## the user to be able to modify such layout characteristics.
  
  top.plot <- xyplot(formula("spread ~ date | group"), new.returns,
                     panel = top.panel,
                     type       = "l",
                     horizontal = FALSE,
                     as.table   = TRUE,
                     layout = c(length(object@in.var),1),
                     scale      = list(x = list(alternating = 1)),
                     main       = "Cumulative Spread Return",
                     ylim       = ylim,
                     xlab       = NULL,
                     ylab       = ylab,
                     ...)
  
  ## The bottom plot doesn't use spread, so subtract the last _3_
  ## columns when computing in.var's.
  
  invars <- names(new.returns)[1:(length(names(new.returns)) - 3)]
  lhs    <- paste(rev(invars), collapse = " + ")
  form   <- as.formula(paste(lhs, "~ date | group"))

  bottom.panel <- function(x, y, subscripts, groups, ...){
    panel.superpose(x, y, subscripts, groups, ...)
  }
  
  bottom.plot <- xyplot(form, new.returns,
                        panel = bottom.panel,
                        type  = "l", horizontal = FALSE,
                        auto.key = list(
                          x         = 0.1,
                          y         = 0.8,
                          corner    = c(0,1),
                          points    = FALSE,
                          lines     = TRUE,
                          cex       = 0.75,
                          title     = "quantile",
                          cex.title = 1),
                        as.table = TRUE,
                        layout   = c(length(object@in.var),1),
                        scale    = list(x = list(alternating = 1)),
                        main     = "Cumulative Quantile Return",
                        xlab     = xlab,
                        ylab     = ylab, ...)


  ## Calculate panel width.  Assuming 0.15 npc used for margins (not a
  ## very safe assumption).  So far I don't have any other good way to
  ## ensure that the aspects of the panels in the two lattice plots
  ## are the same.

  panel.width <- 0.85 / length(object@in.var)

  print(top.plot, position = c(0, 0.6, 1, .98),
        panel.width = list(panel.width, "npc"), more = TRUE)
  print(bottom.plot, position = c(0, 0,   1, 0.6),
        panel.width = list(panel.width, "npc"))
  
}
