#' Returns Backtest Object
#' 
#' \code{backtest.compute} returns a backtest object
#' 
#' @inheritParams backtest
#' @param in.factor is the \code{in.var} of \code{x} transformed by \code{backtest} into factors.
#' @param by.specified
#'
#' @return a backtest object


backtest.compute <- function(x,
                             in.factor,
                             ret.var,
                             by.var       = NULL,
                             date.var     = NULL,
                             natural      = FALSE,
                             by.specified = NULL,
                             do.spread    = TRUE,
                             id.var       = NULL,
                             by.period    = TRUE,
                             overlaps     = 1){

  ## All in.var must have the same number of buckets.
  temp.in.factor = in.factor

  
  if(!isTRUE(length(unique(sapply(in.factor, function(x){length(levels(x))}))) == 1)){
    stop("All in.var's must have the same number of buckets.")
  }
  
  ## ret.var columns must be numeric

  buckets <- length(levels(in.factor[[1]]))
  
  ## Function for bucketing NA values
  
  na.count <- function(x){
    return(sum(is.na(x)))
  }
  
  ## Creating names
  ## create names for what?...

   by.names <- levels(x$by.factor)
  	
  ## Create array for storing turnover.  Dimensions signify:
  ## 1. date
  ## 2. in.var

  if(natural){
    turnover <- array(dim = c(length(levels(x$by.factor)), length(colnames(in.factor))),
                      dimnames = list(levels(x$by.factor), colnames(in.factor)))
  }  else{
    turnover <- array()
  }
  
  ## Construct the empty array.  The dimensions are ordered as follows:
  ## 1: ret.var(s)
  ## 2: in.var(s)  
  ## 3: by.var buckets
  ## 4: in.var buckets
  ## 5: means/counts/trim.means/NAs

  results <- array(dim = c(length(ret.var), length(colnames(in.factor)),
                           length(levels(x$by.factor)), buckets, 4), 
                   dimnames = list(ret.var, colnames(in.factor), by.names,
                     levels(in.factor[[1]]), c("means", "counts", "trim.means", "NAs")))
                     
  ## Construct ret.stats array -- stats to return

  ret.stats <- array(dim = c(length(ret.var), 6), dimnames =
                     list(ret.var, c("min", "max", "mean", "median",
                                     "sd", "NA")))                     
  for(r in ret.var){

    ## Trim out the most extreme 0.5% of ret.var values in x

    x[[r]] <- x[[r]] * x[["weight"]]
    trim.range <- quantile(x[[r]], c(0.0025, 0.9975), na.rm = TRUE)
    
    ## apply trim.range to x and in.factor
    trim.x <- subset(x, trim.range[[1]] < x[[r]] &
                               x[[r]] < trim.range[[2]])
    
    trim.in.factor <- subset(in.factor, trim.range[[1]] < x[[r]] &
                               x[[r]] < trim.range[[2]])

    ## Store ret.stats
    
    ret.stats[r,"min"]    <- min(x[[r]], na.rm = TRUE)
    ret.stats[r,"max"]    <- max(x[[r]], na.rm = TRUE)
    ret.stats[r,"mean"]   <- mean(x[[r]], na.rm = TRUE)
    ret.stats[r,"median"] <- median(x[[r]], na.rm = TRUE)
    ret.stats[r,"sd"]     <- sd(x[[r]], na.rm = TRUE)
    ret.stats[r,"NA"]     <- sum(is.na(x[[r]]))

    ## Select in.var
    for(i in colnames(in.factor)){
      
      ## Bucketize means
 
      results[r,i, , ,"means"] <- bucketize(x[[r]], x.factor = temp.in.factor[[i]],
                                            y.factor = x$by.factor,
                                            compute = weighted.mean, na.rm = TRUE)

      
      ## Bucketize counts
      
      results[r,i, , ,"counts"] <- bucketize(x[[r]], x.factor = in.factor[[i]],
                                             y.factor = x$by.factor,
                                             compute = length)
      
      ## Bucketize trim.means
      
      results[r,i, , ,"trim.means"] <- bucketize(trim.x[[r]], x.factor =
                                                 trim.in.factor[[i]], y.factor =
                                                 trim.x$by.factor, compute
                                                 = weighted.mean, na.rm = TRUE)

      ## Bucketize NAs

      results[r,i, , ,"NAs"] <- bucketize(x[[r]], x.factor = in.factor[[i]],
                                          y.factor = x$by.factor,
                                          compute = na.count)
      
      ## Calculate Turnover

      if(natural){
        turnover[, i] <- calc.turnover(x[[id.var]],
                                       portfolio.factor = in.factor[[i]],
                                       date.factor = x$by.factor)
      }
    }
  }
    
  ## Create and return backtest object
 
  invisible(new("backtest", in.var = colnames(in.factor), ret.var = ret.var, by.var =
                as.character(by.specified), date.var = as.character(date.var),
                buckets = buckets, results = results, ret.stats = ret.stats,
                turnover = turnover, natural = natural, do.spread = do.spread, by.period = by.period, 
                overlaps = overlaps))

}

