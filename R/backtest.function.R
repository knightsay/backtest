#' Creating an Object of Class Backtest
#' 
#' \code{backtest} conducts a backtest and returns the results as an object of class \code{backtest}
#' 
#' @param x is a data frame containing the data to be analysed in the backtest. See \code{details}
#'        for what this data frame must contain
#' @param in.var is a character vector which indicates the name of the column or columns in \code{x}
#'        to be used as input variables
#' @param ret.var is a character vector which indicates the name of the column or columns in \code{x}
#'        to be used as return variables
#' @param by.var is an optional character value, specifying a second variable in \code{x} to be used
#'        for categorising the data. See \code{details} for explanation on how categories are created
#' @param id.var is an optional character value which indicates the name of the column in \code{x} 
#'        containing a unique identifier for each observation. \code{id.var} must be specified if 
#'        \code{natural} is \code{TRUE}
#' @param date.var is an optional character vector which indicates the name of the column in \code{x}
#'        to be used as a date for each observation. \code{date.var} must be specified if 
#'        \code{natural} is \code{TRUE}.  In order to call \code{plot}, the contents of \code{date.var}
#'        must be of class \code{Date} or be coercible to an object of class \code{Date} via 
#'        \code{as.Date}
#' @param buckets is an optional numeric vector which specifies how many quantiles to create according
#'        to \code{in.var} and \code{by.var}
#' @param universe is an optional expression for selecting a subset of \code{x}. See \code{details}
#'        for how \code{universe} may be constructed
#' @param natural is an optional boolean value.  If \code{TRUE}, the \code{summary} method returns 
#'        additional information and the backtest object may be plotted. See details for explanations
#'        on how a natural backtest differs from a pooled backtest
#' @param do.spread is a boolean value. If \code{TRUE}, the \code{summary} method displays information
#'        about the spread between the extreme quantiles. If \code{FALSE}, this information is 
#'        suppressed. Defaults to \code{TRUE}.
#' @param by.period is a boolean value. If \code{TRUE}, the quantiles are recalculated within each 
#'        date period. If \code{FALSE}, the quantiles are calculated all at once. Defaults to \code{TRUE}.
#' @param overlaps is a numeric value which specifies the number of prior periods to include in the 
#'        current period's portfolio weights calculation. If \code{overlaps} is the default of 1,
#'        backtest behaves as usual and only uses a period's own data to determine its portfolio.  
#'        If \code{overlaps} is set to \code{n > 1}, a period's portfolio comprises the weighted mean 
#'        of portfolio weights from the previous \code{n} periods, with period \code{n} having a 
#'        weight of \code{1/n}.
#'        
#' @details Data frames for \code{backtest} must, at a minimum, contain a column of class
#'          numeric to be referenced by the \code{in.var} and \code{ret.var} arguments.  
#'          The \code{in.var} is the primary variable by which the backtest categorises
#'          observations.  It must reference a numeric column in \code{x}.  Using the
#'          values in \code{x}, \code{backtest} breaks the values into equal sized
#'          quantiles, or \code{buckets}.
#'          The \code{by.var} is the secondary variable by which the backtest categorises
#'          observations.  When specifying both \code{in.var} and \code{by.var}, \code{backtest}
#'          organises the observations into a \code{n} by \code{j} matrix where \code{n} is the
#'          number of quantiles or categories created for the \code{by.var} and \code{j} is
#'          the number of quantiles created for the \code{in.var}.  By default,
#'          \code{backtest} creates \code{5} quantiles.
#'          If \code{natural} is \code{TRUE}, the data and arguments must meet certain
#'          requirements.  First, the frequency of the observations and \code{ret.var}
#'          must be the same.  Second, an \code{id.var} and \code{date.var} are
#'          required.  Third, a \code{by.var} is not allowed.  Note that the code
#'          does not verify that the backtest is truly natural; \code{backtest}
#'          accepts the value passed by the user as valid.
#'          
#' @return an object of class \code{backtest}. The functions \code{show} and \code{summary} are used 
#'         to obtain and print a short description and longer summary of the results of the 
#'         \code{backtest}.  The accessor functions \code{counts}, \code{totalCounts}, \code{marginals},
#'         \code{means}, \code{naCounts}, and \code{turnover} extract different parts of the value 
#'         returned by \code{backtest}.
#'
#' @seealso \code{\link{backtest-class}}
#' @export
#' @examples
#' data(starmine)
#' 
#' ## Backtest with 1 'in.var' and 1 'ret.var'
#' bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m", by.period = FALSE)
#' summary(bt)
#' 
#' ## Backtest with 2 'in.var' values, 1 'ret.var', and a 'by.var'
#' bt <- backtest(starmine, in.var = c("smi", "cap.usd"), ret.var = "ret.0.1.m", by.var = "sector", 
#'                by.period = FALSE)
#' summary(bt)
#' 
#' ## Backtest with 1 'in.var', 1 'by.var', and 1 'ret.var'.  Number of
#' ## buckets changed from default of 5 to 4.  Change in number of buckets
#' ## only affects the 'in.var' because the 'by.var' column in 'starmine'
#' ## contains character data. For each value in this column there is a
#' ## unique category.
#' 
#' bt <- backtest(starmine, in.var = "smi", by.var = "sector",
#'                ret.var = "ret.0.1.m", buckets = 4, by.period = FALSE)
#' summary(bt)
#' 
#' ## Backtest with 1 'in.var', multiple 'ret.var', and a
#' ## universe restriction
#' 
#' bt <- backtest(starmine, in.var = "smi",
#' ret.var = c("ret.0.1.m", "ret.0.6.m"),
#' universe = sector == "HiTec", by.period = FALSE)
#' summary(bt)
#' 
#' ## Running a natural backtest with 2 'in.vars', 1 'ret.var' 10 buckets
#' bt <- backtest(starmine, in.var = c("smi","cap.usd"),
#'                ret.var = "ret.0.1.m", date.var = "date",
#'                id.var = "id", buckets = 10,
#'                natural = TRUE, by.period = FALSE)
#' summary(bt)
#' 
#' ## The same backtest, but calculating quantiles within periods.
#' bt <- backtest(starmine, in.var = c("smi","cap.usd"),
#'                ret.var = "ret.0.1.m", date.var = "date",
#'                id.var = "id", buckets = 10,
#'                natural = TRUE, by.period = TRUE)
#' summary(bt)
#' 
#' plot(bt, type = "turnover")
#' plot(bt, type = "return")
#' plot(bt, type = "cumreturn")
#' plot(bt, type = "cumreturn.split")

backtest <- function(x,
                     in.var,
                     ret.var,
                     universe,
                     by.var    = NULL,
                     date.var  = NULL,
                     id.var    = NULL,
                     buckets   = 5,
                     natural   = FALSE,
                     do.spread = TRUE,
                     by.period = TRUE,
                     overlaps  = 1){

  ## Corner Case: only one in.var of class factor allowed per backtest
  
  if(length(in.var) > 1){
    if(any(sapply(x[in.var], class) == "factor") ||
       any(sapply(x[in.var], class) == "character")){
        stop("Only one in.var of class factor or character allowed.")
    }
  }
 
  ## Corner Case: only one by.var allowed
  
  if(length(by.var) > 1){
    stop("Only one by.var allowed per backtest.")
  }

  ## Corner Case: only one id.var allowed

  if(length(id.var) > 1){
    stop("Only one id.var allowed per backtest.")
  }
  
  ## Corner Case: only one by.var allowed when using multiple ret.var

  if(length(ret.var) > 1 && (!is.null(by.var) || !is.null(date.var))){
    warning("Specifying by.var with multiple ret.vars is not supported. Proceed with caution.")
  }

  ## Must provide minimum of one in.var and one ret.var

  if(length(in.var) < 1 || length(ret.var) < 1){
    stop("At least one in.var and ret.var required.")
  }

  ## Only one date.var is allowed

  if(length(date.var) > 1){
    stop("Only one date.var is allowed.")
  }

  ## Natural backtests must have dates and ids.
  
  if(natural && (is.null(date.var) || is.null(id.var))){
    stop("Must specify date.var and id.var for a natural backtest.")
  }

  ## ret.var columns must be numeric
  
  if(!all(sapply(x[ret.var], class) == "numeric")){
    stop("All ret.var columns must be numeric")
  }
  
  ## Check "buckets"

  if(is.null(by.var)){
    buckets[2] <- 1
  }  else{
    if(length(buckets) == 1){
      buckets[2] <- buckets[1]
    }
  }
  
  ## If overlaps is greater than 1, a date.var is required.

  if(overlaps > 1 && is.null(date.var)){
    stop("If overlaps is greater than 1, a date.var is required.")
  }

  ## If overlaps is greater than 0, in.var must be of length one and numeric.

  if(overlaps > 1 && (length(in.var) != 1 || !is.numeric(x[[in.var]]))){
    stop("If overlaps is greater than 1, in.var must be of length one and numeric.")
  }

  ## Overlap option must include id.var
  
  if(overlaps > 1 && length(id.var) < 1){
    stop("If overlaps is greater than 1, there must be an id.var.")
  }


  ## Overlaps must be less than the number of periods

  if(overlaps != 1 && overlaps > length(unique(x[[date.var]]))){
    stop("Overlaps must be less than the number of periods.")
  }

  ## If overlaps > 1, can only use one ret.var
  
  if(overlaps > 1 && length(ret.var) > 1){
    stop("The multiple overlap option can only accept one ret.var.")
  }
  
  ## At a minimum, the length of in.var must be greater than the
  ## number of buckets.  Otherwise, we will get an error when we try
  ## to create more quantiles then there are observations.

  if(any(sapply(x[in.var], function(x){sum(!is.na(x))}) < (buckets[1] - 5))){
    stop("The number of non-NA in.var must be at least 5 more than the number of buckets.") 
  }
  
  ## The length of by.var must also be greater than the number of
  ## buckets to properly create quantiles
  
  if(!is.null(by.var) && sum(!is.na(x[by.var])) < (buckets[2] - 5)){
    stop("The number of non-NA by.var must be at least 5 more than the number of buckets.")
  }

  ## Check for by.period and date

  if(isTRUE(by.period) && is.null(date.var)){
    stop("date.var required if by.period = TRUE (the default)")
  }

  
  ## Save by.var for by.var slot
  
  by.specified <- by.var

  ## Use by.var or date.var?
  ## Can we just get rid of date.
  
  if(!is.null(date.var)){

    if(!is.null(by.var)){
      stop("Cannot specify both by.var and date.var.")
    }    else{
      by.var <- date.var
    }
  }
  
  ## Evaluate "universe"
  ## don't have to go into details of this code; just check 'universe's
  ## documentation above and understand what's it for
  if(!missing(universe)){
    univ <- eval(substitute(universe), x, parent.frame())
    univ <- univ & !is.na(univ)
    x    <- x[univ,]
  }
  
  ## Attach "by.var" factor to "x"
  
  if(is.null(by.var)){
    x$by.factor <- factor(1)
  }  else{
    if(is.null(date.var)){
      x$by.factor <- categorize(x[[by.var]], n = buckets[2])
    }    else{
      x$by.factor <- categorize(x[[by.var]], n = buckets[2], is.date = TRUE)
    }
  }
  
  ## Factor a character in.var
  
  if(length(in.var) == 1 && is.character(x[in.var])){
      x[in.var] <- as.factor(x[in.var])
  }
  

  
  ## Make sure in.var is a factor / factorized... by looping over in.var
  in.factor <- data.frame(array(dim = c(nrow(x), length(in.var)), dimnames = list(NULL,in.var)))
  
  ## Intialize weights
  x$weight <- 1

  ## Build buckets
  for(i in in.var){
    
    ## If in.var is not a factor (is numeric)
    if(!is.factor(x[[i]])){
      
      if(by.period && !all(tapply(x[[i]], x[[date.var]],
                                          function(x){
                                            if(sum(!is.na(x)) < buckets[1]){
                                              return(FALSE)
                                            }
                                            return(TRUE)
                                          }
                                          )
                                   )){
        stop("Not enough observations to fill each bucket by period.")
      }
      
      ## If we are doing by period, form buckets for every date 
      if(by.period){
        in.factor[[i]] <- as.factor(unsplit(lapply(split(x[[i]], x[[date.var]]),
                                                   function(x){
                                                     categorize(x, n = buckets[1])
                                                   }),
                                            x[[date.var]]))
      }
      
      ## If we are not doing by period bucketizing, form all buckets simultaneously
      else{
        in.factor[[i]] <- categorize(x[[i]], n = buckets[1])
      }
      
      
      ## Rename levels, the lowest bucket is the short part of the
      ## portfolio, the highest bucket is the long part      

      levels(in.factor[[i]])[1]          <- "low"
      levels(in.factor[[i]])[buckets[1]] <- "high"
      
      ## Make sure there is data in every bucket
      
      if(length(levels(in.factor[[i]])) != buckets[1]){
        stop(paste("Encountered quantiles with no observations.  This can",
                   "occur with very little data or very regular",
                   "(usually synthesized) data."))
      }
          
      ## Recalculate weights based on overlaps: Overlapping portfolios
      
      if(overlaps > 1){
        
        levels(in.factor[[i]])[1]                  <- "low"
        levels(in.factor[[i]])[buckets[1]]         <- "high"
        ## rename to "mid" all levels that are not low or high 
        levels(in.factor[[i]])[2:(buckets[1] - 1)] <- "mid"
        ## create a new column name
        in.factor.col <- paste("in.factor.", i, sep = "")
        ## create a new column in x
        x[[in.factor.col]] <- in.factor[[i]]
       
        x <- overlaps.compute(x, in.factor.col, date.var, id.var, overlaps)

      }
    }

    ## If the in.var is a factor, just set in.factor to the in.var
    else{
      in.factor[[i]] <- x[[i]]
    }
  }
  
  invisible(backtest.compute(x, in.factor, ret.var, by.var, date.var,
                             natural, by.specified, do.spread, id.var,
                             by.period, overlaps)) 
}
