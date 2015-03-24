#' Calculates the weighted returns
#' 
#' \code{overlaps.compute} Calculates the weighted returns for a long-short portfolio
#' corresponding to a backtest given the number of months a stock is held.
#' 
#' @inheritParams backtest
#' @param in.factor is the name of the vector of factors in \code{x} created
#'        from the single \code{in.var} variable that the overlaps > 1 option allows
#' @param overlaps is the number of months a stock is held in a portfolio
#' 
#' @details This measure of turnover ignores price changes
#' 
#' @return the numeric weighted returns for a portfolio

overlaps.compute <- function(x, in.factor, date.var, id.var, overlaps){

  ## Calculating the weights of each security within a single period
  print("before tribucket")
  print(dim(x))
  x$weight <- tribucket(x, in.factor, date.var)
  print("after tribucket")
  print(dim(x))
  ## Creating a per period weight matrix from x$weight
  
  weight.matrix <- data.frame(x[[date.var]], x[[id.var]], x$weight)
  names(weight.matrix) <- c("date", "id", "weight")
  x <- x[!names(x) %in% "weight"]

  ## Calculating final weights over multiple dates
  
  true.weight <- calc.true.weight(weight.matrix, date.var, id.var, overlaps)

  true.weight <- subset(true.weight, !is.na(true.weight$weight))
  true.weight$weight <- tribucket(true.weight, "weight", date.var, scale = TRUE)
  print("before merge")
  print("dim of true.weight is")
  print(dim(true.weight))
  print(dim(x))
  x <- merge(true.weight, x, all.y = TRUE)
  print("after merge")
  print(dim(x))
}

## Why there is not a single word of comment here for this helper function?...

tribucket <- function(x, in.factor, date.var, scale = FALSE){

  ## Splitting by date
  
  unsplit(lapply(split(x, x[[date.var]]),
                       function(y){
                         
                         tmp <- as.character(y[[in.factor]])
                         if(!isTRUE(scale)){
                           
                           tmp[tmp == "high"] <- 1/length(tmp[tmp == "high"])
                           tmp[tmp == "mid"]  <- 0
                           tmp[tmp == "low"]  <- -1/length(tmp[tmp == "low"])
                         }

                         else{
                           tmp <- as.numeric(tmp)
                           tmp[tmp > 0]  <- tmp[tmp > 0]/sum(tmp[tmp > 0])
                           tmp[tmp == 0] <- 1/length(tmp[tmp == 0])
                           tmp[tmp < 0]  <- tmp[tmp < 0]/sum(tmp[tmp < 0])
                         }
                         
                         y[[in.factor]] <- tmp
                       }), x[[date.var]])
}


calc.true.weight <- function(x, date.var, id.var, overlaps){

  ## Ordering the data.frame by date and then id
  
  x <- x[order(x[[date.var]], x[[id.var]]),]

  ## Reshaping the matrix for easy manipulation of an id over multiple periods
  
  weight.matrix <- reshape(x, direction = "wide", timevar = date.var, v.names = "weight")

  ## Creating the multiple date weight matrix
  
  true.weight <- matrix(NA, ncol = ncol(weight.matrix), nrow = nrow(weight.matrix))
  true.weight <- data.frame(t(as.matrix(apply(weight.matrix, 1,
                         function(x){
                           results <- x[1]
                           for(i in 2:length(x)){
                             if(i <= overlaps){
                               if(all(is.na(x[2:i])))
                                 results[i] <- NA
                               else{
                                 
                                 results[i] <-
                                   as.numeric(sum(as.numeric(x[2:i]), na.rm = TRUE))
                               }
                             }
                             else{
                               if(all(is.na(x[i - overlaps + 1:i])))
                                 results[i] <- NA
                               else{
                                 
                                 results[i] <-
                                   as.numeric(sum(as.numeric(x[i-overlaps+1:i]), na.rm = TRUE))
                               }
                             }
                           }
                           return(results)
                         }))))

  ## Reshaping the true weight matrix
  
  attributes(true.weight) <- attributes(weight.matrix)
  true.weight <- reshape(true.weight)
  
  return(true.weight)
}
