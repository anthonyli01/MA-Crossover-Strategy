library(dplyr)

#' Calculate daily returns
#' 
#' @description 
#' Given a column of closing price data, apply the formula for calculating
#' daily returns and return a new equal size vector with the results.
#' 
#' @param vec Numeric vector with closing prices
#' 
#' @return A numeric vector
#' 
#' @examples 
#' simple_daily_return(trading_data$prccd)
#' 
simple_daily_return <- function(vec) {
  returns = vec / dplyr::lag(vec , 1) - 1
  return(returns)
}

#' Find decile groups
#' 
#' @description 
#' Given a vector of numeric data, rank and find ten equal groups
#' 
#' @param vec Numeric vector
#' 
#' @return A numeric vector
#' 
#' @examples 
#' simple_deciles(trading_data$returns)
#' 
simple_deciles <- function(vec) {
  deciles <- cut(vec, 
                quantile(vec,
                         probs = seq(0, 1, 0.1), 
                         na.rm = TRUE
                ),
                include.lowest = TRUE,
                labels = c("10%", "20%", "30%", "40%", "50%",
                           "60%", "70%", "80%", "90%", "100%")
            )
  return(deciles)
}


movingAvg <- function(data, window_size){
  result <- data * NA
  for (i in window_size:length(data)) {
    result[i] <- sum(data[(i- window_size + 1):i])/window_size
  }
  return (result)
}

simpleBacktest <- function(ts,signal) {
  returns <- ts/stats::lag(ts,1) -1
  strategyReturns <- stats::lag(signal, 1) * returns
  return(strategyReturns)
  
}
