# Task 89

# Libraries
library(XML)
library(RCurl)
library(lubridate)
library(quantmod)
library(tseries)
library(moments)

# ========================== Download Tickers =======================================

get_tickers <- function(url,required_table_number,column_name){
  # Derive a list of stocks from wikipedia page
  tabs <- getURL(url)
  tables <- readHTMLTable(tabs)
  
  # take required table
  companies_symbols_matrix = as.matrix(tables[[required_table_number]][column_name]) 
  
  # Derive a list of stock symbols from a table
  return(companies_symbols_matrix[,1])
  
}


# ===================================================================================


# ========================== Download Data =========================================
download_1_stock_prices <- function(ticker, start_date, end_date){
  stock_data <- getSymbols(Symbols = ticker, from = start_date, to = end_date,
                           auto.assign = FALSE)
  # Take adjusted close price
  one_stock_prices <- stock_data[,6]

  return(one_stock_prices)
}

download_all_stocks_prices <- function(tickers,start_date, end_date){
  
  all_stocks_prices <- xts()
  
  for(i in 1:length(tickers)){
    one_stock_prices <- download_1_stock_prices(tickers[i], start_date = start_date,
                                         end_date = end_date)

    all_stocks_prices <- merge.xts(all_stocks_prices,one_stock_prices)
  }
  
  colnames(all_stocks_prices) <- tickers
  
  return(na.omit(all_stocks_prices))
}

# ===================================================================================

# ============================== Calculate returns ==================================
calculate_returns <- function(all_stocks_prices, tickers, type){
  
  all_stocks_returns <- xts()
  
  for (i in 1:ncol(all_stocks_prices)) {
    if( type == 'd'){
      one_stock_returns <- dailyReturn(x = all_stocks_prices[,i])
    }
    if( type == 'w'){
      one_stock_returns <- weeklyReturn(x = all_stocks_prices[,i])
    }
    if( type == 'm'){
      one_stock_returns <- monthlyReturn(x = all_stocks_prices[,i])
    }
    if( type == 'q'){
      one_stock_returns <- quarterlyReturn(x = all_stocks_prices[,i])
    }
    
    all_stocks_returns <- merge.xts(all_stocks_returns, one_stock_returns)
  }
  
  # the first row contains all 0
  all_stocks_returns <- all_stocks_returns[-1]
  colnames(all_stocks_returns) <- tickers
  
  return(all_stocks_returns)
}
# ===================================================================================

# ===================== Execute Jarque - Bera test ==================================
execute_jarque_bera_test <- function(all_stocks_returns){
  
  all_jb_x2 <- c(rep(0,times = ncol(all_stocks_returns) ))
  all_jb_pvalue <- c(rep(0,times = ncol(all_stocks_returns) ))
  all_jb_param <- c(rep(0,times = ncol(all_stocks_returns) ))
  
  for(i in 1:ncol(all_stocks_returns)){
    
    one_jb_test_results <- jarque.bera.test(all_stocks_returns[,i])
    
    all_jb_x2[i] <- one_jb_test_results$statistic
    all_jb_pvalue[i] <- one_jb_test_results$p.value
    all_jb_param[i] <- one_jb_test_results$parameter
    
  }
  
  return(list(x2 = all_jb_x2, pvalue = all_jb_pvalue, param = all_jb_param))
  
}
# ===================================================================================

# ========================== Visualization ==========================================
visualize_results <- function(jb_pvalue_d, jb_pvalue_w, jb_pvalue_m, jb_pvalue_q){
  
  num_all <- length(jb_pvalue_d)
  
  num_norm_d <- sum(jb_pvalue_d > 0.05)
  num_norm_w <- sum(jb_pvalue_w > 0.05)
  num_norm_m <- sum(jb_pvalue_m > 0.05)
  num_norm_q <- sum(jb_pvalue_q > 0.05)
  
  # Create 1 figure containing 4 graphs
  par(mfrow=c(2,2))
  
  # Number of normaly distributed VS others
  barplot(c(num_norm_d,num_all-num_norm_d), col = c('green', 'red'),
          main = "Daily")
  barplot(c(num_norm_w,num_all-num_norm_w), col = c('green', 'red'),
          main = "Weekly")
  barplot(c(num_norm_m,num_all-num_norm_m), col = c('green', 'red'),
          main = "Monthly")
  barplot(c(num_norm_q,num_all-num_norm_q), col = c('green', 'red'),
          main = "Quarterly")
  title('Normal VS Non-normal', outer=TRUE) 
  

}
# ===================================================================================

# Main
main <- function(){
  
  # Preparations
  url <- 'https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average'
  required_table_number <- 2
  column_name <- 'Symbol'
  tickers <- get_tickers(url,required_table_number,column_name)
  end_date <- as.Date('2018-01-24')
  start_date <- as.Date(end_date - years(10))
  
  # Prices
  all_prices <- download_all_stocks_prices(tickers, start_date, end_date)
  
  # Returns
  all_daily_returns <- calculate_returns(all_prices, tickers,type = 'd')
  all_weekly_returns <- calculate_returns(all_prices, tickers,type = 'w')
  all_monthly_returns <- calculate_returns(all_prices, tickers, type = 'm')
  all_quarterly_returns <- calculate_returns(all_prices, tickers, type = 'q')
  
  
  # Skewness
  all_returns_skewness_d <- skewness(all_daily_returns)
  all_returns_skewness_w <- skewness(all_weekly_returns)
  all_returns_skewness_m <- skewness(all_monthly_returns)
  all_returns_skewness_q <- skewness(all_quarterly_returns)
  
  # Kurtosis
  all_returns_kurtosis_d <- kurtosis(all_daily_returns)
  all_returns_kurtosis_w <- kurtosis(all_weekly_returns)
  all_returns_kurtosis_m <- kurtosis(all_monthly_returns)
  all_returns_kurtosis_q <- kurtosis(all_quarterly_returns)
  
  # Jarque-Bera test
  all_returns_jarque_bera_d <- execute_jarque_bera_test(all_daily_returns)
  all_returns_jarque_bera_w <- execute_jarque_bera_test(all_weekly_returns)
  all_returns_jarque_bera_m <- execute_jarque_bera_test(all_monthly_returns)
  all_returns_jarque_bera_q <- execute_jarque_bera_test(all_quarterly_returns)
  
  # Visualization
  visualize_results(all_returns_jarque_bera_d$pvalue,
                    all_returns_jarque_bera_w$pvalue,
                    all_returns_jarque_bera_m$pvalue,
                    all_returns_jarque_bera_q$pvalue)
}

main()


