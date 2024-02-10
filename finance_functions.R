    
    library(readxl)
    library(dplyr)
    library(haven)
    library(purrr)
    library(stringr)
    library(RQuantLib)
    library(bizdays)
    library(readr)
    
    ##Update below with dates for full range of interest
    load_quantlib_calendars("UnitedStates/NYSE",from="2000-01-01",to="2030-12-31")
    
    option_chain_aggregator = function(input_option_chain, min_strike_ratio=NA, max_strike_ratio=NA,min_avg_days_to_expiry=NA,max_avg_days_to_expiry=NA){
      input_ticker = unique(input_option_chain$underlying)
      input_date = unique(input_option_chain$quote_date)
      input_style = unique(input_option_chain$style)
      input_avg_underlying = unique(input_option_chain$open_close_avg)
      input_option_chain$days_to_expiry = as.numeric(as.Date(input_option_chain$expiration)-as.Date(input_option_chain$quote_date))
      input_option_chain$business_days_to_expiry = as.numeric(bizdays(as.Date(input_option_chain$quote_date),as.Date(input_option_chain$expiration), "QuantLib/UnitedStates/NYSE"))
      input_option_chain$mean_days_to_expiry = rowMeans(input_option_chain[,c("days_to_expiry","business_days_to_expiry")])
      if(length(input_ticker) > 1){
        return("Error: Must provide input with only 1 included ticker")
      }
      if(length(input_date) > 1){
        return("Error: Must provide input with only 1 quote date")
      }
      if(length(input_avg_underlying) > 1){
        return("Error: Differing underlying average for the same day")
      }
      if(is.na(min_strike_ratio) & is.na(max_strike_ratio)){
        return("Error: Must provide strike ratios to aggregate on")
      }else if(is.na(min_strike_ratio) & !is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio <= max_strike_ratio,]
        input_strike_range = paste0("x <= ", max_strike_ratio)
      }else if(!is.na(min_strike_ratio) & is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio > min_strike_ratio,]
        input_strike_range = paste0(min_strike_ratio," < x")
      }else if(!is.na(min_strike_ratio) & !is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio > min_strike_ratio,]
        input_option_chain = input_option_chain[input_option_chain$option_ratio <= max_strike_ratio,]
        input_strike_range = paste0(min_strike_ratio," < x <= ", max_strike_ratio)
      }
      if(is.na(min_avg_days_to_expiry) & is.na(max_avg_days_to_expiry)){
        return("Error: Must provide expiration day ranges to aggregate on")
      }else if(is.na(min_avg_days_to_expiry) & !is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry < max_avg_days_to_expiry,]
        input_expiry_range = paste0("x < ", max_avg_days_to_expiry)
      }else if(!is.na(min_avg_days_to_expiry) & is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry >= min_avg_days_to_expiry,]
        input_expiry_range = paste0(min_avg_days_to_expiry," <= x")
      }else if(!is.na(min_avg_days_to_expiry) & !is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry >= min_avg_days_to_expiry,]
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry < max_avg_days_to_expiry,]
        input_expiry_range = paste0(min_avg_days_to_expiry," <= x < ", max_avg_days_to_expiry)
      }
      
      temp_results <- list("option_underlying_ticker"=input_ticker,
                           "option_quote_date"=input_date,
                           "option_underlying_open_close_avg"=input_avg_underlying,
                           "option_style"=input_style,
                           "option_strike_range"=input_strike_range,
                           "option_expiry_range"=input_expiry_range)
      
      if(nrow(input_option_chain[input_option_chain$type == "call",]) > 0){
        temp_results[["option_total_call_listings"]]=nrow(input_option_chain[input_option_chain$type == "call",])
        temp_results[["option_call_total_volume"]]=sum(input_option_chain$volume[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_total_open_interest"]]=sum(input_option_chain$open_interest[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_mean_delta"]]=mean(input_option_chain$delta[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_mean_gamma"]]=mean(input_option_chain$gamma[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_mean_theta"]]=mean(input_option_chain$theta[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_mean_vega"]]=mean(input_option_chain$vega[input_option_chain$type == "call"],na.rm = TRUE)
        temp_results[["option_call_mean_implied_volatility"]]=mean(input_option_chain$implied_volatility[input_option_chain$type == "call"],na.rm = TRUE)
      }else{
        temp_results[["option_total_call_listings"]]=0
        temp_results[["option_call_total_volume"]]=0
        temp_results[["option_call_total_open_interest"]]=0
        temp_results[["option_call_mean_delta"]]=NA
        temp_results[["option_call_mean_gamma"]]=NA
        temp_results[["option_call_mean_theta"]]=NA
        temp_results[["option_call_mean_vega"]]=NA
        temp_results[["option_call_mean_implied_volatility"]]=NA
      }
      if(nrow(input_option_chain[input_option_chain$type == "put",]) > 0){
        temp_results[["option_total_put_listings"]]=nrow(input_option_chain[input_option_chain$type == "put",])
        temp_results[["option_put_total_volume"]]=sum(input_option_chain$volume[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_total_open_interest"]]=sum(input_option_chain$open_interest[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_mean_delta"]]=mean(input_option_chain$delta[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_mean_gamma"]]=mean(input_option_chain$gamma[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_mean_theta"]]=mean(input_option_chain$theta[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_mean_vega"]]=mean(input_option_chain$vega[input_option_chain$type == "put"],na.rm = TRUE)
        temp_results[["option_put_mean_implied_volatility"]]=mean(input_option_chain$implied_volatility[input_option_chain$type == "put"],na.rm = TRUE)
      }else{
        temp_results[["option_total_put_listings"]]=0
        temp_results[["option_put_total_volume"]]=0
        temp_results[["option_put_total_open_interest"]]=0
        temp_results[["option_put_mean_delta"]]=NA
        temp_results[["option_put_mean_gamma"]]=NA
        temp_results[["option_put_mean_theta"]]=NA
        temp_results[["option_put_mean_vega"]]=NA
        temp_results[["option_put_mean_implied_volatility"]]=NA
      }
      temp_results <- as.data.frame(temp_results)
      return(temp_results)
    }
    
    option_chain_aggregatorV2 = function(input_option_chain, min_strike_ratio=NA, max_strike_ratio=NA,min_avg_days_to_expiry=NA,max_avg_days_to_expiry=NA){
      input_ticker <- unique(input_option_chain$Symbol)
      input_date <- unique(input_option_chain$DataDate)
      input_underlying_price <- unique(input_option_chain$UnderlyingPrice)
      
      input_option_chain$days_to_expiry <- as.numeric(as.Date(input_option_chain$ExpirationDate)-as.Date(input_option_chain$DataDate))
      input_option_chain$business_days_to_expiry = as.numeric(bizdays(as.Date(input_option_chain$DataDate),
                                                                      as.Date(input_option_chain$ExpirationDate), "QuantLib/UnitedStates/NYSE"))
      
      input_option_chain$mean_days_to_expiry <- rowMeans(input_option_chain[c("days_to_expiry", "business_days_to_expiry")])
      input_option_chain$option_ratio <- input_option_chain$StrikePrice / input_option_chain$UnderlyingPrice
      if(length(input_ticker) > 1){
        return("Error: Must provide input with only 1 included ticker")
      }
      if(length(input_date) > 1){
        return("Error: Must provide input with only 1 quote date")
      }
      if(length(input_underlying_price) > 1){
        return("Error: Differing underlying average for the same day")
      }
      if(is.na(min_strike_ratio) & is.na(max_strike_ratio)){
        return("Error: Must provide strike ratios to aggregate on")
      }else if(is.na(min_strike_ratio) & !is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio <= max_strike_ratio,]
        input_strike_range = paste0("x <= ", max_strike_ratio)
      }else if(!is.na(min_strike_ratio) & is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio > min_strike_ratio,]
        input_strike_range = paste0(min_strike_ratio," < x")
      }else if(!is.na(min_strike_ratio) & !is.na(max_strike_ratio)){
        input_option_chain = input_option_chain[input_option_chain$option_ratio > min_strike_ratio,]
        input_option_chain = input_option_chain[input_option_chain$option_ratio <= max_strike_ratio,]
        input_strike_range = paste0(min_strike_ratio," < x <= ", max_strike_ratio)
      }
      if(is.na(min_avg_days_to_expiry) & is.na(max_avg_days_to_expiry)){
        return("Error: Must provide expiration day ranges to aggregate on")
      }else if(is.na(min_avg_days_to_expiry) & !is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry < max_avg_days_to_expiry,]
        input_expiry_range = paste0("x < ", max_avg_days_to_expiry)
      }else if(!is.na(min_avg_days_to_expiry) & is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry >= min_avg_days_to_expiry,]
        input_expiry_range = paste0(min_avg_days_to_expiry," <= x")
      }else if(!is.na(min_avg_days_to_expiry) & !is.na(max_avg_days_to_expiry)){
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry >= min_avg_days_to_expiry,]
        input_option_chain = input_option_chain[input_option_chain$mean_days_to_expiry < max_avg_days_to_expiry,]
        input_expiry_range = paste0(min_avg_days_to_expiry," <= x < ", max_avg_days_to_expiry)
      }
      
      temp_results <- list("option_underlying_ticker"=input_ticker,
                           "option_quote_date"=input_date,
                           "option_underlying_price"=input_underlying_price,
                           "option_strike_range"=input_strike_range,
                           "option_expiry_range"=input_expiry_range)
      
      if(nrow(input_option_chain[input_option_chain$PutCall == "call",]) > 0){
        temp_results[["option_total_call_listings"]]=nrow(input_option_chain[input_option_chain$PutCall == "call",])
        temp_results[["option_call_total_volume"]]=sum(input_option_chain$Volume[input_option_chain$PutCall == "call"],na.rm = TRUE)
        temp_results[["option_call_total_open_interest"]]=sum(input_option_chain$OpenInterest[input_option_chain$PutCall == "call"],na.rm = TRUE)
      }else{
        temp_results[["option_total_call_listings"]]=0
        temp_results[["option_call_total_volume"]]=0
        temp_results[["option_call_total_open_interest"]]=0
      }
      if(nrow(input_option_chain[input_option_chain$PutCall == "put",]) > 0){
        temp_results[["option_total_put_listings"]]=nrow(input_option_chain[input_option_chain$PutCall == "put",])
        temp_results[["option_put_total_volume"]]=sum(input_option_chain$Volume[input_option_chain$PutCall == "put"],na.rm = TRUE)
        temp_results[["option_put_total_open_interest"]]=sum(input_option_chain$OpenInterest[input_option_chain$PutCall == "put"],na.rm = TRUE)
      }else{
        temp_results[["option_total_put_listings"]]=0
        temp_results[["option_put_total_volume"]]=0
        temp_results[["option_put_total_open_interest"]]=0
      }
      temp_results <- as.data.frame(temp_results)
      return(temp_results)
    }
    
    process_file <- function(file_index, date_list) {
      temp_date <- date_list[file_index]
      
      # Load options data
      temp_file1 <- paste0(main_dir, temp_date, "_OData1.csv")
      temp_file2 <- paste0(main_dir, temp_date, "_OData2.csv")
      
      # Use fread for faster reading of CSVs
      temp_df1 <- fread(temp_file1)
      temp_df2 <- fread(temp_file2)
      
      filteredOptions <- rbindlist(list(temp_df1, temp_df2))
      
      # Filter for the first 1000 unique tickers for demonstration
      all_tickers <- unique(filteredOptions$Symbol)[1:1000]
      
      # Calculate option_ratio
      filteredOptions[, option_ratio := StrikePrice / UnderlyingPrice]
      
      # Initialize an empty list to store results for each ticker
      new_chain_list <- list()
      
      for (temp_ticker in all_tickers) {
        test_chain <- filteredOptions[Symbol == temp_ticker]
        
        # Your aggregation logic here, consider using data.table for efficiency
        
        # Append the result for this ticker to the list
        new_chain_list[[temp_ticker]] <- test_chain # Replace test_chain with your actual result
      }
      
      # Combine all ticker results
      new_chain <- rbindlist(new_chain_list, use.names = TRUE, fill = TRUE)
      
      # Save the result
      saveRDS(new_chain, paste0("aggregate_files/", temp_date, "_aggregate.RDS"))
    }
    
    
    process_fileOld <- function(file_i) {
      source("C:/Users/Andy/Downloads/Investing files/Investing_Project_Rstudio/finance_functions.R")
      main_dir = "E:/Market_Data/DiscountOptionData/DTNSubscription/"
      # Change the working directory
      setwd(main_dir)
      
      raw_files <- list.files(main_dir, pattern = "\\.csv$")
      temp_subset <- raw_files[str_detect(raw_files, fixed('.csv')) & str_detect(raw_files, fixed('OData1'))]
      date_list <- unlist(lapply(temp_subset, function(x) {paste0("D_", str_split(x, "_")[[1]][2])}))
      
      timestamp <- Sys.time()
      
      temp_date <- date_list[file_i]
      
      # Load options data
      temp_file1 <- paste0(temp_date, "_OData1.csv")
      temp_file2 <- paste0(temp_date, "_OData2.csv")
      
      temp_df1 <- read_csv(temp_file1)
      temp_df2 <- read_csv(temp_file2)
      
      filteredOptions <- bind_rows(temp_df1, temp_df2)
      rm(temp_df1,temp_df2)
      all_tickers <- unique(filteredOptions$Symbol)
      all_tickers <- all_tickers[1:1000]
      
      # Add stock open-close average to options df and ratio with strike price
      filteredOptions$option_ratio <- filteredOptions$StrikePrice / filteredOptions$UnderlyingPrice
      
      new_chain <- NULL
      
      for (temp_ticker_i in 1:length(all_tickers)) {
        temp_ticker <- all_tickers[temp_ticker_i]
        if (temp_ticker %in% filteredOptions$Symbol) {
          test_chain <- filteredOptions[filteredOptions$Symbol == temp_ticker, ]
          
          # Suggest starting with <10, 10-25, 25-50, 50-100, 100-200, 200-350, >350
          min_strike_set = c(NA, 0.5, 0.75, 0.9, 0.95, 1, 1.05, 1.1, 1.25, 1.5)
          max_strike_set = c(0.5, 0.75, 0.9, 0.95, 1, 1.05, 1.1, 1.25, 1.5, NA)
          min_day_set = c(NA, 10, 25, 50, 100, 200, 350)
          max_day_set = c(10, 25, 50, 100, 200, 350, NA)
          merge_chain = NULL
          for (temp_strike_i in 1:length(min_strike_set)) {
            for (temp_day_i in 1:length(min_day_set)) {
              # Aggregate over all strike ranges
              test_result = option_chain_aggregatorV2(
                test_chain,
                min_strike_ratio = min_strike_set[temp_strike_i],
                max_strike_ratio = max_strike_set[temp_strike_i],
                min_avg_days_to_expiry = min_day_set[temp_day_i],
                max_avg_days_to_expiry = max_day_set[temp_day_i]
              )
              partial_chain = test_result[,c("option_underlying_ticker",
                                             "option_quote_date",
                                             "option_underlying_price")]
              add_chain = test_result[,!(colnames(test_result) %in% c(colnames(partial_chain),"option_strike_range","option_expiry_range"))]
              colnames(add_chain) = paste0(colnames(add_chain),
                                           "_strike_",str_replace_all(test_result$option_strike_range,"[ ]",""),
                                           "_expiry_",str_replace_all(test_result$option_expiry_range,"[ ]",""))
              if(temp_strike_i == 1 & temp_day_i == 1){
                merge_chain = cbind(partial_chain,add_chain)
              }else{
                merge_chain = cbind(merge_chain,add_chain)
              }
            }
          }
          if(is.null(new_chain)){
            new_chain = merge_chain
          }else{
            new_chain = rbind(new_chain,merge_chain)
          }
        }
        if (temp_ticker_i %% 100 == 0) {
          print(paste0("Finished with ", temp_ticker_i, " tickers for ", temp_date))
          timestamp()
        }
      }
      
      saveRDS(new_chain, paste0("aggregate_files/", temp_date, "_aggregate.RDS"))
      print(paste0("Finished with ", temp_date))
      timestamp()
    }
    
