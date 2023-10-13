
library(readxl)
library(dplyr)
library(haven)
library(purrr)
library(stringr)
library(RQuantLib)
library(bizdays)
library(parallel)
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
                       "option_strike_min"=min_strike_ratio*input_avg_underlying,
                       "option_strike_max"=max_strike_ratio*input_avg_underlying,
                       "option_strike_range"=input_strike_range,
                       "option_expiry_range"=input_expiry_range,
                       "option_style"=input_style)
  
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
