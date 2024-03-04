library(tidyverse)
library(tidyquant)
library(Rcpp)

# source the file name
Rcpp::sourceCpp('bondCalc.cpp')

symbols <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

yields <- tidyquant::tq_get(symbols, get = "economic.data", from = "1992-01-01", to = Sys.Date()) %>%
  tidyr::drop_na() %>% 
  dplyr::mutate(maturity_in_years = dplyr::case_when(
    symbol == "DGS1MO" ~ round(1/12, 3),
    symbol == "DGS3MO" ~ round(3/12, 3),
    symbol == "DGS6MO" ~ 6/12,
    symbol == "DGS1"   ~ 1,
    symbol == "DGS2"   ~ 2,
    symbol == "DGS3"   ~ 3,
    symbol == "DGS5"   ~ 5,
    symbol == "DGS7"   ~ 7,
    symbol == "DGS10"  ~ 10,
    symbol == "DGS20"  ~ 20,
    symbol == "DGS30"  ~ 30
  ))  %>%
  dplyr::rename(rate = price) %>%
  dplyr::group_by(symbol) %>% 
  dplyr::mutate(rate = rate / 100,
                couponRate = rate,
                frequency = dplyr::case_when(symbol == "DGS1MO" ~ 12, 
                                      symbol == "DGS3MO" ~ 4,
                                      TRUE ~ 2),
                changeBasisPoints = round(((rate - lag(rate, 1)) * 10000), digits = 5),
                par = 1000) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::mutate(yield_plus = rate + 0.0001,
                yield_minus = rate - 0.0001) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    price = bondPrice(ytm = rate,
                      faceValue = par,
                      coupon = couponRate,
                      ttm = maturity_in_years,
                      freq = frequency),
    pricePlus = bondPrice(ytm = yield_plus,
                          faceValue = par,
                          coupon = couponRate,
                          ttm = maturity_in_years,
                          freq = frequency),
    priceMinus = bondPrice(ytm = yield_minus,
                           faceValue = par,
                           coupon = couponRate,
                           ttm = maturity_in_years,
                           freq = frequency)
  ) %>% 
  dplyr::ungroup()

recentBond_everything <- yields %>%
  filter(date == max(date))



recentBond <- yields %>%
  filter(date == max(date)) %>% 
  transmute(YTM = rate,
            Maturity = maturity_in_years,
            CouponRate = couponRate,
            PortfolioAllocation = par,
            Frequency = frequency,
            Value = round(price, 2))







# bonds$price <- base::apply(bonds, 1, function(row) {
#   
#   bondPrice(ytm = rate, faceValue = par, coupon = couponRate, ttm = row['maturity_in_years'], freq = frequency)
#   
# })



# 
#                 ,
#                 
#                 
#                 
#                 price_plus = mapply(RTL::bond,
#                                     ytm = yield_plus,
#                                     C = 0,
#                                     T2M = maturity_in_years,
#                                     m = 1,
#                                     output = "price"),
#                 
#                 
#                 price_minus = mapply(RTL::bond,
#                                      ytm = yield_minus,
#                                      C = 0,
#                                      T2M = maturity_in_years,
#                                      m = 1,
#                                      output = "price"),
#                 
#                 
#                 price = mapply(RTL::bond,
#                                ytm = rate,
#                                C = 0,
#                                T2M = maturity_in_years,
#                                m = 1,
#                                output = "price"))

# put our initial data pulls and manipulation in here

