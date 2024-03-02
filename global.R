library(dplyr)
library(tidyquant)
library(Rcpp)

# source the file name
Rcpp::sourceCpp('bondCalc.cpp')

bonds <- tidyquant::tq_get("DGS1", 
                           get = "economic.data",
                           from = "1992-01-01",
                           to = Sys.Date()) %>%
  tidyr::drop_na() %>% 
  dplyr::mutate(maturity_in_years = dplyr::case_when(symbol == "DGS1"   ~ 1)) %>%
  dplyr::rename(rate = price) %>%
  dplyr::mutate(rate = rate / 100,
                couponRate = rate,
                frequency = 1,
                changeBasisPoints = round(((rate - lag(rate, 1)) * 10000), digits = 5),
                par = 1000) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::mutate(yield_plus = rate + 0.0001,
                yield_minus = rate - 0.0001) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    price = bondPrice(ytm = rate, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency),
    pricePlus = bondPrice(ytm = yield_plus, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency),
    priceMinus = bondPrice(ytm = yield_minus, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency)
  ) %>% 
  dplyr::ungroup()

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