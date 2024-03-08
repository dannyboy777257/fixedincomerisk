library(tidyverse)
library(tidyquant)
library(Rcpp)


# Group: Jonathan Vlahadamis, Jamie Kim, Daniel Vovk, Luke Talman
# Project on Fixed income Risk

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
                couponRate = 0,
                frequency = dplyr::case_when(symbol == "DGS1MO" ~ 12, 
                                      symbol == "DGS3MO" ~ 4,
                                      TRUE ~ 2),
                changeBasisPoints = round(((rate - dplyr::lag(rate, 1)) * 10000), digits = 5),
                par = 1000) %>% 
  dplyr::arrange(dplyr::desc(date)) %>% 
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
  dplyr::filter(date == max(date))

recentBond <- yields %>%
  dplyr::filter(date == max(date)) %>% 
  dplyr::filter(symbol %in% c("DGS6MO", "DGS2", "DGS5", "DGS20", "DGS30")) %>%
  dplyr::mutate(Asset = dplyr::case_when(
    symbol == "DGS6MO" ~ "Asset 1",
    symbol == "DGS2" ~ "Asset 2",
    symbol == "DGS5" ~ "Asset 3",
    symbol == "DGS20" ~ "Asset 4",
    symbol == "DGS30" ~ "Asset 5")) %>% 
  dplyr::transmute(
            Asset = Asset,
            YTM = rate,
            Maturity = maturity_in_years,
            CouponRate = couponRate,
            PortfolioAllocation = par,
            Frequency = frequency,
            Price = round(price, 2))
  # above code is used to choose defaults preloaded in tab 1
