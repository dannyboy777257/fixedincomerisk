library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(DT)

function(input, output, session) {
  
  recentBondReac <- reactiveValues(data = recentBond)
  
  # editable table render 
  output$recentBondTable <- renderDT({
    datatable(recentBondReac$data, editable = TRUE)
  }, server = TRUE)
  
  # Observe cell edits and update the data accordingly
  observeEvent(input$recentBondTable_cell_edit, {
    # user edits
    editInfo <- input$recentBondTable_cell_edit
    # adding user edits back to df and applying price func.
    recentBondReac$data <- DT::editData(recentBondReac$data, editInfo) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        price = bondPrice(ytm = rate, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency),
        pricePlus = bondPrice(ytm = yield_plus, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency),
        priceMinus = bondPrice(ytm = yield_minus, faceValue = par, coupon = couponRate, ttm = maturity_in_years, freq = frequency)
      )
  })
}