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
  
  shiny::observeEvent(input$generateButton, {
    filteredYieldData <- yields %>%
      dplyr::filter(symbol %in% input$yieldSelection & date == input$dateInput)
    
    # Generate Plotly plot
    output$yieldCurvePlot <- renderPlotly({
      plotly::plot_ly(data = filteredYieldData, x = ~maturity_in_years, y = ~rate, type = 'scatter', mode = 'lines+markers',
                      marker = list(size = 10), 
                      line = list(shape = "spline")) %>%
        plotly::layout(title = paste("Yield Curve on", input$dateInput),
                       xaxis = list(title = "Maturity (Years)"),
                       yaxis = list(title = "Rate (%)"))
    })
  })
}