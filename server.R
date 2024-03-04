library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(DT)

function(input, output, session) {
  
  recentBondReac <- reactiveValues(data = recentBond)
  
  # editable table render, outputting the maturity table by default. 
  output$recentBondTable <- renderDT({
    datatable(recentBondReac$data,
              editable = TRUE,
              options = list(pageLength = 11,
                             searching = FALSE,
                             lengthChange = FALSE,
                             paging = FALSE), 
              class = 'cell-border stripe')
  }, server = TRUE)

  observeEvent(input$recentBondTable_cell_edit, {
    # user edits
    editInfo <- input$recentBondTable_cell_edit
    # adding user edits back to df and applying price func.
    recentBondReac$data <- DT::editData(recentBondReac$data, editInfo) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(price = round(bondPrice(ytm = YTM, # changed "Value" to price here , can change it back after.
                                faceValue = PortfolioAllocation,
                                coupon = CouponRate,
                                ttm = Maturity,
                                freq = Frequency), 2))
                   # yield_plus = , 
                    # yield_minus = , 
                    # price_plus = , 
                    # price_minus = , 
                    
                    
      
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
  
  # output$chart1 <- plotly::renderPlotly({
  #   shiny::req()
  #   data %>% 
  #     plotly::plot_ly(x = ~var1, y = ~var2, type = "bar", color = ~var) %>% 
  #     plotly::layout(xaxis = list(title = "title"), yaxis = list(title = "title2"), 
  #                    title = "title")
  # })
  # 
  # output$chart2 <- plotly::renderPlotly({
  #   shiny::req()
  #   data %>% 
  #     plotly::plot_ly(x = ~var1, y = ~var2, type = "scatter", color = ~var) %>% 
  #     plotly::layout(xaxis = list(title = "title"), yaxis = list(title = "title2"), 
  #                    title = "title")
  # })
  # 
  # output$chart3 <- plotly::renderPlotly({
  #   shiny::req()
  #   data %>% 
  #     plotly::plot_ly(x = ~var1, y = ~var2, type = "scatter", color = ~var) %>% 
  #     plotly::layout(xaxis = list(title = "title"), yaxis = list(title = "title2"), 
  #                    title = "title")
  # })
  
}
