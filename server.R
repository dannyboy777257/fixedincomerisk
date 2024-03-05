library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(DT)
library(shinyWidgets)

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
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  plotData <- eventReactive(input$generateButton, {
    # Start and end dates from the slider input
    startDate <- as.Date(input$dateSlider[1])
    endDate <- as.Date(input$dateSlider[2])
    symbolsSelected <- input$yieldSelection
    
    # Filter the data for the selected date range and symbols
    filteredYields <- yields %>%
      filter(date >= startDate & date <= endDate,
             symbol %in% symbolsSelected) %>%
      arrange(date, maturity_in_years)
    
    # Find all the unique dates within the filtered data
    uniqueDates <- unique(filteredYields$date)
    
    list(filteredYields = filteredYields, uniqueDates = uniqueDates)
  })
  
  output$yieldCurvePlot <- renderPlotly({
    # Get the data from the eventReactive expression
    data <- plotData()
    filteredYields <- data$filteredYields
    uniqueDates <- data$uniqueDates
    
    # Initialize an empty plotly object
    p <- plot_ly()
    
    # Loop through each date to plot the yield curve for that day
    for(i in seq_along(uniqueDates)) {
      currentDate <- uniqueDates[i]
      # Filter the data for the current date
      dailyYields <- filteredYields %>%
        filter(date == currentDate) %>%
        arrange(maturity_in_years) # Ensure yields are ordered by maturity
      
      if(nrow(dailyYields) > 0) {
        formattedDate <- format(as.Date(currentDate), "%Y-%m-%d") # Format the date for the legend
        p <- add_trace(p,
                       x = dailyYields$maturity_in_years,
                       y = dailyYields$rate,
                       name = formattedDate, # Use the formatted date for the legend
                       type = 'scatter',
                       mode = 'lines+markers',
                       line = list(shape = 'spline'))
      }
    }
    
    # Customize the layout of the plot
    p <- layout(p,
                title = 'Yield Curves',
                xaxis = list(title = 'Maturity (Years)'),
                yaxis = list(title = 'Rate (%)'),
                showlegend = TRUE)
    
    return(p)
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
