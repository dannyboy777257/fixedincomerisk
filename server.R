library(dplyr)
library(plotly)
library(shiny)

function(input, output, session) {
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

