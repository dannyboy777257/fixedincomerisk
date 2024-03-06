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
    DT::datatable(recentBondReac$data,
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
  
  output$plChart <- plotly::renderPlotly({
    shiny::req(recentBondReac$data)
    # browser()
    userData <- recentBondReac$data %>% 
      dplyr::mutate(yield_plus = YTM + 0.0001,
                    yield_minus = YTM - 0.0001) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        price = bondPrice(ytm = YTM,
                          faceValue = PortfolioAllocation,
                          coupon = CouponRate,
                          ttm = Maturity,
                          freq = Frequency),
        pricePlus = bondPrice(ytm = yield_plus,
                              faceValue = PortfolioAllocation,
                              coupon = CouponRate,
                              ttm = Maturity,
                              freq = Frequency),
        priceMinus = bondPrice(ytm = yield_minus,
                               faceValue = PortfolioAllocation,
                               coupon = CouponRate,
                               ttm = Maturity,
                               freq = Frequency)
      ) %>% 
      dplyr::ungroup()
    # browser()
    # change to user input so they can choose their boundaries
    # MULTIPLY SHOCK BY 100 FOR CORRECT UNITS
    # FIX WEIGHTS
    append <- dplyr::tibble(yield = base::round(seq(0.025, 0.075, 0.0001),4)) %>% 
      tidyr::nest()
    
    recentBondReac$plotData <- userData %>% 
      dplyr::mutate(duration_w_formula = Maturity / (1+(YTM / 2)), 
                    
                    duration = (pricePlus - priceMinus) / ((2*0.0001)*price),  
                    
                    gamma = (pricePlus - 2 * price + priceMinus)/(price * 2 * 0.0001)) %>% 
        dplyr::mutate(sequence = append) %>% 
        tidyr::unnest(sequence) %>% 
        tidyr::unnest(data) %>% 
        dplyr::group_by(YTM) %>% 
        dplyr::mutate(price_new_yield = mapply(bondPrice, 
                                               ytm = yield,
                                               faceValue = PortfolioAllocation,
                                               coupon = CouponRate,
                                               ttm = Maturity,
                                               freq = Frequency),
                      shock = yield - YTM, 
                      delta_pl = duration * shock * price, 
                      gamma_pl = gamma * shock * price,
                      unexplained_pl = (price_new_yield - price) - delta_pl - gamma_pl,
                      fake_weight = 1 / 11,
                      total_pl = delta_pl + gamma_pl + unexplained_pl,
                      pl_attribution_to_weight = fake_weight * total_pl) %>% 
      dplyr::select(YTM,
                    yield, 
                    shock,
                    total_pl, 
                    fake_weight,
                    pl_attribution_to_weight) %>% 
      dplyr::arrange(yield) %>% 
      dplyr::select(YTM, yield, shock, pl_attribution_to_weight)
    
    recentBondReac$plotData %>% 
        plotly::plot_ly(
          x = ~ shock,
          y = ~ pl_attribution_to_weight,
          colors = ~ YTM,
          name = ~ YTM) %>%
        plotly::add_lines() %>%
        plotly::layout(
          title = list(text = "Where is PL coming from at each shock", x = 0),
          xaxis = list(title = "Shock Amount %"),
          yaxis = list(title = "Weighed Portfolio PL"
          )
        )
    
    
  })
  
  # output$plChart2 <- plotly::renderPlotly({
  #   shiny::req(recentBondReac$plotData)
  #  
  #   recentBondReac$plotData %>% 
  #     plotly::plot_ly(
  #       x = ~ shock,
  #       y = ~ pl_attribution_to_weight,
  #       colors = ~ YTM,
  #       name = ~ YTM) %>%
  #     plotly::add_lines() %>%
  #     plotly::layout(
  #       title = list(text = "Where is PL coming from at each shock", x = 0),
  #       xaxis = list(title = "Shock Amount %"),
  #       yaxis = list(title = "Weighed Portfolio PL"
  #       )
  #     )
  #   
  #   
  # })
  

  
  
  yieldTSReac1 <- reactiveValues(data = yields)
  
  filteredTS <- reactive({
    data <- yieldTSReac1$data
    selectedSymbols <- c(input$asset1, input$asset2, input$asset3, input$asset4)
    allocationValues <- c(input$allocation1, input$allocation2, input$allocation3, input$allocation4)
    allocations <- setNames(allocationValues, selectedSymbols)
    
    if(length(selectedSymbols) > 0) {
      data <- data %>% 
        filter(symbol %in% selectedSymbols) %>%
      mutate(par = case_when(
        symbol == names(allocations)[1] ~ as.numeric(allocations[1]),
        symbol == names(allocations)[2] ~ as.numeric(allocations[2]),
        symbol == names(allocations)[3] ~ as.numeric(allocations[3]),
        symbol == names(allocations)[4] ~ as.numeric(allocations[4]),
        TRUE ~ par
      ))
    }
    
    data <- data %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(price = round(bondPrice(ytm = rate,
                                            faceValue = par,
                                            coupon = couponRate,
                                            ttm = maturity_in_years,
                                            freq = frequency), 2))
    
    return(data)
  })
  
  output$YTMsample <- renderPlotly({
    plotDf <- filteredTS()
    if(nrow(plotDf) == 0) {
      return(NULL) 
    }
    plot <- plotDf %>% 
      plot_ly(x = ~date, y = ~rate, color = ~symbol, type = 'scatter', mode = 'lines+markers')
    
    return(plot)
  })
  
  output$allocation <- renderPlotly({
    plotDf <- filteredTS()
    if(nrow(plotDf) == 0) {
      return(NULL) 
    }
    plot <- plotDf %>% 
      plot_ly(x = ~date, y = ~par, color = ~symbol, type = 'scatter', mode = 'lines+markers')
    
    return(plot)
  })
  
  
  plotData <- eventReactive(input$generateButton, {
    startDate <- as.Date(input$dateRangeInput[1])
    endDate <- as.Date(input$dateRangeInput[2])
    symbolsSelected <- input$yieldSelection
    
    yields %>%
      filter(date >= startDate & date <= endDate, symbol %in% symbolsSelected) %>%
      arrange(date, maturity_in_years)
  }, ignoreNULL = FALSE)  # ignoreNULL=FALSE makes it run once on app startup
  
  output$yieldCurvePlot <- renderPlotly({
    # Accessing the eventReactive plotData directly
    df <- plotData()
    
    plotly::plot_ly(df, x = ~maturity_in_years, y = ~rate,
                    frame = ~as.character(date), ids = ~paste(symbol, date),
                    type = 'scatter', mode = 'lines+markers',
                    line = list(shape = 'spline')) %>%
      plotly::layout(title = 'Yield Curves Through Time', 
                     xaxis = list(title = 'Maturity (Years)'), 
                     yaxis = list(title = 'Rate (%)')) %>%
      plotly::animation_opts(frame = 50, redraw = TRUE) %>%
      plotly::animation_slider(currentvalue = list(prefix = "Date: ")) %>%
      plotly::layout(updatemenus = list(
        list(
          type = "buttons",
          showactive = FALSE,
          y = 0.8,
          x = 1.05,
          xanchor = "right",
          yanchor = "top",
          pad = list(t = 0, r = 10),
          buttons = list(
            list(method = "animate", args = list("play", list(frame = "next", duration = 500)), label = "Play")
          )
        )
      ))
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
