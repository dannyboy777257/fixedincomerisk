library(dplyr)
library(plotly)
library(shiny)
library(ggplot2)
library(DT)
library(GGally)
library(grDevices)
library(stats)
library(tidyr)

function(input, output, session) {
  # reactive data set from global to initialize
  recentBondReac <- shiny::reactiveValues(data = recentBond)
  
  # editable table render, outputting the maturity table by default. 
  output$recentBondTable <- DT::renderDT({
    DT::datatable(recentBondReac$data,
              editable = list(target = "cell", disable = list(columns = c(1,7))),
              options = list(pageLength = 5,
                             searching = FALSE,
                             lengthChange = FALSE,
                             paging = FALSE), 
              class = 'cell-border stripe')
  }, server = TRUE)

  shiny::observeEvent(input$recentBondTable_cell_edit, {
    # user edits
    editInfo <- input$recentBondTable_cell_edit
    # adding user edits back to df and applying price func.
    recentBondReac$data <- DT::editData(recentBondReac$data, editInfo) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(Price = round(bondPrice(ytm = YTM, # changed "Value" to price here , can change it back after.
                                faceValue = PortfolioAllocation,
                                coupon = CouponRate,
                                ttm = Maturity,
                                freq = Frequency), 4)) %>% 
    dplyr::ungroup()
                    
  })
  # render the pl chart with various assets in the portfolio
  output$plChart <- plotly::renderPlotly({
    shiny::req(recentBondReac$data)
    # get full portfolio value to find weights of assets
    portfolioValue <- sum(abs(recentBondReac$data$PortfolioAllocation))
    # recalcualte prices
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
                               freq = Frequency),
        totalPortfolio = portfolioValue) %>% 
      dplyr::ungroup()
    # sequence of yields between 0.025 and 0.075 to shock and reprice 
    append <- dplyr::tibble(yield = base::round(seq(0.025, 0.075, 0.0001),4)) %>% 
      tidyr::nest()

    # calculating various Greeks on portfolio with shocks
    recentBondReac$plotData <- userData %>% 
      dplyr::mutate(duration_w_formula = Maturity / (1+(YTM / 2)), 
                    
                    duration = (pricePlus - priceMinus) / ((2*0.0001)*price),  
                    
                    gamma = (pricePlus - 2 * price + priceMinus)/(price * (0.0001^2))) %>% 
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
                      gamma_pl = (price/2) * gamma * (shock^2),
                      unexplained_pl = (price_new_yield - price) - delta_pl - gamma_pl,
                      weight = price / totalPortfolio,
                      total_pl = delta_pl + gamma_pl + unexplained_pl,
                      pl_attribution_to_weight = weight * total_pl) %>% 
      dplyr::ungroup()
    
    # assigning data to other reactive plot data so plots will update
    recentBondReac$plotData3 <- recentBondReac$plotData %>% 
      dplyr::select(shock,
                    yield,
                    delta_pl, 
                    gamma_pl,
                    unexplained_pl)
    
    recentBondReac$plotData1 <- recentBondReac$plotData %>% 
      dplyr::select(Asset,
                    yield, 
                    shock,
                    total_pl, 
                    weight,
                    pl_attribution_to_weight) %>% 
      dplyr::arrange(yield) %>% 
      dplyr::select(Asset, yield, shock, pl_attribution_to_weight) %>% 
      dplyr::ungroup()


   recentBondReac$MONEY <- recentBondReac$plotData1 %>% 
      dplyr::group_by(yield) %>%  
      dplyr::summarise(totalPl = sum(pl_attribution_to_weight)) %>% 
      dplyr::ungroup()

    # render plotly for asset PL chart
    recentBondReac$plotData1 %>% 
        plotly::plot_ly(
          x = ~ shock,
          y = ~ pl_attribution_to_weight,
          colors = ~ Asset,
          mode = 'lines',
          name = ~ Asset) %>%
        plotly::add_lines() %>%
        plotly::layout(
          title = list(text = "PL Attribution per Asset", x = 0.5),
          xaxis = list(title = "Shock Amount %"),
          yaxis = list(title = "Weighted Portfolio PL"
          )
        )
    
    
  })
  # chart for total portfolio pl
  output$plChart2 <- plotly::renderPlotly({
    shiny::req(recentBondReac$MONEY)

    recentBondReac$MONEY %>%
      dplyr::mutate(yield = yield * 100) %>% 
      plotly::plot_ly(
        x = ~ yield,
        y = ~ totalPl) %>%
      plotly::add_lines() %>%
      plotly::layout(
        title = list(text = "Total Portfolio Profit and Loss", x = 0.5),
        xaxis = list(title = "Yield (%)"),
        yaxis = list(title = "Total Portfolio PL"
        )
      )
  })
  # chart for pl attributed to greeks
  output$plChart3 <- plotly::renderPlotly({
    shiny::req(recentBondReac$plotData3)

    recentBondReac$plotData3 %>%
      dplyr::group_by(yield) %>%
      dplyr::summarise(`Delta Pl` = sum(delta_pl),
                       `Gamma Pl` = sum(gamma_pl),
                       `Unexplained Pl` = sum(unexplained_pl)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(cols = -c("yield"), names_to = "plName", values_to = "plValue") %>%
      dplyr::mutate(yield = yield * 100) %>% 
      plotly::plot_ly(
        x = ~ yield,
        y = ~ plValue, 
        color = ~ plName) %>%
      plotly::add_lines() %>%
      plotly::layout(
        title = list(text = "Delta & Gamma PL Attribution", x = 0.5),
        xaxis = list(title = "Yield (%)"),
        yaxis = list(title = "Total Portfolio PL"
        )
      )
    
    
  })
  

  # used for 2nd tab
  
  yieldTSReac1 <- shiny::reactiveValues(data = yields)
  
  # reactive data for all charts on 2nd tab taking in inputs and manipulating accordingly 
  filteredTS <- shiny::reactive({

    data <- yieldTSReac1$data
    selectedSymbols <- c(input$asset1, input$asset2, input$asset3, input$asset4)
    allocationValues <- c(input$allocation1, input$allocation2, input$allocation3, input$allocation4)

    inputTable <- tibble::tibble(selectedSymbols, allocationValues) %>% 
      dplyr::group_by(selectedSymbols) %>% 
      dplyr::summarise(allocationsTotal = sum(allocationValues)) %>% 
      dplyr::ungroup()
    
    data <- data %>% 
      dplyr::filter(symbol %in% inputTable$selectedSymbols) %>%
      dplyr::left_join(inputTable, by = c("symbol" = "selectedSymbols")) %>% 
      dplyr::mutate(par = allocationsTotal) %>% 
      dplyr::select(-allocationsTotal)
    
    data <- data %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(price = round(bondPrice(ytm = rate,
                                            faceValue = par,
                                            coupon = couponRate,
                                            ttm = maturity_in_years,
                                            freq = frequency), 2),
                    pricePlus = bondPrice(ytm = yield_plus,
                                          faceValue = par,
                                          coupon = couponRate,
                                          ttm = maturity_in_years,
                                          freq = frequency),
                    priceMinus = bondPrice(ytm = yield_minus,
                                           faceValue = par,
                                           coupon = couponRate,
                                           ttm = maturity_in_years,
                                           freq = frequency)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        
        duration = (pricePlus - priceMinus)/ ((2*0.0001)*price),
        
        delta = 0.0001*price*duration )
    
    return(data)
  })
  
  # dollar value of basis point graph
  output$DVBP <- plotly::renderPlotly({
    filteredTS() %>% 
      plotly::plot_ly(x = ~ date, 
                      y = ~delta, 
                      name = ~symbol, 
                      color = ~symbol) %>% 
      plotly::add_lines() %>% 
      plotly::layout(title = list(text = "Bond Price Change per 1 Basis Point Increase in Yield: 1995 - Present", x = 0), 
                     xaxis = list(title = "Date"), 
                     yaxis = list(title = "$ Value of 1 Basis Point:")
      )
  })
  # SD plot of DVBP 
  output$standardDev <- plotly::renderPlotly({
    
    sd_date_start <-  "2019-01-01"

    # used Increase because risk should worry about downside/ possible loss
    filteredTS() %>%
      dplyr::filter(date > sd_date_start) %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::mutate(sd_of_delta = slider::slide_dbl(
        .x = delta, 
        .f = sd, 
        .before = 30, 
        .after = 0, 
        .complete = FALSE
      ) * sqrt(252/30)) %>%
      dplyr::mutate(sd_of_delta = sd_of_delta * 100) %>% 
      plotly::plot_ly(x = ~ date, 
                      y = ~sd_of_delta, 
                      name = ~symbol, 
                      color = ~symbol) %>% 
      plotly::add_lines() %>% 
      plotly::layout(title = list(text = "30 Day Rolling SD of Dollar Value Change from 1 Basis Point Increase (Annualized)", x = 0), 
                     xaxis = list(title = "Date"), 
                     yaxis = list(title = "Standard Deviation (%)")
      )

  })
  
  # correlation matrix plot/function
  output$corMatrix <- shiny::renderPlot({
    
    corrData <- filteredTS() %>% 
      dplyr::select(symbol, date, rate) %>% 
      tidyr::pivot_wider(names_from = symbol, values_from = rate) %>% 
      dplyr::select(-date)
    # correlation function to color in plot
    col_function <- function(data, mapping, method = "pearson", use ="pairwise"){
      x <- GGally::eval_data_col(data, mapping$x)
      y <- GGally::eval_data_col(data, mapping$y)
      correlation <- stats::cor(x,y, method = method, use = use)
      col_palate <- grDevices::colorRampPalette(c("blue", "white", "green"), interpolate = "spline")
      fill_in <- col_palate(100)[base::findInterval(correlation, seq(-1,1, length=100))]
      GGally::ggally_cor(data = data, mapping = mapping) + ggplot2::theme_void() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = fill_in))
    }
    
    corr_matrix <- corrData %>% 
      GGally::ggpairs(axisLabels = "none", upper = list(continuous = col_function, lower = list(continuous = GGally::wrap("points", size = 0.8))))
    
    corr_matrix
    
  })
  
# Yield Curve Tab
  plotData <- shiny::eventReactive(input$generateButton, {
    startDate <- base::as.Date(input$dateRangeInput[1])
    endDate <- base::as.Date(input$dateRangeInput[2])
    symbolsSelected <- input$yieldSelection
    
    if (endDate < startDate) {
      # Notify the user about the invalid date range
      shiny::showNotification("End date cannot be before start date.", type = "error")
      
      # Return NULL to prevent further execution
      return(NULL)
    } else if (endDate == startDate) {
      # Notify the user that the start and end dates cannot be the same
      shiny::showNotification("Start and end dates cannot be the same.", type = "error")
      
      # Return NULL to prevent further execution
      return(NULL)
    }
    
    yields %>%
      dplyr::filter(date >= startDate & date <= endDate, symbol %in% symbolsSelected) %>%
      dplyr::arrange(date, maturity_in_years)
  }, ignoreNULL = FALSE)  # ignoreNULL=FALSE makes it run once on app startup
  
  output$yieldCurvePlot <- plotly::renderPlotly({
    # Accessing the eventReactive plotData directly
    df <- plotData()
    
    if (is.null(df)) return(NULL)
    
    plotly::plot_ly(df, x = ~maturity_in_years, y = ~(rate * 100),
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
  
  # User Guide 
  output$dynamicGuide <- renderUI({
    shiny::req(input$guideSelection)
    if(input$guideSelection == "Portfolio Analysis") {
      return(
        shiny::div(
          shiny::h4(strong("About the Tab")),
          shiny::p("The 'Portfolio Analysis' tab provides essential tools for managing bond portfolios against interest rate fluctuations. Featuring an editable table, 
          it allows users to directly manipulate bond data, reflecting immediate impacts on portfolio valuation. This real-time analysis, coupled with a Profit/Loss (P/L) chart, 
          enables users to visualize the effects of yield changes on their investments, aiding in effective risk management and decision-making. The tab simplifies yield stress testing, 
          offering insights into portfolio performance under various interest rate scenarios. Through interactive visualization, users can gauge the weighted impact of each bond, 
          facilitating strategic portfolio adjustments. Overall, this tab is a vital resource for optimizing bond portfolios, tailored for both current and anticipated market conditions."),
          shiny::h4(strong("How to Use the 'Portfolio Analysis' Tab")),
          shiny::p("The 'Portfolio Analysis' tab is designed for dynamic management and analysis of your bond portfolio. Here's how to navigate and utilize its features:"),
          shiny::p("1. Begin with the 'Recent Bond Table' to view your current bond positions. This editable table allows for real-time data manipulation, showing key details of your portfolio."),
          shiny::p("2. Directly edit any cell in the 'Recent Bond Table' to model different scenarios. Adjust elements like yield-to-maturity (YTM), coupon rates, or allocations to see how these changes may affect your portfolio’s metrics."),
          shiny::p("3. Check the 'Profit/Loss (P/L) Chart' located below the table for a graphical representation of how potential interest rate changes could impact your portfolio’s value. This aids in visualizing risk and return."),
          shiny::p("4. Analyze the PL Attributions from Delta and Gamma to understand how the portfolio is affected."),
          shiny::p("Following these steps helps in effectively managing the risk and return profile of your bond portfolio, leveraging real-time data and analytics for strategic portfolio optimization."),
          shiny::h4(strong("Disclaimers")),
          shiny::p("YTM and Coupon rate should be in inputted in decimal form. (ie: a YTM and coupon rate of 5% should be 0.05"),
          shiny::p("Maturity should be inputted in terms of years. (ie: a Maturity of 6 months should be 0.5"),
          shiny::p("Portfolio Allocation is the $ value allocated to an asset. Short positions should be denoted with '-'")
        ))
    } else if(input$guideSelection == "Historical Analysis") {
      return(
        shiny::div(
          shiny::h4(strong("About the 'Historical Analysis' Tab")),
          shiny::p("The 'Historical Analysis' tab provides a streamlined approach for examining the historical performance and volatility of bond assets. 
            It features visual tools to analyze bond price sensitivity to yield changes, the rolling standard deviation of these changes, and correlations of the assets, offering 
            insights into risk and return patterns. Users can customize their analysis by selecting specific assets and setting allocations, making it 
            an essential resource for informed investment strategy and risk management."),
          shiny::h4(strong("How to Use the 'Historical Analysis' Tab")),
          shiny::p("This tab allows users to delve into the historical performance and risk metrics of selected bond assets. Follow these steps to maximize its utility:"),
          shiny::p("1. Start by choosing up to four assets from the dropdown menus labeled 'Choose Asset 1', 'Choose Asset 2', etc."),
          shiny::p("2. For each selected asset, specify its allocation in your portfolio using the 'Asset Allocation' fields. These allocations reflect the weight of each asset in the analysis, influencing the overall interpretation of historical data."),
          shiny::p("3. Review the 'Bond Price Change per 1 Basis Point Increase in Yield' chart, which visualizes how minor yield fluctuations historically impacted the dollar value of your selected bonds."),
          shiny::p("4. Examine the '30 Day Rolling Standard Deviation of Dollar Value Change from 1 Basis Point Increase (Annualized)' chart."),
          shiny::p("5. Examine the Correlation Matrix of each asset to understand the relationship between different assets at different maturities."),
          shiny::p("By following these steps, users can effectively utilize the 'Historical Analysis' tab to gain a comprehensive understanding of past bond performance and risk, aiding in the cultivation of a resilient investment portfolio.")
        )
      )
    } else if(input$guideSelection == "Yield Curves") {
      return(
        shiny::div(
          shiny::h4(strong("About the Tab")),
          shiny::p("The 'Yield Curves' tab offers an essential visualization tool for tracking the yield curve across different maturities, 
            crucial for anticipating interest rate trends and economic health. By animating yield curve changes over time, users can identify patterns 
            and strategize effectively. This functionality is not just about observing historical trends but actively using them to predict 
            future market movements, making it a valuable asset for navigating the fixed income market with confidence."),
          shiny::h4(strong("How to Use The 'Yield Curves' Tab")),
          shiny::p("1. Use the Date Range and Yield Selection to customize your analysis."),
          shiny::p("2. Once settings are chosen to user's discretion, click the generate button to load the start date's yield curve"),
          shiny::p("3. Click the play button to begin the animation and analyze the movement of the yield curve through the chosen date ranges."),
          shiny::p("4. To stop at a specific point in time, use the slider under the graph to examine the yield curve at a user specified time."),
          shiny::h4(strong("Disclaimers")),
          shiny::p("Using a large date range will result in very slow loading times, as each day's yield curve must be saved as a frame to load the animation."),
        )
      )
    }
  })
      
  
}

