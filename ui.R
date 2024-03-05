library(bslib)
library(plotly)
library(shiny)
library(DT)
library(shinyWidgets)


ch <- 400

shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  shiny::titlePanel("Interest Rate Risk on Fixed Income"),
  shiny::br(),
  shiny::mainPanel(
    shiny::tabsetPanel(
      shiny::tabPanel("Portfolio Analysis", 
                      DT::dataTableOutput("recentBondTable")
                      # shiny::br(),
                      # plotly::plotlyOutput("chart1", height = ch), 
                      # shiny::br(), 
                      # shiny::br(), 
                      # plotly::plotlyOutput("chart2", height = ch), 
                      # shiny::br(), 
                      # shiny::br(), 
                      # plotly::plotlyOutput("chart3", height = ch) 
      ),
      
      
      
      
      shiny::tabPanel("Portfolio with Limits"),
      
      

      shiny::tabPanel("Something cool",
                       shiny::sidebarLayout(
                         shiny::sidebarPanel(
                           shiny::selectInput("asset1", "Choose Asset 1:", 
                                              choices = c("Select" = "", unique(yields$symbol))),
                           shiny::selectInput("asset2", "Choose Asset 2:", 
                                              choices = c("Select" = "", unique(yields$symbol))),
                           shiny::selectInput("asset3", "Choose Asset 3:", 
                                              choices = c("Select" = "", unique(yields$symbol))),
                           shiny::selectInput("asset4", "Choose Asset 4:", 
                                              choices = c("Select" = "", unique(yields$symbol)))
                         ),
                         shiny::mainPanel(
                           plotly::plotlyOutput("YTMsample")
                         )
                       )),
      
      shiny::tabPanel("Yield Curves",
                      sidebarLayout(
                        shiny::sidebarPanel(
                          sliderTextInput("dateSlider",
                                          "Choose Date Range:",
                                          choices = seq(as.Date("1992-01-01"), Sys.Date(), by = "day"),
                                          selected = c(as.Date("1992-01-01"), Sys.Date()),
                                          grid = TRUE,
                                          animate = TRUE),
                          shiny::checkboxGroupInput("yieldSelection", "Select Yields to Plot", 
                                                    choices = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", 
                                                                "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"),
                                                    selected = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", 
                                                                 "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")),
                          shiny::actionButton("generateButton", "Generate Plot")
                        ),
                        shiny::mainPanel(
                          plotly::plotlyOutput("yieldCurvePlot")
                        )))
    )
  )
)