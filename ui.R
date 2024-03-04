library(bslib)
library(plotly)
library(shiny)
library(DT)


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
      
      
      
      
      
      shiny::tabPanel("Something cool"),
      
      
      
      
      
      shiny::tabPanel("Yield Curves",
                      sidebarLayout(
                        shiny::sidebarPanel(
                          shiny::dateInput("dateInput", "Choose Date", 
                                           value = Sys.Date(), min = "1992-01-01", max = Sys.Date()),
                          shiny::checkboxGroupInput("yieldSelection", "Select Yields to Plot", 
                                                    choices = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", 
                                                                "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")),
                          shiny::actionButton("generateButton", "Generate Plot")
                        ),
                        shiny::mainPanel(
                          plotly::plotlyOutput("yieldCurvePlot")
                        )))
    )
  )
)