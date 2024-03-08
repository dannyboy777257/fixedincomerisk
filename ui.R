library(bslib)
library(shiny)
library(DT)
library(tidyverse)
library(tidyquant)


ch <- 400

shiny::fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  shiny::titlePanel("Interest Rate Risk on Fixed Income"),
  shiny::br(),
  shiny::mainPanel(
    shiny::tabsetPanel(
      shiny::tabPanel("User Guide",
                      shiny::selectInput("guideSelection", "Select a Tab to Learn More:", 
                                         choices = c("Portfolio Analysis", "Historical Analysis", "Yield Curves")),
                      shiny::uiOutput("dynamicGuide")
                      ),
      
      shiny::tabPanel("Portfolio Analysis", 
                      DT::dataTableOutput("recentBondTable"),
                      shiny::br(),
                      shiny::br(),
                      plotly::plotlyOutput("plChart", height = ch), 
                      shiny::br(),
                      shiny::br(),
                      plotly::plotlyOutput("plChart2", height = ch)
                     # )
                      # shiny::br(), 
                      # shiny::br(), 
                      # plotly::plotlyOutput("chart3", height = ch) 
      ),

      shiny::tabPanel("Historical Analysis",
                       shiny::sidebarLayout(
                         fluid = TRUE,
                         shiny::sidebarPanel(
                           shiny::selectInput("asset1", "Choose Asset 1:", 
                                              choices = c("Select" = "", unique(yields$symbol)),
                                              selected = unique(yields$symbol)[3]),
                           shiny::numericInput("allocation1", "Asset 1 Allocation:", value = 100),
                           shiny::selectInput("asset2", "Choose Asset 2:", 
                                              choices = c("Select" = "", unique(yields$symbol)),
                                              selected = unique(yields$symbol)[4]),
                           shiny::numericInput("allocation2", "Asset 2 Allocation:", value = 100),
                           shiny::selectInput("asset3", "Choose Asset 3:", 
                                              choices = c("Select" = "", unique(yields$symbol)),
                                              selected = unique(yields$symbol)[7]),
                           shiny::numericInput("allocation3", "Asset 3 Allocation:", value = -100),
                           shiny::selectInput("asset4", "Choose Asset 4:", 
                                              choices = c("Select" = "", unique(yields$symbol)),
                                              selected = unique(yields$symbol)[]),
                           shiny::numericInput("allocation4", "Asset 4 Allocation:", value = -100)
                         ),
                         shiny::mainPanel(
                           plotly::plotlyOutput("DVBP"),
                           shiny::br(),
                           shiny::br(),
                           plotly::plotlyOutput("standardDev")
                         )
                       )),
      
      shiny::tabPanel("Yield Curves",
                      shiny::sidebarLayout(
                        fluid = TRUE,
                        shiny::sidebarPanel(
                          shiny::dateRangeInput("dateRangeInput", "Choose Date Range:",
                                                start = Sys.Date() - 365, end = Sys.Date() - 1,
                                                # end is Sys.Date - 1 to account for recent data not being pulled from tidyquant
                                                min = "1992-01-01", max = Sys.Date()),
                          shiny::checkboxGroupInput("yieldSelection", "Select Yields to Plot", 
                                                    choices = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", 
                                                                "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"),
                                                    selected = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", 
                                                                 "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")),
                          shiny::actionButton("generateButton", "Generate Plot")
                        ),
                        shiny::mainPanel(plotly::plotlyOutput("yieldCurvePlot"))
                      ))
    )
  )
)