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
                      # plotly::plotlyOutput("somecharts", height = ch), 
                      # shiny::br(), 
                      # shiny::br(), 
                      # plotly::plotlyOutput("2", height = ch), 
                      # shiny::br(), 
                      # shiny::br(), 
                      # plotly::plotlyOutput("3", height = ch) 
                      
      ), 
      shiny::tabPanel("Portfolio with Limits", 
      ),
      shiny::tabPanel("Something cool", 
      )
    )
  )
)
