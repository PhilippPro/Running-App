library(shiny)
library(shinydashboard)
library(readODS)
library(plotly)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Running App"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("dashboard")),
      menuItem("Graphs", tabName = "graphs", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # Input
      tabItem(tabName = "input",
              h2("Data Input")
      ),
      
      # Graphs
      tabItem(tabName = "graphs",
              
              fluidRow(
                box(selectInput("jahr_von", "Von", choices = c(2020:2022))),
                box(selectInput("jahr_bis", "Bis", selected = 2022, choices = c(2020:2022)))
              ),
              
              fluidRow(
                tableOutput('summary_table')
              ),
              
              
              fluidRow(
                box(selectInput("index", "x-Axis", choices = c("Index", "Date")))
                ),
              fluidRow(
                box(plotlyOutput("plot1", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot2", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot_km_pro_jahr", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot_km_pro_jahr_monat", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot_mean_km_pro_jahr_monat", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot_km_pro_schuhe", height = 250), width = 12)
              )
              # fluidRow(
              #   box(
              #     title = "Controls",
              #     sliderInput("slider", "Number of observations:", 1, 100, 50)
              #   )
              # )
      )
    )
  )
)