library(shiny)
library(shinydashboard)

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
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      )
    )
  )
)