library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
source("all_usefull.R")

ui <- dashboardPage(
  dashboardHeader(title = "Notenrechner Statistik"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bachelor Prüfungsordung 2010", tabName = "bsc_2010")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "bsc_2010")
    )
  )
  # TODO app_body
)

server <- function (input, output) {
  print("execute server fnk")
  reac <- reactiveValues()
}

shinyApp(ui, server)