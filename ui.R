library(shiny)
library(plotly)

my.dashboardHeader<-dashboardHeader.main

my.dashboardSidebar<-
  sidebarMenu(
      menuItem("Raising anchor", tabName = "raisinganchor", icon = icon("dashboard")),
      menuItem("From icebergs to trees", tabName = "fromicebergstotrees", icon = icon("dashboard")),
      menuItem("Improving your predictions", tabName = "improvingyourpredictions", icon = icon("dashboard"))
  )
      

my.dashboardBody<-
  # Boxes need to be put in a row (or column)
  tabItems(
    # First tab content
     tabItem.raisinganchor
    ,tabItem.fromicebergstotrees
    ,tabItem.improvingyourpredictions
    )
  

ui <- dashboardPage(
  my.dashboardHeader,
  dashboardSidebar(my.dashboardSidebar),
  dashboardBody(my.dashboardBody)
  )