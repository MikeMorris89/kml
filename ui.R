library(shiny)
library(plotly)

my.dashboardHeader<-dashboardHeader.main

my.dashboardSidebar<-
  sidebarMenu(id="mysidebar",
              menuItem("Home", tabName = "shinycaret", icon = icon("dashboard")),
      menuItem("Kaggle mashine Learning", tabName = "raisinganchor", icon = icon("dashboard"),
      menuSubItem("Raising anchor", tabName = "raisinganchor", icon = icon("dashboard")),
      menuSubItem("From icebergs to trees", tabName = "fromicebergstotrees", icon = icon("dashboard")),
      menuSubItem("Improving your predictions", tabName = "improvingyourpredictions", icon = icon("dashboard"))
   ),
   menuItem("Explore Caret",tabName = "exploreggplot",icon=icon("dashboard"),
    menuItem("Explore Diamonds", tabName = "exploreggplot", icon = icon("dashboard")),
    menuItem("Predict Diamonds", tabName = "explorecaret", icon = icon("dashboard")),
    menuItem("Shiny Caret", tabName = "shinycaret", icon = icon("dashboard")),
    menuItem("Notes", tabName = "explorecaretnotes", icon = icon("dashboard"))
  ),
  menuItem("Shiny Exapmles",tabName = "ShinyExapmles",icon=icon("dashboard"),
           menuItem("Bootstrap Tooltips", tabName = "shinytooltips", icon = icon("dashboard"))
           
  )
)


my.dashboardBody<-
  # Boxes need to be put in a row (or column)
  tabItems(
    # First tab content
     tabItem.raisinganchor
    ,tabItem.fromicebergstotrees
    ,tabItem.improvingyourpredictions
    ,tabItem.exploreggplot
    ,tabItem.shinycaret
    ,tabItem.shinytooltips
    ,tabItem.explorecaretnotes
    )
  

ui <- dashboardPage(
  my.dashboardHeader,
  dashboardSidebar(my.dashboardSidebar),
  dashboardBody(my.dashboardBody)
  )