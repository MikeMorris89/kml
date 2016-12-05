#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$table.prob.survial <- renderTable({
    my.data<-"train"
    my.factor<-input$select.factor
    my.pred<-"Survived"
    data.frame(my.data1(my.data,my.factor,my.pred))
  })
  
  output$plot.prob.survial<-  renderPlotly({
    my.data<-"train"
    my.factor<-input$select.factor
    my.pred<-"Survived"
    my.plot1(my.data,my.factor,my.pred)
  })
  
  output$table.abs.survial <- renderTable({
    my.data<-"train"
    my.factor<-input$select.factor
    my.pred<-"Survived"
    data.frame(my.data.abs1(my.data,my.factor,my.pred))
    })
  
  output$plot.abs.survial<-  renderPlotly({
    my.data<-"train"
    my.factor<-input$select.factor
    my.pred<-"Survived"
    my.plot.abs1(my.data,my.factor,my.pred)
  })
  
})
