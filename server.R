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
shinyServer(function(input, output, session) {
  
  temp.session<-session
  
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
  
  
  pick.func <- function(func) {
     if (func == "initial: review.missing(all_data)"){
       review.missing(all_data.init)
     }
    else if (func == "define: add.factors") {
      review.missing
    }
    else if (func == "updated: review.missing(all_data)") {
      review.missing(all_data)
    }
    else if (func == "define: review.missing(all_data)") {
      review.missing
    }
    # switch(func,
    #        `initial: review.missing(all_data)` = 1,
    #        `define: add.factors` = 2,
    #        `updated: review.missing(all_data)` = 3,
    #        `define: review.missing(all_data)`=4)
  }
  
  output$ex_out <- renderPrint({
    if (input$func == '') 'nothing' 
    else pick.func(input$func)
    
  })
  output$func <- renderText({
    paste('You selected', if (input$func == '') 'nothing' else input$func,
          'as your function to review.')
  })
  
  dataset.diamonds <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  
  output$plot.ggplot <- renderPlot({
    ex.ggplot(dataset.diamonds(),x=input$x, y=input$y,input$color,input$facet_row,input$facet_col,input$jitter,input$smooth)
  })
  
  dataset.diamonds <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  #####################################################
  #CARET
  #####################################################
  values <- reactiveValues()
  
  queryMagic <- function() {
    print("Warning")
    
    return("Data")
  }
  output$console <- renderPrint({
    #logText()
    #return(print(values[["log"]]))
    # You could also use grep("Warning", values[["log"]]) to get warning messages and use shinyBS package
    # to create alert message
  return(ew(list
            (
              fit.ew
              ,OutOfSample.ew
              ,confusion.ew
            )
  ))
  })
  
  logText <- reactive({
    values[["log"]] <- capture.output(data <- queryMagic())
  })
  
  data.shinycaret <- reactive({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
        responseRoutine(method=input$method
                        ,centerscale=input$centerscale
                        ,trainpercent=input$trainpercent
                        ,randomseed=input$randomseed
                        ,dataset=input$selectdata
                        ,cols.exclude=input$uiselectexclude
                        ,col.case.selected=input$uiselectTain
                        ,col.predict=input$uiselectpredict
                        ,usedefault=input$usedefault
                        ,nearzero=input$nearzero
                        ,correlationcutoff=input$correlationcutoff
                        ,colssplittext=input$colssplittext
                        ,colssplitnumb=input$uiselectnumb2split
                        ,colstxtasnumb=input$uiselecttxtasnumb
                        ,selectpctna=input$selectpctna
        )
    )
        #lbl("END : data.shinycaret <- reactive")
  })
  # Fill-in the tabs with output from caret
  
  output$fit <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
    data.shinycaret()$fit
    )
  })
  output$rpt3 <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
      rpt.nearZeroVar
    )  
  })
  output$rpt1 <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
      rpt.high.corr.remove
    
    )  
  })
  output$rpt2 <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
      rpt.cor.matrix
      )
  })

  output$rpt4 <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
    rpt.review.missing.pre
    )
  })
  output$rpt5 <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
    rpt.review.missing.post
    )
  })
  
  output$printgoButton <- renderPrint({
      return(input$goButton[1])
  })
  
  output$confusion <- renderPrint({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(
    data.shinycaret()$confusion
    )
  })
  output$plotconfusion <- renderPlotly({
    if (input$goButton == 0)
      return(iteratemc(1000))
    
    isolate(
      plot.confusion(data.shinycaret()$confusion)
    )
  })
  
  output$dotplot <- renderPlot({
    if (input$goButton == 0)
      return(NULL)
    
    isolate(dotPlot(varImp(data.shinycaret()$fit), main="Dotplot of variable importance values"))
  })
  
  output$scresult <- renderPlotly({
    if (input$goButton == 0)
      return(iteratemc(1000))

      caret.resp<-data.shinycaret()$response
      caret.pred<-data.shinycaret()$prediction
    
      plot.results(caret.resp,caret.pred,input$resultchart)
    
    })
  
  #invalidateLater()
  #autoInvalidate <- reactiveTimer(20)
  
  # observe({
  #   invalidateLater(200,temp.session)
  #   if(imymc<1000) {
  #     sub_df <- subset(df, df$Incrmt <= imymc)
  #     simul_plot <- iteratemc(imymc)
  #     
  #     plotly(simul_plot)
  #     output$scresult <- renderPlotly({
  #       plotly(simul_plot)
  #     })
  #     
  #     imymc<<-imymc+1
  #   }else if (input$goButton != 0)
  #   {
  #     if(length(grep("temp.session",ls()))>0)
  #     {
  #        rm(temp.session)
  #     }
  #     output$scresult <- renderPlotly({
  #       isolate(
  #         plot.results(data.shinycaret()$response,data.shinycaret()$prediction,input$resultchart)
  #       )
  #     })
  #   }else
  #   {
  #     imymc<<-1
  #   }
  # })
  
  output$sccor <- renderD3heatmap ({
    if (input$goButton == 0)
      return(iteratemc(1000))
    
    plot.corr(data.shinycaret()$cor.matrix,input$selectcortype,input$selectcororder)
    })
  output$sccor2 <- renderD3heatmap ({
    if (input$goButton == 0)
      return(iteratemc(1000))
    

    plot.corr(data.shinycaret()$cor.matrix,input$selectcortype,input$selectcororder)
    })
  
  output$rmse <- renderPrint({
    if (input$goButton == 0)
      return(c("Click 'Go' to calc."))
    
    isolate(
      my.rmse(data.shinycaret()$prediction,data.shinycaret()$response)
    )
  })
  addPopover(session, "rmseoutput", "Data", content = "Value only usable on \n continuous variables", trigger = 'click')
  
  observe({
    setDataset(input$selectdata)
    
    updateSelectInput(session, "uiselectTain",
                      label = "Select Case Column:",
                      choices = c("**Create New**",col.names),
                      selected = if(has.col.case){col.case[1]}else{"**Create New**"}
    )
    updateSelectInput(session, "uiselectexclude",
                      label = "Columns to Exclude:",
                      choices =col.names,
                      selected = c(col.names[1],col.names[2])
    )
    updateSelectInput(session, "uiselectpredict",
                      label = "Column to Predict:",
                      choices =col.names,
                      selected = mapped.prediction.values(input$selectdata)
    )
    updateSelectInput(session, "uiselectnumb2split",
                      label = "Number Columns to split:",
                      choices =c(cols.treat.numbers,cols.numbers)
    )
    updateSelectInput(session, "uiselecttxtasnumb",
                      label = "Text Columns to Use as Numbers:",
                      choices =cols.treat.numbers
    )
    
  })
  
  # observeEvent(input$uiselecttxtasnumb, ({
  # updateSelectInput(session, "uiselectnumb2split",
  #                   label = "Number Columns to split:",
  #                   choices =c(cols.treat.numbers,cols.numbers)
  # )
  # }))
  
  addPopover(session, "uiselectTain", "Test and Train Column", placement = "bottom", content = "This is the column that will be used /nsplit the dataset into test and train sets.", trigger = 'hover')
  observeEvent(input$uiselectTain, ({
    updateButton(session, "btncreatecase", 
                 disabled = eval.create.case(input$uiselectTain)
    )
    col.case.selected<-input$uiselectTain
    assign("col.case.selected",col.case.selected,envir = .GlobalEnv)
  }))
  observeEvent(input$uiselectexclude, ({
      cols.exclude<-input$uiselectexclude
      assign("cols.exclude",cols.exclude,envir = .GlobalEnv)
  }))
  observeEvent(input$uiselectpredict, ({
    col.predict<-input$uiselectpredict
    assign("col.predict",col.predict,envir = .GlobalEnv)
  }))
  
  observeEvent(input$btncreatecase, ({
    assign("col.case.selected","myCase",envir = .GlobalEnv)
    #update.case.trainpercent(input$trainpercent)
    set.case.meta(sc.data)
    updateSelectInput(session, "uiselectTain",
                      label = "Select Case Column:",
                      choices = c("myCase"),
                      selected = "myCase"
    )
    shinyjs::disable('uiselectTain')
  }))
  
  
  

  
  #################################################
  #tool tips
  #################################################
  output$distPlotserver <- renderPlot({
    plot.tooltips(input$binsserver)
  })
  output$distPlottooltips <- renderPlot({
    plot.tooltips(input$binstooltips)
  })
  output$distPlotmodals <- renderPlot({
    plot.tooltips(input$binsmodals)
  })
  addPopover(session, "distPlotserver", "Data", content = tt.popover, trigger = 'click')
  observeEvent(input$togOne, ({
    updateButton(session, "actOne", disabled = !input$togOne)
  }))
  output$exampleOutput <- renderText({
    eval.function(as.numeric(input$num1),as.numeric(input$num2),session)
  })
  observeEvent(input$binsbuttons, ({
    b <- input$binsbuttons
    disabled = NULL
    style = "default"
    icon = ""
    if(b < 5) {
      disabled = TRUE
      icon <- icon("ban")
    } else {
      disabled = FALSE
    }
    if(b < 15 | b > 35) {
      style = "danger"
    } else if(b < 20 | b > 30) {
      style = "warning"
    } else {
      style = "default"
      icon = icon("check")
    }
    
    updateButton(session, "actTwo", disabled = disabled, style = style, icon = icon)
    
  }))
  addPopover(session, "distPlottooltips", "Data", content = tt.popover, trigger = 'click')
  output$exampleText <- renderText({
    input$actTwo
    createtext(input$binsbuttons)
  })
  output$genericPlot <- renderPlot(plot(rnorm(100)))
  observeEvent(input$p1Button, ({
    updateCollapse(session, "collapseExample", open = "Panel 1")
  }))
  observeEvent(input$styleSelect, ({
    updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
  }))
  addPopover(session, "binstooltips", "Data", content = tt.popover, trigger = 'click')
  output$distTablemodals <- renderDataTable({
    create.dt(input$binsmodals)
  }, options = list(pageLength=10))
})
