library(shinyjs)

tabItem.shinycaret<-
  tabItem(tabName = "shinycaret",
          
          fluidPage(
            titlePanel("Shiny Caret Lab"),
            
            # Sidebar with controls.  Note the use of the br()
            # element to introduce extra vertical spacing
            sidebarLayout(
              sidebarPanel(
                fluidRow(
                  column(6,selectInput(inputId = "selectdata",label = "Select Dataset",choices = dschoices,selected="segmentationOriginal")),
                  column(3,actionButton("goButton", "Go")),
                  column(3,verbatimTextOutput("printgoButton"))
                )
                ,h4("Preprocessing")
                ,fluidRow(
                   column(6,selectInput("uiselectTain", "Select Case Column:",c('LOADING'),multiple = TRUE))
                  ,column(6,br(),bsButton("btncreatecase", label = "Create Case", block = TRUE))
                )
                ,fluidRow(
                    column(4,selectInput("uiselectexclude", "Columns to Exclude:",c('LOADING'),multiple = TRUE))
                   ,column(4,selectInput("uiselectnumb2split", "Number Columns to split:",c('LOADING'),multiple = TRUE))
                   ,column(4,selectInput("uiselecttxtasnumb", "Text Columns to Use as Numbers:",c('LOADING'),multiple = TRUE))
                   )
                ,fluidRow(
                  column(6,selectInput("uiselectpredict", "Column to Predict:",c('LOADING'),multiple = TRUE))
                  ,column(6,checkboxInput("centerscale", "Center and Scale Variables", TRUE))
                  )
                ,fluidRow(
                  column(4,
                  sliderInput("trainpercent",
                              "Fraction of data that goes to training",
                              value = 0.75, step = 0.05, min = 0.50, max = 0.80)
                  )
                  ,column(4,
                          checkboxInput("usedefault", "Use Default Train %", TRUE)
                  )
                  ,column(4,
                          checkboxInput("colssplittext", "Split Text Columns", TRUE)
                  )
                  )
                  ,fluidRow(
                    column(4,
                           sliderInput("nearzero",
                                       "% of near-zero variance allowed",
                                       value = 10, step = 1, min = 2, max = 99)
                           
                    )
                    ,column(4,
                            sliderInput("selectpctna",
                                        "Replace missing numbers ",
                                        value = .75, step = .01, min = .02, max = 1.00)
                            
                    )
                    ,column(4,
                            numericInput("randomseed", "Random Seed", 19937, min=100, max=1000000)
                            )
                    )
                ,fluidRow(
                  column(4,
                         sliderInput("correlationcutoff",
                                     "Remove variables with correlation higher than :",
                                     value = .95, step = .01, min = .02, max = .99)
                         
                  )
                  ,column(4,selectInput("selectcortype", "CorType:",choicesCorType,selected = choicesCorType[4])
                  )
                  ,column(4,selectInput("selectcororder", "CorOrder:",choicesCorOrder,selected = choicesCorOrder[4])
                  )
                )
                ,HTML("<hr>"),
                radioButtons("method", h4("Caret Model"),choicesModel,selected = choicesModel[2]),
                h6(html.note),
                HTML("<hr>")
              ),
              
              # Tab panels:
              mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Result",
                                     fluidRow(
                                       column(1),
                                       column(5,
                                              selectInput(inputId = "resultchart",label = "Select Chart"
                                                          ,selected = "Categorical : Percent Matched"
                                                          ,choices =choicesResult
                                                            )
                                       ),
                                       column(5,
                                              label_context("Root Mean Squared Error"),
                                              tags$br(id="rmseoutput",verbatimTextOutput("rmse"))
                                       ),
                                       column(1)
                                     ),
                                     fluidRow(
                                       column(1)
                                       ,column(10,plotlyOutput("scresult"))
                                       ,column(1)
                                     )
                                     ,
                                     fluidRow(
                                       column(1)
                                       ,column(10,d3heatmapOutput("sccor"))
                                       ,column(1)
                                     )
                            ),
                            tabPanel("Console",              verbatimTextOutput("console")),
                            tabPanel("Fit",              verbatimTextOutput("fit")),
                            tabPanel("ConfusionMatrix"
                                       ,fluidPage(
                                         plotlyOutput("plotconfusion")
                                        ,verbatimTextOutput("confusion")
                                       )
                                     ),
                            tabPanel("CorrelationMatrix", d3heatmapOutput ("sccor2")),
                            tabPanel("DotPlot",          plotOutput("dotplot")),
                            tabPanel("Data Cleansing",   
                                     verbatimTextOutput("rpt3"),
                                     verbatimTextOutput("rpt2"),
                                     verbatimTextOutput("rpt1"),
                                     verbatimTextOutput("rpt4"),
                                     verbatimTextOutput("rpt5")
                                     ),
                            
                            tabPanel("Help",             HTML(HELP.HTML) )
                )
              )
            )
            
            
          )
  )
