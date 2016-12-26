tabItem.shinytooltips<-tabItem(tabName = "shinytooltips",
tabsetPanel(
  tabPanel(title = "Server tip",
fluidPage(
  sidebarLayout(
  sidebarPanel(
    sliderInput("binsserver",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30),
    bsTooltip("binsserver", "The wait times will be broken into this many equally spaced bins",
              "right", options = list(container = "body"))
  ),
  mainPanel(
    plotOutput("distPlotserver")
  )
  )
)),
tabPanel(title = "Alerts",
         fluidPage(
           sidebarLayout(
             sidebarPanel(textInput("num1", NULL, value = 100),
                          "divided by", textInput("num2", NULL, value = 20),
                          "equals", textOutput("exampleOutput")),
             mainPanel(
               bsAlert("alert")
             )
           )
         )),
tabPanel(title = "Buttons",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               sliderInput("binsbuttons",
                           "Move the slider to see its effect on the button below:",
                           min = 1,
                           max = 50,
                           value = 1),
               bsButton("actTwo", label = "Enable text output", icon = icon("ban")),
               tags$p("Clicking the first button below changes the disabled state of the second button."),
               bsButton("togOne", label = "Toggle 'Block Action Button' disabled status", block = TRUE, type = "toggle", value = TRUE),
               bsButton("actOne", label = "Block Action Button", block = TRUE)
               
             ),
             mainPanel(
               textOutput("exampleText")
             )
           )
         )),
tabPanel(title = " Collapses",
         fluidPage(
           sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
                        actionButton("p1Button", "Push Me!"),
                        selectInput("styleSelect", "Select style for Panel 1",
                                    c("default", "primary", "danger", "warning", "info", "success"))
           ),
           mainPanel(
             bsCollapse(id = "collapseExample", open = "Panel 2",
                        bsCollapsePanel("Panel 1", "This is a panel with just text ",
                                        "and has the default style. You can change the style in ",
                                        "the sidebar.", style = "info"),
                        bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                                        "and a 'success' style.", plotOutput("genericPlot"), style = "success")
             )
           )
           
         )),
tabPanel(title = "Tooltips and Popovers",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               sliderInput("binstooltips",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30),
               bsTooltip("binstooltips", "The wait times will be broken into this many equally spaced bins",
                         "right", options = list(container = "body"))
             ),
             mainPanel(
               plotOutput("distPlottooltips")
             )
           )
         )),
tabPanel(title = "Modals",
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               sliderInput("binsmodals",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30),
               actionButton("tabBut", "View Table")
             ),
             
             mainPanel(
               plotOutput("distPlotmodals"),
               bsModal("modalExample", "Data Table", "tabBut", size = "large",
                       dataTableOutput("distTablemodals"))
             )
           )
         ))
)
)