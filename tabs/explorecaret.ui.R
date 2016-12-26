

tabItem.explorecaret<-
  tabItem(tabName = "explorecaret",
          hr("Apply to Diamonds"),
               plotOutput('plot.ggplot.caret'),
               hr(),
               fluidRow(
                 column(3,
                        sliderInput('sampleSize.caret', 'Sample Size', 
                                    min=1, max=nrow(dataset.diamonds),
                                    value=min(1000, nrow(dataset.diamonds)), 
                                    step=500, round=0),
                        br(),
                        checkboxInput('jitter.caret', 'Jitter'),
                        checkboxInput('smooth.caret', 'Smooth')
                 ),
                 column(4, offset = 1,
                        selectInput('x.caret', 'X', names(dataset.diamonds)),
                        selectInput('y.caret', 'Y', names(dataset.diamonds), names(dataset.diamonds)[[2]]),
                        selectInput('color.caret', 'Color', c('None', names(dataset.diamonds)))
                 ),
                 column(4,
                        selectInput('facet_row.caret', 'Facet Row',
                                    c(None='.', names(diamonds[sapply(diamonds, is.factor)]))),
                        selectInput('facet_col.caret', 'Facet Column',
                                    c(None='.', names(diamonds[sapply(diamonds, is.factor)])))
                 )
               ),
               fluidRow(
                 column(3,
                        sliderInput('sampleSize.caret', 'Sample Size', 
                                    min=1, max=nrow(dataset.diamonds),
                                    value=min(1000, nrow(dataset.diamonds)), 
                                    step=500, round=0),
                        br(),
                        checkboxInput('jitter.caret', 'Jitter'),
                        checkboxInput('smooth.caret', 'Smooth')
                 ),
                 column(4, offset = 1,
                        selectInput('x.caret', 'X', names(dataset.diamonds)),
                        selectInput('y.caret', 'Y', names(dataset.diamonds), names(dataset.diamonds)[[2]]),
                        selectInput('color.caret', 'Color', c('None', names(dataset.diamonds)))
                 ),
                 column(4,
                        selectInput('facet_row.caret', 'Facet Row',
                                    c(None='.', names(diamonds[sapply(diamonds, is.factor)]))),
                        selectInput('facet_col.caret', 'Facet Column',
                                    c(None='.', names(diamonds[sapply(diamonds, is.factor)])))
                 )
               )
             )
            
            