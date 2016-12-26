

tabItem.exploreggplot<-tabItem(tabName = "exploreggplot",
                               hr("Diamonds Explorer"),
                               
                               plotOutput('plot.ggplot'),
                               
                               hr(),
                               
                               fluidRow(
                                 column(3,
                                        h4("Diamonds Explorer"),
                                        sliderInput('sampleSize', 'Sample Size', 
                                                    min=1, max=nrow(dataset.diamonds),
                                                    value=min(1000, nrow(dataset.diamonds)), 
                                                    step=500, round=0),
                                        br(),
                                        checkboxInput('jitter', 'Jitter'),
                                        checkboxInput('smooth', 'Smooth')
                                 ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset.diamonds),selected = "clarity"),
           selectInput('y', 'Y', names(dataset.diamonds), selected = "price"),
           selectInput('color', 'Color', c('None', names(dataset.diamonds)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row',
                       c(None='.', names(diamonds[sapply(diamonds, is.factor)]))),
           selectInput('facet_col', 'Facet Column',
                       c(None='.', names(diamonds[sapply(diamonds, is.factor)])))
    )
  )
)