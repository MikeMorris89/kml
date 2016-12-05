tabItem.raisinganchor<-tabItem(
    tabName = "raisinganchor",
          h2("Raising anchor")
           ,p("Introduction to the Titanic data set. On this tab we cover predictions using survival rate, gender data, as well as age data.")
          
           ,p("Please select the factor to use for review of the proportions and absolute numbers of survival cases.")
           #,selectInput("select.data.1",label = "Dataset:",choices = c("train","test","all"),selected = "train")
           ,selectInput("select.factor",label = "Factor:",choices = c("Survived","Pclass","Sex","Age","SibSp","Fare","Embarked","Child"),selected = "Sex")
          ,fluidRow(
           column(3,plotlyOutput("plot.prob.survial",height = "300px"))
           ,column(3,tableOutput("table.prob.survial"))
           ,column(3,plotlyOutput("plot.abs.survial",height = "300px"))
           ,column(3,tableOutput("table.abs.survial"))
          )
        
    
)


            # table(train$Survived) 
            # 
            # # Survival rates in proportions
            # prop.table(table(train$Survived))
            #   
            # # Two-way comparison: Sex and Survived
            # table(train$Sex, train$Survived)
            # 
            # # Two-way comparison: row-wise proportions
            #prop.table(table(train$Sex, train$Survived),1)
        

