mypacks<-c("roxygen2"
,'d3heatmap'
,'shinyjs'
,'AppliedPredictiveModeling'
,'caret'
,'pROC'
,'plotly'
,'ggplot2'
,'ggthemes'
,'scales'
,'dplyr' # data manipulation
,'mice' # imputation
,'randomForest' # classification algorithm
,'shinydashboard' # classification algorithm
,'reshape' # classification algorithm
,'rpart' # classification algorithm
,'htmlwidgets' # classification algorithm
,'rattle'
,'rpart.plot'
,'RGtk2'
,'titanic'
,'R6'
,'RGtk2'
#,'RevoScaleR'
,'shinyBS'
,"RDocumentation")
install.packages(mypacks,dep=T)
#library("RDocumentation")

install.packages("titanic")
install.packages("logging",dep=T)

library(devtools)
install_github("mikemorris89/rmm")
library(rmm)



