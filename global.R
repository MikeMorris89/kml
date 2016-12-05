 library(shiny)
 library(randomForest)
 library(plotly)
 library(shinydashboard)
 library(reshape)
 library(rpart)
 #library(rattle)
 library(rpart.plot)
 library(RColorBrewer)
 library(RGtk2)
 library(ggplot2)

#  train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
#  train <- read.csv(train_url)
# # 
# # # Import the testing set: test
#  test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
#  test <- read.csv(test_url)

# save(train,file = "train.rdata")
# save(test,file = "test.rdata")
options(shiny.trace = TRUE)
save.image(file="titanic.RData")

load(file="titanic.RData")

train$Child <- NA
train$Child[train$Age < 18] <-1
train$Child[train$Age >= 18] <-0

test$Child <- NA
test$Child[test$Age < 18] <-1
test$Child[test$Age >= 18] <-0

colnames(train)
colnames(test)

lapply(dir("tabs",full.names = T,pattern = "*.R$"), source)

