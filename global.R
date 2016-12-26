# 
# options(shiny.trace = FALSE)
# options(shiny.reactlog=FALSE)
# options(shiny.fullstacktrace = FALSE)
# options(shiny.error = browser)
library(stringr)

#library(dplyr)
library(tidyr)
library(d3heatmap)
library(corrplot)
library(AppliedPredictiveModeling)
library(caret)
library(shiny)
library(e1071)
library(pROC)
library(shinyjs)
library(shiny)
library(shinyBS)
library(randomForest)
library(plotly)
library(shinydashboard)
library(reshape)
library(rpart)
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
#library(RGtk2)
library(ggplot2)
library(titanic)
library(R6)
library(RGtk2)
# train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
# train.orig <- read.csv(train_url)
# # # 
# # # # Import the testing set: test
# test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
# test.orig <- read.csv(test_url)
ds <- c("diamonds"
        ,"titanic_train"
        ,"titanic_test"
        ,"GermanCredit"
        ,"logisticCreditPredictions"
        ,"economics"
        ,"economics_long"
        ,"segmentationOriginal"
        ,'JohnsonJohnson'
        ,'LifeCycleSavings'
        ,'USJudgeRatings'
        ,'USPersonalExpenditure'
        ,'occupationalStatus'
)
data(list = ds)
dschoices<-c("diamonds"
  ,"economics_long"
  ,"economics"
  ,"GermanCredit"
  ,"JohnsonJohnson"
  ,"LifeCycleSavings"
  ,"logisticCreditPredictions"
  ,"occupationalStatus"
  ,"segmentationOriginal"
  ,"titanic_all_data"
  ,"USJudgeRatings"
  ,"USPersonalExpenditure"
)
#testing sets
dschoices<-c("segmentationOriginal"
             ,"titanic_all_data"
             )
choicesCorType <-c("circle","square","ellipse","number","shade","color","pie","upper","lower")
choicesCorOrder <-c("AOE","hclust","FPC","alphabet")
choicesModel<-c("Linear Discriminant Analysis (lda) [3 sec]"          = "lda",
  "Classification and Regression Trees (rpart) [6 sec]" = "rpart",
  "Bagged CART (treebag) [90 sec]"                      = "treebag",
  "Stochastic Gradient Boosting (gbm) [20 sec]"         = "gbm",
  "Random Forest (rf) [100 sec]"                        = "rf",
  "Support Vector Machines with Polynomial Kernel (svmPoly) [100 sec]"  = "svmPoly"
)
choicesResult<-c("Categorical : Percent Matched","Categorical : Total Matched")

# lendingclubloan<-read.csv("data/loan.csv")
# lendingclubloandic<-read.table("data/lending club data dictionary.txt",sep='\t')
#save(lendingclubloan, lendingclubloandic, file = "data/lc.RData")

dataset.diamonds <- diamonds
train<-titanic_train
test<-titanic_test
# save(train,file = "train.rdata")
# save(test,file = "test.rdata")
#options(shiny.trace = TRUE)
#save.image(file="titanic.RData")

###################################
#Capture Errors and Warnings
###################################
ew <- function(...) {
  warn <- err <- NULL
  res <- withCallingHandlers(
    tryCatch(..., error=function(e) {
      err <<- conditionMessage(e)
      NULL
    }), warning=function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(res, warn=warn, err=err)
}

conv.char.2.factor<-function(df){
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  return(df)
}
conv.factor.2.char<-function(df){
  df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.character)
  return(df)
}
#load(file="titanic.RData")
countSpaces <- function(s) { 
  sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) 
}

sep.cols<-function(my.data,x){
  my.data$tmpcol<-my.data[,x]
  return(separate(my.data,tmpcol, into = paste(x, 1:max(countSpaces(my.data[,x])), sep = ""), extra = "merge", fill = "right", remove = TRUE)
  )
}

sep.cols.every.char<-function(sc.data,x,maxchar){
  sc.data$tmpcol<-sc.data[,x]
  return(separate(sc.data,tmpcol, into = paste(x,'spl', 0:maxchar, sep = ""),sep = c(0:maxchar+1), extra = "merge", fill = "right", remove = TRUE)
  )
}


left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

#merge data
############
#static all data
all_data.init<-merge(train,test
                     ,by = intersect(names(train),names(test))
                     , all=T)
#all data for updates
all_data<-all_data.init

#add factors
############
add.factors<-function(all_data)
{
  all_data$title<-right(
    left(all_data$Name,regexpr("[.]",all_data$Name))
    ,nchar(left(all_data$Name,regexpr("[.]",all_data$Name)))
    -regexpr("([^ ]*)$",left(all_data$Name,regexpr("[.]",all_data$Name)))+1
  )
  all_data$title<-gsub("[.]","",all_data$title)
  all_data$title<-as.factor(all_data$title)
  all_data$Sex<-as.factor(all_data$Sex)
  all_data$family_size <- all_data$SibSp  + all_data$Parch + 1
  
  all_data$Child <- NA
  all_data$Child[all_data$Age < 18] <-1
  all_data$Child[all_data$Age >= 18] <-0
  return(all_data)
}
all_data<-add.factors(all_data)

line.break<-function(){return(paste(unlist(rep("#",70)),collapse=""))}

lbl<-function(my.lbl){
  return(list(
  line.break(),
  paste("#",my.lbl,sep=" "),
  line.break()
  ))
}


#review missing data
review.missing<-function(all_data)
{
  return(
  list(
  lbl("View All NA"),
  list(sapply(all_data, function(x) sum(is.na(x)))),
  lbl("Empty Strings"),
  list(sapply(all_data[, sapply(all_data, class) == 'character'], function(x) sum(x==""))),
  lbl("Na Numerics"),
  list(sapply(all_data[, sapply(all_data, class) == 'numeric'], function(x) sum(is.na(x)))),
  lbl("Factors Summaries"),
  list(summary(all_data[, sapply(all_data, class) == 'factor']))
  )
  )
}

#convert all factors to char or num
###################################
#indx <- sapply(all_data, is.factor)
#all_data[indx] <- lapply(all_data[indx], function(x) as.numeric(as.character(x)))


#d[, sapply(d, class) %in% c('character', 'factor')]



#fill missing data
all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

all_data$Embarked[c(62, 830)]
all_data$Fare[1044]

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


##########################
#update depenat factors
##########################
all_data<-add.factors(all_data)


all_data[sapply(all_data, is.character)] <- lapply(all_data[sapply(all_data, is.character)], as.factor)


# Split the data back into a train set and a test set
all_data$Case<-as.factor(mapply(all_data$Survived, FUN=function(x){if(is.na(x)==T){"Test"}else{"Train"}}))
#train.clean <- all_data[1:891,]
#test.clean <- all_data[892:1309,]
titanic_all_data<-all_data

#rm("all_data")
rm("titanic_test")
rm("titanic_train")
rm("predicted_age")

#review.missing(train.clean)

#set.seed(415)
#my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp+Parch+Fare+ Embarked+ title,data = train.clean,importance=TRUE,ntree=1000)
#my_forest2<- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp+Parch+Fare+ Embarked+ title,data = train.clean,importance=TRUE,ntree=1000,proximity=TRUE)
#varImpPlot(my_forest)

# Make your prediction using the test set
#my_prediction <- predict(my_forest,test.clean)
#my_prediction2 <- predict(my_forest2,test.clean)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
#my_solution <-data.frame(test$PassengerId,my_prediction)
#my_solution2 <-data.frame(test$PassengerId,my_prediction2)

#my_solution[my_solution2$my_prediction2 != my_solution$my_prediction,]

#MDSplot(my_forest2, my_prediction)
#head(my_forest2$proximity)

suppressPackageStartupMessages(require(ggplot2))
set.seed(2016)
index_Score <- function(){
  # Picking 2 points randomly on the stick at the same time
  x <- runif(n = 2, min = 0, max = 1) 
  a <- min(x) # first point
  b <- max(x) # second point
  # pieces of the stick with their respective length
  pieces <- c(a, b-a, 1-b)
  cond1 <- sum(pieces[c(1,2)]) > pieces[3] # condition # 1
  cond2 <- sum(pieces[c(1,3)]) > pieces[2] # condition # 2
  cond3 <- sum(pieces[c(3,2)]) > pieces[1] # condition # 3
  combine_conds <- ifelse(cond1 & cond2 & cond3, 1, 0) # if all 3 conditions are satisfied
  return(combine_conds)
}

cnt <- c()
total <- 1000
imymc<-1
for(k in 1:total) cnt = c(cnt, index_Score())
montecarlodf <- setNames(data.frame(1:total, rep(0, total)), c("Incrmt","Probs"))
for (i in 1:total)  montecarlodf$Probs[i] <- sum(cnt[1:i])/i


lapply(dir("tabs",full.names = T,pattern = "*.R$"), source)

