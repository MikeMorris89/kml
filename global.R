# 
# options(shiny.trace = FALSE)
# options(shiny.reactlog=FALSE)
# options(shiny.fullstacktrace = FALSE)
#options(shiny.error = browser)
#options(shiny.error = recover)
#basicConfig()

#options(shiny.error = function() { 
#  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })

#install_github("mikemorris89/rmm")
library(rmm)
library(magrittr)
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


cnt <- c()
total <- 1000
imymc<-1
for(k in 1:total) cnt = c(cnt, index_Score())
montecarlodf <- setNames(data.frame(1:total, rep(0, total)), c("Incrmt","Probs"))
for (i in 1:total)  montecarlodf$Probs[i] <- sum(cnt[1:i])/i


lapply(dir("tabs",full.names = T,pattern = "*.R$"), source)

