# Your train and test set are still loaded in
# str(train)
# str(test)
my.plot2<-function(my.data){

df1<-get(my.data,envir = .GlobalEnv)
# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = df1 , method = "class")
# Time to plot your fancy tree
#text(my_tree_two)
#fancyRpartPlot(my_tree_two)
# Time to plot your fancy tree
p<-fancyRpartPlot(my_tree_two)
p
return(p)
}

my.img<-function(my.data){
  
  list(src = outfile,
       contentType = 'image/png',
       width = 400,
       height = 300,
       alt = "This is alternate text")
  return(p)
}