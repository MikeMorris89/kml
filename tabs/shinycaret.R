library(AppliedPredictiveModeling)
library(caret)
library(shiny)
library(e1071)
library(pROC)
library(d3heatmap)

eval.create.case<-function(col){
  get("has.col.case",envir = .GlobalEnv)
  if(col=="**Create New**" && has.col.case==FALSE)
  {
    return(FALSE)
  }else
  {
    return(TRUE)
  }
}

set.case.meta<-function(sc.data){
  col.case<-find.test.train.columns(sc.data)  
  has.col.case<-is.empty(col.case)
  col.names<-names(sc.data)
  cols.numbers<-names(sc.data[, sapply(sc.data, class) %in% c('integer','numeric')])
  
  my.tmp<-lapply(sc.data[,!names(sc.data) %in% cols.numbers],function(str){as.numeric(gsub("[^0-9]","",str))})
  cols.treat.numbers<-names(my.tmp)[grep(FALSE,lapply(sapply(my.tmp,mean, na.rm=TRUE),is.na))]
  
  assign("col.case",col.case,envir = .GlobalEnv)
  assign("col.names",col.names,envir = .GlobalEnv)
  assign("has.col.case",has.col.case,envir = .GlobalEnv)
  assign("cols.numbers",cols.numbers,envir = .GlobalEnv)
  assign("cols.treat.numbers",cols.treat.numbers,envir = .GlobalEnv)
  
}
#set.case.meta(sc.data)

find.test.train.columns<-function(data){
  return(names(data)[which(
    apply(data,MARGIN=2,function(x){
      return(length(grep("test|train",x,ignore.case=TRUE))>0)
    })
  )]
  )
}
is.empty<-function(value){
  return(length(value)>0)
}
#find.test.train.columns()

setDataset<-function(dataset){
  sc.data<-get(dataset,envir = .GlobalEnv)
  if(class(sc.data)!="data.frame") {
    sc.data<-data.frame(sc.data)
  }
  set.case.meta(sc.data)
  return(sc.data)
}
#setDataset("segmentationOriginal")
#setDataset("GermanCredit")
#dataset="GermanCredit"
conv.bin.cols<-function(sc.data){
  my.bin<-lapply(sc.data,function(x) all(x %in% c(0,1,NA,"","0","1",NULL,TRUE,FALSE,"NULL")))
  return(my.bin)
}
conv.bin<-function(sc.data){
  my.bin<-unlist(conv.bin.cols(sc.data))
  #class(sc.data)
  sc.data[,grep(TRUE,my.bin)]<-as.factor(unlist(sc.data[,grep(TRUE,my.bin)]))
  sc.data<-conv.char.2.factor(sc.data)
  return(sc.data)
}

formatDataset<-function(sc.data,colssplittext,colssplitnumb,colstxtasnumb,selectpctna,nearzero){
  #colssplitnumb,colstxtasnumb
  sc.data<-conv.char.2.factor(sc.data)
  if(colssplittext==TRUE){
    my.lvls<-lapply(sc.data[,sapply(sc.data,is.factor)],levels) 
    my.lvls<-lapply(lapply(my.lvls,function(x){ suppressWarnings(as.factor(!is.na(as.numeric(x))))}) ,levels)
    my.lvls<-my.lvls[lapply(my.lvls, function(x){is.element(FALSE, x)})==TRUE]
    my.lvls<-names(my.lvls[!names(my.lvls) %in% c(colstxtasnumb)]
    )
    my.lvls<-
    for (l in my.lvls){
      sc.data<-sep.cols(sc.data,l)
    }
  }
  #head(sc.data)
  ###############################
  #remove empty columns
  ###############################
  sc.data<-conv.char.2.factor(sc.data)
  
  my.lvls<-lapply(sc.data[,sapply(sc.data,is.factor)],levels) 
  my.lvls<-lapply(lapply(my.lvls,function(x){ suppressWarnings(as.factor(!is.na(as.numeric(x))))}) ,levels)
  my.lvls.empty<-my.lvls[lapply(my.lvls,length)==0]
  sc.data<-sc.data[!sapply(colnames(sc.data), function(x) x %in% names(my.lvls.empty))]
 
  sc.data<-conv.char.2.factor(sc.data)
  #str(sc.data)
  ###############################
  #binanary to factor
  ###############################
  my.bins<-conv.bin.cols(sc.data)
  sc.data<-conv.bin(sc.data)
  #str(sc.data)
  
  
  ###############################
  #set text numerics
  ###############################
  #cols.treat.numbers<-c("Ticket","Cabin")
  #colssplitnumb,colstxtasnumb
  #sc.data.bak<-sc.data
  #sc.data<-sc.data.bak
  #str(sc.data)
  #colssplitnumb<-c("Cabin","Ticket")
  pre.colssplitnumb<-names(sc.data)
  if(length(colssplitnumb)>0)
  {
    #colssplitnumb<-"Cabin"
    #colssplitnumb<-get("colssplitnumb",envir = .GlobalEnv)
    my.tmp.2<-lapply(sc.data[,colssplitnumb],function(str){as.numeric(gsub("[^0-9]","",str))})
    my.tmp.2<-lapply(my.tmp.2,as.character)
    #my.tmp.2<-conv.factor.2.char(my.tmp.2)
    if(length(colssplitnumb)==1)
    {
      my.tmp.2.ul<-unlist(my.tmp.2)
      my.tmp.3<-max(nchar(unlist(my.tmp.2.ul)),na.rm = TRUE)
      names(my.tmp.3)<-colssplitnumb
      sc.data[,colssplitnumb]<-as.character(str_pad(my.tmp.2.ul, my.tmp.3, pad = "0"))
    }else{
      my.tmp.3<-lapply(my.tmp.2, function(x) max(nchar(x),na.rm = TRUE))
      sc.data[,names(my.tmp.3)]<-lapply(lapply(my.tmp.2,str_pad, 8, pad = "0"),as.character)
    }
    #str(sc.data)
    #sc.data2<-sc.data
    for (l in names(my.tmp.3)){
      sc.data<-sep.cols.every.char(sc.data,l,8)
    }
  }
  sc.data <- sc.data[!is.na(names(sc.data))]
  post.colssplitnumb<-names(sc.data)
  new.colssplitnumb<-setdiff(post.colssplitnumb,pre.colssplitnumb)
  assign("rpt.new.colssplitnumb",new.colssplitnumb,envir = .GlobalEnv)
  
  #sc.data.bak<-sc.data
  if(length(colstxtasnumb)>0)
  {
    if(length(colstxtasnumb)==1)
    {
      sc.data[,colstxtasnumb]<-as.numeric(gsub("[^0-9]","",sc.data[,colstxtasnumb]))
    }else{
      sc.data[,colstxtasnumb]<-lapply(sc.data[,colstxtasnumb],function(str){as.numeric(gsub("[^0-9]","",str))})
    }
  }
  sc.data <- sc.data[!is.na(names(sc.data))]

  sc.data<-conv.char.2.factor(sc.data)
  ###############################
  #convert na to median or ""
  ###############################
  my.lvls<-lapply(sc.data[sapply(sc.data, is.factor)],levels)
  my.lvls<-lapply(lapply(my.lvls,function(x){ suppressWarnings(as.factor(!is.na(as.numeric(x))))}) ,levels)
  my.lvls<-my.lvls[lapply(my.lvls, function(x){length(x)>0})==TRUE]
  my.lvls.keep.as.char<-names(my.lvls[lapply(my.lvls, function(x){is.element(FALSE, x)})==TRUE])
  my.lvls.keep.as.char<-c(my.lvls.keep.as.char,names(which(my.bins == TRUE)),colssplitnumb)
  if(length(new.colssplitnumb)>0)
  {
    assign("my.lvls.keep.as.char1",my.lvls.keep.as.char,envir = .GlobalEnv)
    my.lvls.keep.as.char<-unique(c(my.lvls.keep.as.char,new.colssplitnumb))
  }
  assign("my.lvls.keep.as.char",my.lvls.keep.as.char,envir = .GlobalEnv)
  
  sc.data<-conv.factor.2.char(sc.data)
  #names(sc.data)
  #my.lvls.keep.as.char
  #set empty strings
  if(length(my.lvls.keep.as.char)){
    sc.data[,my.lvls.keep.as.char][is.na(sc.data[,my.lvls.keep.as.char])] <- ""
  }
  
  #set numerics
  my.lvls.convert.char.num<-names(my.lvls[lapply(my.lvls, function(x){is.element(FALSE, x)})==FALSE])
  my.lvls.convert.char.num<-my.lvls.convert.char.num[!(my.lvls.convert.char.num %in% my.lvls.keep.as.char)]

  sc.data[,my.lvls.convert.char.num]<-lapply(sc.data[,my.lvls.convert.char.num],as.numeric)
  my.numeric.cols<-names(sc.data[, sapply(sc.data, class) == 'numeric'])
  
  if(length(my.numeric.cols)>0)
  {  
    for(l in my.numeric.cols){
      
      pctna<-length(sc.data[is.na(sc.data[,l]),l])/length(sc.data[!is.na(sc.data[,l]),l])
      if(pctna<=selectpctna)
      {
        sc.data[is.na(sc.data[,l]),l]<-median(sc.data[,l], na.rm=TRUE)
      }else{
        sc.data[is.na(sc.data[,l]),l]<-""
      }
    }
  }
  
  sc.data<-conv.char.2.factor(sc.data)
  ###############################
  #binanary to factor
  ###############################
  #conv.bin(sc.data)
  
 
  
  # Use Case to divide into raw training and test and final Test sets, as
  # defined in original data.
  # Drop "Cell" ID and Case from original data.frame.
  
  # Remove near-zero variance variables from rawTrain.
  # Use freqCut=2 to get rid of mostly skewed variables with few unqiue values.
  my.nzv <- caret::nearZeroVar(sc.data, freqCut=nearzero, saveMetrics=TRUE)
  
  #my.nzv
  #my.nzv[my.nzv[,"zeroVar"] + my.nzv[,"nzv"] > 0, ]
  count.my.nzv <- sum(my.nzv$nzv)
  my.nzv[my.nzv[,"percentUnique"]>99,"nzv"]<-TRUE
  my.nzv[my.nzv[,"zeroVar"] == TRUE , "nzv"] <-TRUE
  #my.nzv[my.nzv$nzv,]
  #rpt.nearZeroVar
  #str(rpt.nearZeroVar)
  
  if (count.my.nzv > 0){
    sc.data  <- sc.data[, !my.nzv$nzv]
  }
  my.nzv$removed<-sapply(my.nzv$nzv,function(x){if(x){ "Removed"} else {"Kept"}})
  assign("rpt.nearZeroVar",my.nzv[order(my.nzv$removed),],envir = .GlobalEnv)
  
  #head(sc.data)
  sc.data<-conv.char.2.factor(sc.data)
  
  return(sc.data)
}

update.case.trainpercent<-function(usedefault,trainpercent,col.case.selected,col.predict,sc.data){
  if(usedefault==TRUE){
    return(sc.data)
  }else{
    sc.data[,col.case.selected]<-as.character('tbd')
    sc.data$rnames<-rownames(sc.data)
    rownames(sc.data)<-1:nrow(sc.data)
    l<-as.numeric(rownames(sc.data[is.na(sc.data[,col.predict])==FALSE,]))
    ltrain<-sample(l,round(trainpercent*length(l)))
    ltest<-l[!l %in% ltrain]
    sc.data[ltrain,col.case.selected]<-"Train"
    sc.data[ltest,col.case.selected]<-"Test"
    sc.data[is.na(sc.data[,col.predict]),col.case.selected]<-"Apply"
    sc.data[,col.case.selected]<-as.factor(sc.data[,col.case.selected])
    summary(sc.data[,col.case.selected])
  }
  return(sc.data)
}
setdefaultpram<-function(dataset="titanic_all_data"){
  head(titanic_all_data)
  head(segmentationOriginal)
  if(dataset=="titanic_all_data"){
    cols.exclude<-NULL
    colssplitnumb<-"Ticket"
    colstxtasnumb<-"Cabin"
    col.case.selected<-"Case"
    dataset="titanic_all_data"
    trainpercent=0.75
    randomseed='19937'
    sc.data<-setDataset("titanic_all_data")
    col.predict<-"Survived"
    has.col.case<-get("has.col.case",envir = .GlobalEnv)
    usedefault<-TRUE
    nearzero<-10
    selectpctna=.5
    colssplittext=TRUE
    correlationcutoff=.9
    centerscale=TRUE
    trainingData<-setupData(trainpercent, randomseed)$training
    validationData<-setupData(trainpercent, randomseed)$validation
    method="lda"
  }
  if(dataset=="segmentationOriginal"){
    str(segmentationOriginal)
    dataset="segmentationOriginal"
    cols.exclude<-c("Cell")
    col.case.selected<-"Case"
    trainpercent=0.75
    randomseed='19937'
    sc.data<-setDataset("segmentationOriginal")
    col.predict<-"Class"
    has.col.case<-get("has.col.case",envir = .GlobalEnv)
    usedefault<-TRUE
    nearzero<-10
    selectpctna=.5
    colssplittext=TRUE
    correlationcutoff=.9
    centerscale=TRUE
    trainingData<-setupData(trainpercent
                            ,randomseed
                            ,dataset
                            ,cols.exclude
                            ,col.case.selected
                            ,col.predict
                            ,usedefault
                            ,nearzero
                            ,correlationcutoff
                            ,colssplittext
                            ,selectpctna)$training
    validationData<-setupData(trainpercent
                              ,randomseed
                              ,dataset
                              ,cols.exclude
                              ,col.case.selected
                              ,col.predict
                              ,usedefault
                              ,nearzero
                              ,correlationcutoff
                              ,colssplittext
                              ,selectpctna)$validation
  }
}

setupData <- function(trainpercent
                      ,randomseed
                      ,dataset
                      ,cols.exclude
                      ,col.case.selected
                      ,col.predict
                      ,usedefault
                      ,nearzero
                      ,correlationcutoff
                      ,colssplittext
                      ,colssplitnumb
                      ,colstxtasnumb
                      ,selectpctna
                      ){
  myinputs<-list(match.call())
  
  # if(is.na(col.predict))
  #   missinginputs<-
  # if(is.na(col.case.selected))
  #   exit
  # 
  
  
  # Make sure computations can be reproducible.
  set.seed(randomseed)
  sc.data<-setDataset(dataset)
  
  #sc.data$col.to.predict
  sc.data<-update.case.trainpercent(usedefault,trainpercent,col.case.selected,col.predict,sc.data)
  sc.data.pred<-sc.data[,col.predict]
  sc.data.case<-sc.data[,col.case.selected]
  sc.data<-sc.data[, !names(sc.data) %in% c(col.predict,col.case.selected,cols.exclude)]
  
  assign("rpt.review.missing.pre",review.missing(sc.data),envir = .GlobalEnv)
  sc.data<-formatDataset(sc.data,colssplittext,colssplitnumb,colstxtasnumb,selectpctna,nearzero)
  assign("rpt.review.missing.post",review.missing(sc.data),envir = .GlobalEnv)
  
  head(sc.data)
  
  sc.data$my.pred<-sc.data.pred
  sc.data$my.pred<-as.factor(sc.data$my.pred)
  sc.data$my.case<-sc.data.case
  
  if(has.col.case){
    rawTrain  <- sc.data[sc.data$my.case == "Train",!grepl("my.case",names(sc.data))]
    finalTest <- sc.data[sc.data$my.case == "Test",!grepl("my.case",names(sc.data))]
    finalTest.na <- sc.data[sc.data[is.na(sc.data$my.pred)==FALSE,]$my.case == "Test",!grepl("my.case",names(sc.data))]
    rawApply <- sc.data[sc.data$my.case == "Apply",!grepl("my.case",names(sc.data))]
  }
  
  
  # Remove variables with high correlation
  HIGH.CORRELATION.CUTOFF <- correlationcutoff
  cormatrix <- cor(sapply(rawTrain[,!grepl("my.pred",names(rawTrain))], as.numeric))
  cor.high   <- findCorrelation(cormatrix, HIGH.CORRELATION.CUTOFF)
  assign("rpt.cor.matrix",cormatrix,envir = .GlobalEnv)
  #plot.corr(cormatrix,method = "number")
  
  
  high.corr.remove <- row.names(cormatrix)[cor.high]
  assign("rpt.high.corr.remove",high.corr.remove,envir = .GlobalEnv)
  
  rawTrain <- rawTrain[,  -cor.high]
  finalTest <- finalTest[, -cor.high]
  
  
  # Partition raw training data into a training and validation set.
  inTrainSet <- createDataPartition(y=rawTrain$my.pred, p=trainpercent, list=FALSE)
  training <- rawTrain[inTrainSet,]
  
  validation <- rawTrain[-inTrainSet,]
  invisible( list(myinputs=myinputs,training=training, validation=validation, finalTest=finalTest,finalTest.na=finalTest.na,rawApply=rawApply,cormatrix=cormatrix) )
}

# Train learning model.  Apply to out-of-sample test data to compute
# confusion matrix and related data.
generic.fit <- function(method, trainingData, validationData, centerscale)
{
  #setupData()
  preprocess.methods <- NULL
  if (centerscale) preprocess.methods = c("center", "scale")
  
  assign("trainingData",trainingData,envir = .GlobalEnv)
  assign("validationData",validationData,envir = .GlobalEnv)
  
  #method<-'rpart'
  #names(getModelInfo())
  rm(fit)
  fit.ew<-ew(
    fit <- train(my.pred ~ .,
                 data=trainingData,
                 preProcess=preprocess.methods, 
                 method=method,
                 trControl = trainControl(
                   method = "cv", 
                   number = 2,
                   repeats = 0,
                   verboseIter = TRUE
                 ))
  )
  
  OutOfSample.ew<-ew(           
    OutOfSample  <- predict(fit, newdata=validationData)
  )
  confusion.ew<-ew(
    confusion <- confusionMatrix(validationData$my.pred, OutOfSample)
  )
  
  
  assign("my.fit",fit,envir = .GlobalEnv)
  assign("my.OutOfSample",OutOfSample,envir = .GlobalEnv)
  assign("my.confusion",confusion,envir = .GlobalEnv)
  
  assign("fit.ew",fit.ew,envir = .GlobalEnv)
  assign("OutOfSample.ew",OutOfSample.ew,envir = .GlobalEnv)
  assign("confusion.ew",confusion.ew,envir = .GlobalEnv)
  invisible( list(fit=fit, confusion=confusion,prediction=OutOfSample,response=validationData$my.pred) )
}



mapped.prediction.values<-function(sel.data){
  get("col.names",envir = .GlobalEnv)
  #sel.data<-"segmentationOriginal"
  pred<-
    switch(sel.data
         ,"segmentationOriginal"="Class"
         ,"GermanCredit"="Class"
         ,"logisticCreditPredictions"="obs"
         ,"titanic_all_data"="Survived"
         ,col.names[1]
    )
  return(pred)
}#mapped.prediction.values("titanic_all_data")


responseRoutine <- function(method
                            ,centerscale
                            ,trainpercent
                            ,randomseed
                            ,dataset
                            ,cols.exclude
                            ,col.case.selected
                            ,col.predict
                            ,usedefault
                            ,nearzero
                            ,correlationcutoff
                            ,colssplittext
                            ,colssplitnumb
                            ,colstxtasnumb
                            ,selectpctna)
{
  d <- setupData(trainpercent
                 ,randomseed
                 ,dataset
                 ,cols.exclude
                 ,col.case.selected
                 ,col.predict
                 ,usedefault
                 ,nearzero
                 ,correlationcutoff
                 ,colssplittext
                 ,colssplitnumb
                 ,colstxtasnumb
                 ,selectpctna
  )
  fit <- generic.fit(method, d$training, d$validation, centerscale)
  invisible(fit)
}


my.rmse<-function(prediction,response){
  if(class(levels(prediction))=="character" && length(levels(prediction))==2)
  {
    my.result<-sqrt(mean((unclass(prediction)-unclass(response))^2))
  }else{
    my.result<-sqrt(mean((prediction-response)^2))
  }
  return(my.result)
}
#my.rmse(prediction,response)

#response<-responseRoutine()$response
#prediction<-responseRoutine()$prediction
plot.results<-function(response,prediction,type="Categorical : percent matched"){
  my.colors<-c("dodgerblue","firebrick","dodgerblue","firebrick")
  assign("plot.response",response,envir = .GlobalEnv)
  assign("plot.prediction",prediction,envir = .GlobalEnv)
  my.fit
  my.OutOfSample
  predict(my.fit, newdata=validationData)
  OutOfSample  <- predict(fit, newdata=validationData)
  td<-trainingData
  # td$my.pred<-as.factor(ifelse(td$my.pred == 0, "No", "Yes"))
  # str(td)
  # lm
  #td<-td[,lapply(td,class)!="factor"]
  td$my.pred<-as.integer(trainingData$my.pred)
  review.missing(td)
  my.fit2<-caret::train(my.pred ~ .,
        data=td,
        preProcess=preprocess.methods, 
        method="lm",
        trControl = trainControl(
          method = "cv", 
          number = 2,
          repeats = 0,
          verboseIter = TRUE
        ))
  my.validationData<-validationData
  my.validationData$my.pred<-as.integer(validationData$my.pred)
  
  OutOfSample  <- predict(my.fit2, newdata=my.validationData)
  prediction<-OutOfSample
  response<-validationData$my.pred
  my.data<-data.frame(prediction = prediction
                   ,truth = response
                   ,Result=paste(response,mapply(prediction,response,FUN=function(x,y){if(x==y){ "matched"} else{"not matched"}}),sep=' ')
                   )
  my.data$i<-rownames(my.data)
  
  if(type=="Categorical : Percent Matched")
  {
  my.data  %>%
    group_by(truth,Result) %>%
    summarise(value= n()) %>%
    mutate(percent = (value/sum(value))) %>%
    plot_ly(x = ~truth,y= ~percent, color = ~Result, colors = my.colors,type='bar') %>% layout(yaxis = list(title = 'Count'), barmode = 'stack', title="Percent Matched")
  }
  else if(type=="Categorical : Total Matched")
  {
  my.data  %>%
    group_by(Result) %>%   
    summarise(count= n()) %>%
      plot_ly(x = ~Result,y= ~count, color = ~Result, colors = my.colors,type='bar') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack', title="Total Matched")
  }
  else if(type=="Categorical : Total Matched")
  {
    my.data  %>%
    plot_ly(x = ~i,y= ~prediction, color = ~truth, colors = my.colors,type='bar') %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack', title="Total Matched")
  }
}
#plot.corr(responseRoutine()$cormatrix)
#my.m<-responseRoutine()$cormatrix
#my.m<-responseRoutine(dataset="segmentationOriginal")$cormatrix
plot.corr<-function(m,method="circle",order="alphabet"){
  rpt.cor.matrix<-get("rpt.cor.matrix",envir = .GlobalEnv)
  #rpt.cor.matrix<-matrix(rpt.cor.matrix)
  #str(rpt.cor.matrix)
  my.d3<-d3heatmap(rpt.cor.matrix, scale = "column",
            color = scales::col_quantile("Blues", NULL, 5)
            ,dendrogram = "row", k_row = 3)
  #my.d3
  #str(rpt.cor.matrix)
  return(my.d3)
}

#plot.confusion(confusion)
plot.confusion <-function(confusion){
  tmp.confusion<-as.data.frame(as.table(confusion))
  plot <- ggplot(tmp.confusion)
  plot <-plot + geom_tile(aes(x=Prediction, y=Reference, fill=Freq)) + scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + labs(fill="Normalized\nFrequency")
  plot <-plot + geom_text(aes(x=Prediction, y=Reference, label = sprintf("%1.0f", Freq)), vjust = 1)
  return(plot)
}

HELP.HTML <<- paste(
  'This Shiny app uses techniques described in the Coursera <a',
  'href="https://www.coursera.org/course/predmachlearn"><i>Practical',
  'Machine Learning</i></a> class.<br>',
  '<hr> <b>Concept</b>:&nbsp; Use interactive interface to select',
  'machine learning parameters for the <a',
  'href="http://caret.r-forge.r-project.org/">caret package</a> to',
  'solve a problem involving image segmentation.&nbsp; This interactive',
  'interface may provide insights on best machine learning approach for',
  'a given problem.&nbsp; Future:&nbsp; perhaps with small set of',
  'metadata, the approach could be used with a variety of problems and',
  'datasets.<br><br>',
  'To see data:  library(AppliedPredictiveModeling) and then',
  'data(segmentationOriginal).<br><br>',
  'Some variables with near-zero variance and/or high correlation to',
  'other variables have been removed.',
  '<hr> <b>Inputs</b>:&nbsp; At the left, select various preprocessing',
  'options and caret method.<br>',
  '<br>',
  '<b>Fit tab</b>:&nbsp; Output from caret train function applied to',
  'subset of original training data.<br>',
  '<br>',
  '<b>ConfusionMatrix tab</b>:&nbsp; Confusion matrix, sensitivity,',
  'specificity and related data from caret confusionMatrix function',
  'applied to an out-of-sample validation dataset. Use to compare',
  'models.&nbsp; [A final test set is reserved and is not used in the',
  'current implementation.]<br>',
  '<br>',
  '<b>DotPlot tab</b>:&nbsp; dotPlot using output from caret varImp',
  'function that gives importance rankings of variables included in the',
  'fit.<br>',
  '<hr> <b>Reference</b>:&nbsp; Andrew A Hill, Peter LaPan, Yizheng Li',
  'and Steve Haney.&nbsp; <a',
  'href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2080643/">Impact',
  'of image segmentation on high-content screening data quality for',
  'SK-BR-3 cells</a>, <i>BMC Bioinformatics</i>, Sept. 2007.')
html.note<-paste("Note:  Approximate processing times above are for a 2.4 GHz Intel i7-3630QM CPU.",
                 "All models work on a local server, but only lda and rpart work at shinyapps.io.")



