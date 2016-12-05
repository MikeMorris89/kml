


my.plot1<-function(my.data,my.factor,my.pred){
  
  # #testing
  # my.data<-"train"
  # my.factor<-"Child"
  # my.pred<-"Survived"
  
  df1<-get(my.data,envir = .GlobalEnv)
  df1<-data.frame(df1[my.factor], df1[my.pred])
  df1<-prop.table(table(df1),1)
  
  colnames(df1)<-gsub("1",my.pred,gsub("0",paste("not",my.pred,sep=" "),colnames(df1)))
  data.frame(row.names(df1),df1[,1],df1[,2])
  df2<-data.frame(row.names(df1),df1[,1],df1[,2])
  colnames(df2)<-c(my.factor,colnames(df1)[1],colnames(df1)[2])
  
  p<-plot_ly(df2,
          y = ~`not Survived`,
          x = row.names(df2),
          name=colnames(df1)[1]
          ,type="bar")  %>%
    add_trace(y = ~Survived , name = colnames(df1)[2]) %>%
    layout(yaxis = list(title = 'Percent age of passengers'), barmode = 'stack')
  
  return(p)
}


my.data1<-function(my.data,my.factor,my.pred){
  
  #testing
  # my.data<-"train"
  # my.factor<-"Sex"
  # my.pred<-"Survived"
  
  df1<-get(my.data,envir = .GlobalEnv)
  df1<-data.frame(df1[my.factor], df1[my.pred])
  df1<-prop.table(table(df1),1)
  
  colnames(df1)<-gsub("1",my.pred,gsub("0",paste("not",my.pred,sep=" "),colnames(df1)))
       
  df2<-data.frame(row.names(df1),df1[,1],df1[,2])
  colnames(df2)<-c(my.factor,colnames(df1)[1],colnames(df1)[2])
  return(data.frame(df2))
}


my.plot.abs1<-function(my.data,my.factor,my.pred){
  
  # #testing
  # my.data<-"train"
  # my.factor<-"Sex"
  # my.pred<-"Survived"
  
  df1<-get(my.data,envir = .GlobalEnv)
  df1<-data.frame(df1[my.factor], df1[my.pred])
  df1<-table(df1)
  
  colnames(df1)<-gsub("1",my.pred,gsub("0",paste("not",my.pred,sep=" "),colnames(df1)))
  data.frame(row.names(df1),df1[,1],df1[,2])
  df2<-data.frame(row.names(df1),df1[,1],df1[,2])
  colnames(df2)<-c(my.factor,colnames(df1)[1],colnames(df1)[2])
  
  p<-plot_ly(df2,
             y = ~`not Survived`,
             x = row.names(df2),
             name=colnames(df1)[1]
             ,type="bar")  %>%
    add_trace(y = ~Survived , name = colnames(df1)[2]) %>%
    layout(yaxis = list(title = 'Percent age of passengers'), barmode = 'stack')
  
  return(p)
}


my.data.abs1<-function(my.data,my.factor,my.pred){
  
  #testing
  # my.data<-"train"
  # my.factor<-"Sex"
  # my.pred<-"Survived"
  
  df1<-get(my.data,envir = .GlobalEnv)
  df1<-data.frame(df1[my.factor], df1[my.pred])
  df1<-table(df1)
  
  colnames(df1)<-gsub("1",my.pred,gsub("0",paste("not",my.pred,sep=" "),colnames(df1)))
       
  df2<-data.frame(row.names(df1),df1[,1],df1[,2])
  colnames(df2)<-c(my.factor,colnames(df1)[1],colnames(df1)[2])
  return(df2)
}