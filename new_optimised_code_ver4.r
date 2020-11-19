#install.packages("smooth")
#install.packages("pbapply")
library(smooth)
library(utils)
library(pbapply)
library(tidyr)
library(dplyr)
library(forecast)
library(zoo)

#install.packages("tidyverse")
library(tidyverse)
options(scipen=999)


##############################################################INPUT PROCEDURE#######################################################################
workingDir <- choose.dir(default = "D:/", caption = "Select Working Directory")
#workingDir <- "D:/Storage/Test/Master BL"
setwd(workingDir)
#wd<-getwd()

# Read Input Files --------------------------------------------------------
#inputParam <- read.csv("train.csv", stringsAsFactors = FALSE)
salesInput<-read.csv("train.csv",stringsAsFactors = FALSE)
salesInput=salesInput[,c("Key","Period","Values")]
month.or.week=as.numeric(substr(salesInput$Period,5,6))
month.week.flag=ifelse(length(month.or.week[month.or.week>12])>0,1,0)

if(month.week.flag==1)
  {salesInput$Period=as.Date(paste(salesInput$Period,"1"), "%Y%W%u")
  }else{
    salesInput$Period=as.Date(paste(salesInput$Period,"01"),format="%Y%m%d")
}
#movAvgWts<-read.csv("WTS.csv",stringsAsFactors = FALSE)


########################################################################## ############################################
unique_key=(unique(salesInput[,'Key']))  ################unique no of keys##########
# test_key=salesInput[salesInput$Key==unique_key[1],]
# test_key$Period=as.Date(paste(test_key$Period,"01"),format="%Y%m%d")
# test_key=test_key[order(test_key$Period),]
frequency_length=ifelse(month.week.flag==1,52,12) #########################for monthly,yearly,weekly frequency  should be given (12,365.25,52) respectively #################################################
h=ifelse(month.week.flag==1,104,24)   ###########no of period to forecast #############################
list_keys=pblapply(unique_key,function(x)salesInput[salesInput$Key==x,])

ets.model.build=function(df,h)
{
  df=list_keys[[1]]
  df=df[order(df$Period),]
  if (h==104){
  tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = 52)
  }else{
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
    }
  forecast.fit=ets(tsInput,damped = TRUE,additive.only = F,model = 'ZZZ',upper = c(rep(0.9999, 3), 0.98),
                opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
                nmse = 3,allow.multiplicative.trend=T)

  

  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  
  #View(ets_forecast_df)
  return(forecast.df)

}
arima.model.build=function(df,h,order=NULL)
{
  # df=df[order(df$Period),]
  # ORDER=NULL
  if (h==104){
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
  }else{
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
  }
  
  
  if(is.null(order))
    {
    forecast.fit <- auto.arima(tsInput, max.order = 11, max.p=5,max.q=5,seasonal = FALSE, stepwise = FALSE, allowmean = TRUE, approximation = FALSE)
    }else
    {
      forecast.fit=Arima(tsInput,order=c(as.numeric(arimaorder(forecast.fit))))
      
    }
  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  #forecast.df['order']=as.numeric(arimaorder(forecast.fit))
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  #View(ets_forecast_df)
  return(forecast.df)
  
}


arima.model.noapproximation.build=function(df,h)
{
  df=df[order(df$Period),]
  
  if (h==104){
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
  }else{
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
  }
  
  forecast.fit <- auto.arima(tsInput, max.order = 11, max.p=5,max.q=5,seasonal = FALSE, stepwise = FALSE, allowmean = TRUE, approximation = FALSE)
  
  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  #View(ets_forecast_df)
  return(forecast.df)
  
}

arima.deepevaluation.build=function(df,h)
{
  df=df[order(df$Period),]
  
  if (h==104){
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
  }else{
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
  }
  
  forecast.fit <- auto.arima(tsInput, max.p=5,max.q=5,seasonal = FALSE, stepwise = FALSE, allowmean = TRUE, parallel = TRUE, num.cores = 4)

  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  #View(ets_forecast_df)
  return(forecast.df)
  
}




exponential.smoothing.build=function(df,h)
{
  #df=list_keys[[1]]
  df=df[order(df$Period),]
  
  if (h==104){
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
  }else{
    tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
  }
  
  forecast.fit=ets(tsInput,additive.only = F,model = 'ANN',upper = c(rep(0.9999, 3), 0.98),
                   opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
                   nmse = 3,allow.multiplicative.trend=T)
  
  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  #View(ets_forecast_df)
  return(forecast.df)
  
}


stl.ets.build=function(df,h)
{
 # df=list_keys[['24.5OZ-VASELINE-HAND & BODY EXCL SUN']]
  df=df[order(df$Period),]
  tryCatch({
  
    if (h==104){
      tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
    }else{
      tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
    }
    
  forecast.fit<- stlm(tsInput, s.window = "periodic", method = c("ets"), etsmodel = "ZZN", allow.multiplicative.trend = TRUE)

  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  },error=function(e)
  {
    print(unique(df$Key)[1])
    temp.date=seq(df$Period[nrow(df)],by="week",length.out=h+1)
    #forecast.df=NULL
    forecast.df=as.data.frame(temp.date[2:length(temp.date)])
    forecast.df[,'Model']="error_model"
    forecast.df['key']=unique(df$Key)[1]
    forecast.df['KF']='forecast'
    forecast.df['Value']=0
    colnames(forecast.df)=c('Period','Model','Key','KF','Value')
    #forecast.df=NA
  }
  )
  
  #View(ets_forecast_df)
  return(forecast.df)
  
}

stl.arima.build=function(df,h)
{
  df=df[order(df$Period),]
  tryCatch({
    if (h==104){
      tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%W'))), frequency = frequency_length)
    }else{
      tsInput <- ts(df$Values, start = c(as.numeric(format(min(df$Period),'%Y')),as.numeric(format(min(df$Period),'%m'))), frequency = frequency_length)
    }
    
  forecast.fit<- stlm(tsInput, s.window = "periodic", method = c("arima"), allow.multiplicative.trend = TRUE)
  
  
  
  forecast.predict=forecast(forecast.fit,h=h)
  
  forecast.df=as.data.frame(as.numeric(forecast.predict$mean))
  forecast.df[,'Date']=as.Date(as.yearmon(time(forecast.predict$mean)))
  forecast.df[,'Model']=forecast.predict$method
  forecast.df['key']=unique(df$Key)[1]
  forecast.df['KF']='forecast'
  
  colnames(forecast.df)=c('Value','Period','Model','Key','KF')
  
  },error=function(e)
  {
    print(unique(df$Key)[1])
    temp.date=seq(df$Period[nrow(df)],by="week",length.out=h+1)
    #forecast.df=NULL
    forecast.df=as.data.frame(temp.date[2:length(temp.date)])
    forecast.df[,'Model']="error_model"
    forecast.df['key']=unique(df$Key)[1]
    forecast.df['KF']='forecast'
    forecast.df['Value']=0
    colnames(forecast.df)=c('Period','Model','Key','KF','Value')
    #forecast.df=NA
  }
  )
  return(forecast.df)
  
}



make.df.fromlist=function(list.name,df.name)
{
if(exists(list.name))
 {
  return(assign(df.name,do.call(rbind,eval(parse(text=list.name))),envir = globalenv()))
}else
{
  print("object does not exist")
}
  
}
# list.ets.forecast=pblapply(list_keys,function(x)ets.model.build(x,12))
#  list.es.forecast=pblapply(list_keys,function(x)exponential.smoothing.build(x,12))
#  list.arima.forecast=pblapply(list_keys,function(x)arima.model.build(x,12))
#  list.stlets.forecast=pblapply(list_keys,function(x)stl.ets.build(x,12))
# list.arima_na.forecast=pblapply(list_keys,function(x)arima.model.noapproximation.build(x,12))
# list.stlarima.forecast=pblapply(list_keys,function(x)stl.arima.build(x,12))

# make.df.fromlist("list.ets.forecast","df.ets.forecast")
# make.df.fromlist("list.stlets.forecast","df.stlets.forecast")
# make.df.fromlist("list.stlarima.forecast","df.es.forecast")
# make.df.fromlist("list.arima.forecast","df.arima.forecast")
# make.df.fromlist("list.es.forecast","df.es.forecast")


 
# View(list.stlets.forecast[[88]])
# plot(list.stlets.forecast[[1]]$Period,list.stlets.forecast[[1]]$Value)
# lines(list_keys[[1]]$Values,col='red')
# ets_forecast[2]
# accuracy(ets_forecast)
# ets_forecast
# 


########################################################NO OF KEYS TO BE FORECASTED##########################################################



custom.accuracy.calculator=function(df,actual_col1,predicted_col2)
{
  #accuracy=abs(actual_col1-predicted_col2)
  actual=actual_col1
  baseline=predicted_col2
  accuracy=(1-(abs(actual-baseline)/actual))
  accuracy.mean=mean(accuracy)
  return (accuracy.mean)
}
#custom.accuracy.calculator(tempmodel.list.final[[1]],tempmodel.list.final[[1]]$actual,tempmodel.list.final[[1]]$Value)

x=3
model.vector=c("exponential.smoothing.build","arima.model.build","stl.ets.build","stl.arima.build","ets.model.build")

names(list_keys)=unique_key

accuracy.calculation=function(df,model.vector,x)
{
  #df=list_keys[['24.5OZ-VASELINE-HAND & BODY EXCL SUN']]
  #test.df=data.frame()
  df=df[order(df$Period),]
  rows_neeeded=1:(nrow(df)-x)
  train_data=df[rows_neeeded,]
  test_data=df[-rows_neeeded,]
  test_data$Values[test_data$Values==0]=1
  
  #temp.model.list=list()
    tempmodel.list=pblapply(model.vector,function(i){print(i) 
      match.fun(i)(train_data,h=nrow(test_data))})
    tempmodel.list.final=pblapply(tempmodel.list,function(x){x$actual=test_data$Values 
    return(x)})
    
    accuracy.list=pblapply(tempmodel.list.final,function(x){custom.accuracy.calculator(x,x$Value,x$actual)})
    accuracy.vector=unlist(accuracy.list)
    names(accuracy.vector)=model.vector
    print(df$Key[1])
    if(!is.nan(max(accuracy.vector)) & !is.infinite(max(accuracy.vector)))
       {
      
    test.df=data.frame(Key=tempmodel.list[[which(accuracy.vector==max(accuracy.vector))[1]]]$Key[1])
    test.df$Model=names(accuracy.vector[which(accuracy.vector==max(accuracy.vector))[1]])
      
    }else{
      test.df=data.frame(Key=tempmodel.list[[1]]$Key[1])
      test.df$Model="Model_cant_fit"
    }
    return(test.df)
}

#table(model.selected.df$Model)
models.selected=pblapply(list_keys,function(x){accuracy.calculation(x,model.vector ,3)})
model.selected.df=do.call(rbind,models.selected)
row.names(model.selected.df)=NULL
#View(list_keys[["441948025E2071101#113516"]])
#View(tempmodel.list[[2]])
pb = txtProgressBar(min = 0, max = nrow(model.selected.df), initial = 0) 
list.final.forecast=list()
a=1
for(i in 1:nrow(model.selected.df))
{
  setTxtProgressBar(pb,i)
  if((unique(list_keys[[i]]$Key)==model.selected.df[i,]$Key) & (model.selected.df[i,]$Model!="Model_cant_fit"))
  {
  list.final.forecast[[a]]=match.fun(model.selected.df[i,]$Model)(list_keys[[i]],h=24)
  }
  a=a+1
}
final.forecast.df=do.call(rbind,list.final.forecast)
final.forecast.df$Model=NULL
final.forecast.df=merge(final.forecast.df,model.selected.df,by="Key")

final.actuals.df=salesInput
final.actuals.df=merge(final.actuals.df,model.selected.df,by="Key")

final.actuals.df$KF="Actual"

colnames(final.forecast.df)=c("Key","Values","Period","KF","Model")   
final.forecast.df$Period=format(final.forecast.df$Period,'%Y%m')
final.actuals.df$Period=format(final.actuals.df$Period,'%Y%m')

final.output=rbind(final.actuals.df,final.forecast.df)
final.output=final.output[,c("Key","Period","Values","Model","KF")]
write.csv(final.output,"final.output2.csv",row.names = F) 

