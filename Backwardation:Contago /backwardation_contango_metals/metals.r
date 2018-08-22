rm(list=ls())
data1=read.csv("GC.csv")
data2=read.csv("SI.csv")

back1=read.csv("Backwardation_for_Gold.csv")
back2=read.csv("Backwardation_for_Silver.csv")


mav <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}


final.data.base=NULL


#data=data3
#back=back3
#n=63
analysis.function2 = function(n,data,back,result.data.frame){
  f=9
  dates.back=back$Date
  dates.data=data$YYYYMMDD
  
  Adjusted.Price=data$Offset+data$LC
  Moving.average.20 = mav(data$TRL,20)
  counter=0
  for(i in 20:length(dates.data)){
    t=  back[back$Date==as.character(dates.data[i]),]
    if(is.data.frame(t) && nrow(t)==0){
      counter=counter+1
    }else{
      temp=data.frame(Date=dates.data[i],Security_ID=n,Month0_Raw_Price=t[f][1,],Month1_Raw_Price=t[f+1][1,],Month5_Raw_Price=t[f+4][1,],Month1_Minus_Month0=(t[f+1][1,]-t[f][1,]),Month5_Minus_Month0=(t[f+4][1,]-t[f][1,]),TR20=Moving.average.20[i],Adj.Price=Adjusted.Price[i],AdjustedLCPrice30=Adjusted.Price[i+30] ,AdjustedLCPrice60=Adjusted.Price[i+60],AdjustedLCPrice90=Adjusted.Price[i+90],Return30=(Adjusted.Price[i+30]-Adjusted.Price[i])/Moving.average.20[i],Return60=(Adjusted.Price[i+60]-Adjusted.Price[i])/Moving.average.20[i],Return90=(Adjusted.Price[i+90]-Adjusted.Price[i])/Moving.average.20[i])  
      result.data.frame=rbind(result.data.frame,temp)  
    }
  }
  return(result.data.frame)
}

analysis.function = function(n,data,back,result.data.frame){
  
  dates.back=back$Date
  dates.data=data$YYYYMMDD
  
  Adjusted.Price=data$Offset+data$LC
  Moving.average.20 = mav(data$TRL,20)
  counter=0
  for(i in 20:length(dates.data)){
    t=  back[back$Date==as.character(dates.data[i]),]
    if(is.data.frame(t) && nrow(t)==0){
      counter=counter+1
    }else{
      
      temp=data.frame(Date=dates.data[i],Security_ID=n,Month0_Raw_Price=t[10]$CL0.value,Month1_Raw_Price=t[11]$CL1.value,Month5_Raw_Price=t[13]$CL3.value,Month1_Minus_Month0=(t[11]$CL1.value-t[10]$CL0.value),Month5_Minus_Month0=(t[13]$CL3.value-t[10]$CL0.value),TR20=Moving.average.20[i],Adj.Price=Adjusted.Price[i],AdjustedLCPrice30=Adjusted.Price[i+30] ,AdjustedLCPrice60=Adjusted.Price[i+60],AdjustedLCPrice90=Adjusted.Price[i+90],Return30=(Adjusted.Price[i+30]-Adjusted.Price[i])/Moving.average.20[i],Return60=(Adjusted.Price[i+60]-Adjusted.Price[i])/Moving.average.20[i],Return90=(Adjusted.Price[i+90]-Adjusted.Price[i])/Moving.average.20[i])  
      result.data.frame=rbind(result.data.frame,temp)  
      
    }
  }
  return(result.data.frame)
}






result.data.frame=data.frame(Date=character(),Security_ID=integer(),Month0_Raw_Price=double(),Month1_Raw_Price=double(),Month5_Raw_Price=double(),Month1_Minus_Month0=double(),Month5_Minus_Month0=double(),TR20=double(),Adj.Price=double(),AdjustedLCPrice30=double(),AdjustedLCPrice60=double(),AdjustedLCPrice90=double(),Return30=double(),Return60=double(),Return90=double())
result.data.frame=analysis.function(50,data1,back1,result.data.frame)
result.data.frame=analysis.function(51,data2,back2,result.data.frame)


write.csv(result.data.frame,"answer.csv")





#plot(result.data.frame$Month1_Minus_Month0,type="l")
#plot(result.data.frame$Month5_Minus_Month0,type="l")
#plot(result.data.frame$Return30,type="l")
#plot(result.data.frame$Return60,type="l")
#plot(result.data.frame$Return90,type="l")


