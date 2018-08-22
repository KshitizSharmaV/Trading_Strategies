rm(list=ls())
data1=read.csv("SH.csv")
data2=read.csv("WH.csv")
data3=read.csv("CH.csv")


back1=read.csv("Backwardation_for_Soya_Bean.csv")
back2=read.csv("Backwardation_for_Wheat.csv")
back3=read.csv("Backwardation_for_Corn.csv")

mav <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}


final.data.base=NULL


#data=data3
#back=back3
#n=63
analysis.function = function(n,data,back,result.data.frame){
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






result.data.frame=data.frame(Date=character(),Security_ID=integer(),Month0_Raw_Price=double(),Month1_Raw_Price=double(),Month5_Raw_Price=double(),Month1_Minus_Month0=double(),Month5_Minus_Month0=double(),TR20=double(),Adj.Price=double(),AdjustedLCPrice30=double(),AdjustedLCPrice60=double(),AdjustedLCPrice90=double(),Return30=double(),Return60=double(),Return90=double())
result.data.frame=analysis.function(63,data3,back3,result.data.frame)
result.data.frame=analysis.function(64,data2,back2,result.data.frame)
result.data.frame=analysis.function(60,data1,back1,result.data.frame)

write.csv(result.data.frame,"answer.csv")





#plot(result.data.frame$Month1_Minus_Month0,type="l")
#plot(result.data.frame$Month5_Minus_Month0,type="l")
#plot(result.data.frame$Return30,type="l")
#plot(result.data.frame$Return60,type="l")
#plot(result.data.frame$Return90,type="l")


