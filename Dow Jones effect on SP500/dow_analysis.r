# The next step is to identify those dates on which all 30 stocks in the DOW 30 are down, 
# and identify those dates on which all 30 of the stocks in the DOW 30 are up . 
# As a  follow-up, can you please send me these dates?


# The hypothesis is that when all 30 stocks are down, this is potentially bullish for the stock market.  
# Similarly, when all 30 stocks are up, this is potentially bearish for the stock market. 

rm(list=ls())
data=read.csv("DowData.csv")
new.data=NULL
col.names=colnames(data)
for(i in 2:(ncol(data))){
  temp=data[,i]
  first=temp[2:length(temp)]
  second=temp[1:(length(temp)-1)]
  return = (first-second)/second * 100
  new.data=cbind(new.data,return)
}
rownames(new.data)=data[,1][2:length(data[,1])]
colnames(new.data)=col.names[2:43]
new.data=as.data.frame(new.data)


ldf <- list() # creates a list
listcsv <- dir(pattern = "data.*csv") # creates the list of all the csv files in the directory
listcsv 
for (i in 5:length(listcsv)){
  ldf <- read.csv(listcsv[i])
  ldf<-read.csv("sears.csv")
  date=ldf$date
  
  for(k in 1:length(date)){
    if(nchar(date[k])==5){
      date[k]=paste0("0", date[k])
    }
  }
  
  date=as.Date(as.character(date),format="%d%m%y")
  print("Hello1")
  price=ldf$PRC
  print("Hello1")
  first=price[2:length(price)]
  second=price[1:(length(price)-1)]
  return=NULL
  return = cbind((first-second)/second * 100)
  rownames(return)=as.character(date[2:length(date)])
  colnames(return)=ldf$TICKER[1]
  
  #new.data <- merge(new.data, return, by=0, all=TRUE) 
  #new.data=as.data.frame(new.data)
  write.csv(return,"EKreturns.csv")
  break
}



write.csv(new.data,"temp.csv")


