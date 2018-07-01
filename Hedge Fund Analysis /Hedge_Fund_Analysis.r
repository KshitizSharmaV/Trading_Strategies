rm(list=ls())
data=read.csv("Hedge_Fund.csv")
#temp=data[1:7820,]
temp=data
dates <- as.Date(as.character(temp[,1]),"%d/%m/%y")
dates=format(dates, format="%d/%m/%Y")
temp$Date=dates


# Doing the analysis
library(lubridate)

#month(as.POSIXlt(dates, format="%d/%m/%Y"))
#year(as.POSIXlt(dates, format="%d/%m/%Y"))


fund.names=unique(temp$Fund.Name)
year.length=c(1977:2018)


performace.matrix = matrix(nrow=length(fund.names),ncol=length(year.length))
rownames(performace.matrix)=fund.names
colnames(performace.matrix)=year.length

i=1
performace=c()
while(i<length(fund.names)){
  
  t=temp[temp$Fund.Name==fund.names[i],]
  
  years=unique(year(as.POSIXlt(t$Date, format="%d/%m/%Y")))
  
  for(j in 1:length(years)){
    t2=t[year(as.POSIXlt(t$Date, format="%d/%m/%Y"))==years[j],]
    if(length(t2$Date)==12){
      annualized.return=1
      for(k in 1:12){
        annualized.return=annualized.return*(1+t2[k,]$Rate.of.Return/100)
      }
      sharpe.ratio=(annualized.return-1)/(sd(t2[1:12,]$Rate.of.Return)*sqrt(12))
      performace.matrix[as.character(fund.names[i]),as.character(years[j])]=sharpe.ratio
    }
  }
  i=i+1
}

write.csv(performace.matrix,"Fund Performances.csv")
