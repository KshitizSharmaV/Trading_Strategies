# To test the hypothesis that the sectors that lose the most money in a downturn for the S&P 500 also 
# gain the most money when the S&P 500 recovers

rm(list=ls())
# Read the data
data=read.csv("Workbook2.csv")

sp.price = data$Close
sp.dates = data$Dates
l = length(sp.price) 

days=22*3

c=0
df=data.frame(Date1=character(),Went.Down=double(),Date2=character(),Min.point=double(),Date3=character(),Recovery.Point=double())
i=1
while(i < (l-days)){
  t=0
  min.price = min(sp.price[i:(i+days)])
  min.pos=which.min(sp.price[i:(i+days)])
  break.point = sp.price[i] - 0.05*sp.price[i]
  if(min.price <=  break.point){
    max.price = c(sp.price[(i+days):(i+2*days)])
    max.pos = which.max(sp.price[(i+days):(i+2*days)])
    break.point.2 = min.price + 0.05*min.price
    for(k in 1:length(max.price)){
      if(break.point.2 <= max.price[k]){
        temp=data.frame(Date1=sp.dates[i],Went.Down=sp.price[i],Date2=sp.dates[i-1+min.pos],Min.point=min.price,Date3=sp.dates[i+days-1+k],Recovery.Point=max.price[k])
        df = rbind(df,temp)
        t=1
        break
      }    
    }
  }
  if(t==1){
    i=i+2*days
  }else{
    i=i+1
  }
}

df

dates <- as.Date(df$Date1,"%d/%m/%y")


library(quantmod)
lookup.symb=c("XLP","XLY","XLE","XLF","XLV","XLI","XLB","XLK","XLU")

# 1. XLP - consumer staples
# 2. XLY   -consumer Discertionary 
# 3. XLE - Energy
# 4. XLF - Financials
# 5.  XLV - Health  XLHE
# 6. XLI - Industrial sector  
# 7.  XLB -Materials
# 8.  XLRE - Real Estate
# 9. XLK - Technology sector
# 10.  XLU -utilities

stockData <- new.env()
getSymbols(lookup.symb, from="1998-12-26", env=stockData, src="yahoo")

CloseMatrix=NULL
for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  CloseMatrix=cbind(CloseMatrix,Cl(tmp))
  colnames(CloseMatrix)[i]=lookup.symb[i]
}



dates.needed=as.Date(index(CloseMatrix),"%d/%m/%y")


df.new=data.frame(Date=as.character(),Went.Most.Done=as.character(),Went.Most.Up=as.character())
temp2=c()
count=0
for(i in 10:length(df$Date1)){
  d1=as.Date(df$Date1[i],"%d/%m/%y")
  d3=as.Date(df$Date3[i],"%d/%m/%y")
  d1.pos=which(dates.needed==d1)
  d2.pos=which(dates.needed==d3)
  temp=CloseMatrix[d1.pos:d2.pos,]
  t=0
  return1=c()
  return2=c()
  temp2[[i]]=temp
  for(j in 1:9){
    max.value=which.max(temp[,j])
    min.value=which.min(temp[,j])  
    return1[j]= (as.numeric(temp[min.value,j])  - as.numeric(temp[1,j]))/as.numeric(temp[1,j])
    return2[j] = (as.numeric(temp[max.value,j]) - as.numeric(temp[min.value,j]))/as.numeric(temp[min.value,j])
  }
  print("New Set")
  print(return1)
  print(return2)
  
  df.temp=data.frame(Date=df$Date1[i],Went.Most.Done=lookup.symb[which.min(return1)],Went.Most.Up=lookup.symb[which.max(return2)])
  if(lookup.symb[which.min(return1)]==lookup.symb[which.max(return2)]){
    count=count+1
  }
  df.new=rbind(df.new,df.temp)
}
df.new
write.csv(df.new,"Results.csv")






















  

