rm(list=ls())
library(Rcmdr)

# Read the file for closing data 
data=read.csv("Workbook3.csv")

# Get Dates
dates <- as.Date(as.character(data[,1]),"%d/%m/%y")
dates=format(dates, format="%d/%m/%Y")

# Get Closing Prices and 50 Day MA
closing.prices = data$Close
MA.50 = data$X50.D.MA.Price


# Testing strategy 
# Strategy - Check when the closing price crosses 50 Day MA on day (x+1) downward
# And on day (x) S&P500 is above the MA

# Storing length in l 
l = length(closing.prices)

# Data Frame for storing the closing days when S&P breaks MA 50
data.base = data.frame(Date=character(),Closing.Price.0 = double(),MA.Price=double(),Price.1=double(),Price.2=double(),Price.3=double(),Price.4=double(),Price.5=double(),Price.6=double(),Price.7=double(),Price.8=double(),Price.9=double(),Price.10=double(),Price.11=double(),Price.12=double(),Price.13=double(),Price.14=double(),Price.15=double(),Price.16=double(),Price.17=double(),Price.18=double(),Price.19=double(),Price.20=double(),Price.21=double(),Price.22=double(),Price.23=double(),Price.24=double(),Price.25=double(),Price.26=double(),Price.27=double(),Price.28=double(),Price.29=double(),Price.30=double(),Price.31=double(),Price.32=double(),Price.33=double(),Price.34=double(),Price.35=double(),Price.36=double(),Price.37=double(),Price.38=double(),Price.39=double(),Price.40=double(),Price.41=double(),Price.42=double(),Price.43=double(),Price.44=double(),Price.45=double(),Price.46=double(),Price.47=double(),Price.48=double(),Price.49=double(),Price.50=double(), Return.1=double(),Return.2=double(),Return.3=double(),Return.4=double(),Return.5=double(),Return.6=double(),Return.7=double(),Return.8=double(),Return.9=double(),Return.10=double(),Return.11=double(),Return.12=double(),Return.13=double(),Return.14=double(),Return.15=double(),Return.16=double(),Return.17=double(),Return.18=double(),Return.19=double(),Return.20=double(),Return.21=double(),Return.22=double(),Return.23=double(),Return.24=double(),Return.25=double(),Return.26=double(),Return.27=double(),Return.28=double(),Return.29=double(),Return.30=double(),Return.31=double(),Return.32=double(),Return.33=double(),Return.34=double(),Return.35=double(),Return.36=double(),Return.37=double(),Return.38=double(),Return.39=double(),Return.40=double(),Return.41=double(),Return.42=double(),Return.43=double(),Return.44=double(),Return.45=double(),Return.46=double(),Return.47=double(),Return.48=double(),Return.49=double(),Return.50=double())

i=52
t=0
while(i < (l-50)){
  if(MA.50[i] > closing.prices[i]){
    if(MA.50[i-1] < closing.prices[i-1]){
      temp<-data.frame(Date=dates[i] , Closing.Price.0 = closing.prices[i],MA.Price = MA.50[i],Price.1=closing.prices[i+1],Price.2=closing.prices[i+2],Price.3=closing.prices[i+3],Price.4=closing.prices[i+4],Price.5=closing.prices[i+5],Price.6=closing.prices[i+6],Price.7=closing.prices[i+7],Price.8=closing.prices[i+8],Price.9=closing.prices[i+9],Price.10=closing.prices[i+10],Price.11=closing.prices[i+11],Price.12=closing.prices[i+12],Price.13=closing.prices[i+13],Price.14=closing.prices[i+14],Price.15=closing.prices[i+15],Price.16=closing.prices[i+16],Price.17=closing.prices[i+17],Price.18=closing.prices[i+18],Price.19=closing.prices[i+19],Price.20=closing.prices[i+20],Price.21=closing.prices[i+21],Price.22=closing.prices[i+22],Price.23=closing.prices[i+23],Price.24=closing.prices[i+24],Price.25=closing.prices[i+25],Price.26=closing.prices[i+26],Price.27=closing.prices[i+27],Price.28=closing.prices[i+28],Price.29=closing.prices[i+29],Price.30=closing.prices[i+30],Price.31=closing.prices[i+31],Price.32=closing.prices[i+32],Price.33=closing.prices[i+33],Price.34=closing.prices[i+34],Price.35=closing.prices[i+35],Price.36=closing.prices[i+36],Price.37=closing.prices[i+37],Price.38=closing.prices[i+38],Price.39=closing.prices[i+39],Price.40=closing.prices[i+40],Price.41=closing.prices[i+41],Price.42=closing.prices[i+42],Price.43=closing.prices[i+43],Price.44=closing.prices[i+44],Price.45=closing.prices[i+45],Price.46=closing.prices[i+46],Price.47=closing.prices[i+47],Price.48=closing.prices[i+48],Price.49=closing.prices[i+49],Price.50=closing.prices[i+50], Return.1=(closing.prices[i+1]/closing.prices[i]-1),Return.2=(closing.prices[i+2]/closing.prices[i]-1),Return.3=(closing.prices[i+3]/closing.prices[i]-1),Return.4=(closing.prices[i+4]/closing.prices[i]-1),Return.5=(closing.prices[i+5]/closing.prices[i]-1),Return.6=(closing.prices[i+6]/closing.prices[i]-1),Return.7=(closing.prices[i+7]/closing.prices[i]-1),Return.8=(closing.prices[i+8]/closing.prices[i]-1),Return.9=(closing.prices[i+9]/closing.prices[i]-1),Return.10=(closing.prices[i+10]/closing.prices[i]-1),Return.11=(closing.prices[i+11]/closing.prices[i]-1),Return.12=(closing.prices[i+12]/closing.prices[i]-1),Return.13=(closing.prices[i+13]/closing.prices[i]-1),Return.14=(closing.prices[i+14]/closing.prices[i]-1),Return.15=(closing.prices[i+15]/closing.prices[i]-1),Return.16=(closing.prices[i+16]/closing.prices[i]-1),Return.17=(closing.prices[i+17]/closing.prices[i]-1),Return.18=(closing.prices[i+18]/closing.prices[i]-1),Return.19=(closing.prices[i+19]/closing.prices[i]-1),Return.20=(closing.prices[i+20]/closing.prices[i]-1),Return.21=(closing.prices[i+21]/closing.prices[i]-1),Return.22=(closing.prices[i+22]/closing.prices[i]-1),Return.23=(closing.prices[i+23]/closing.prices[i]-1),Return.24=(closing.prices[i+24]/closing.prices[i]-1),Return.25=(closing.prices[i+25]/closing.prices[i]-1),Return.26=(closing.prices[i+26]/closing.prices[i]-1),Return.27=(closing.prices[i+27]/closing.prices[i]-1),Return.28=(closing.prices[i+28]/closing.prices[i]-1),Return.29=(closing.prices[i+29]/closing.prices[i]-1),Return.30=(closing.prices[i+30]/closing.prices[i]-1),Return.31=(closing.prices[i+31]/closing.prices[i]-1),Return.32=(closing.prices[i+32]/closing.prices[i]-1),Return.33=(closing.prices[i+33]/closing.prices[i]-1),Return.34=(closing.prices[i+34]/closing.prices[i]-1),Return.35=(closing.prices[i+35]/closing.prices[i]-1),Return.36=(closing.prices[i+36]/closing.prices[i]-1),Return.37=(closing.prices[i+37]/closing.prices[i]-1),Return.38=(closing.prices[i+38]/closing.prices[i]-1),Return.39=(closing.prices[i+39]/closing.prices[i]-1),Return.40=(closing.prices[i+40]/closing.prices[i]-1),Return.41=(closing.prices[i+41]/closing.prices[i]-1),Return.42=(closing.prices[i+42]/closing.prices[i]-1),Return.43=(closing.prices[i+43]/closing.prices[i]-1),Return.44=(closing.prices[i+44]/closing.prices[i]-1),Return.45=(closing.prices[i+45]/closing.prices[i]-1),Return.46=(closing.prices[i+46]/closing.prices[i]-1),Return.47=(closing.prices[i+47]/closing.prices[i]-1))
      temp <- c(temp,Return.48=(closing.prices[i+48]/closing.prices[i]-1),Return.49=(closing.prices[i+49]/closing.prices[i]-1),Return.50=(closing.prices[i+50]/closing.prices[i]-1))    
      data.base<-rbind(data.base,temp)
      t=1
    }
  }
  # Custom Incerementaion for While Loop
  if(t==1){
    i=i+51
    t=0
  }else{
    i=i+1
  }
}

write.csv(data.base,"Breaking50dmovingaverage.csv")


# Second Part to find 2,3,4,5... 10 days returns in the SP for all data set

different.days.gaps.returns = function(days){
  df = data.frame(Date=character(),Close.price=double(),Return=double())
  temp = data.frame(Date=dates[51],Close.price=closing.prices[51],Return=0)
  df = rbind(df,temp)
  i=51+days
  while(i < l){
    temp = data.frame(Date=dates[i],Close.price=closing.prices[i],Return=(closing.prices[i]/closing.prices[i-days]-1))
    df = rbind(df,temp)
    # Custom Incerementaion for While Loop
    i=i+days 
  }
  return(df)
}

# Finding Levene Test function
d <- 5
t=54
Levene_test=c()
for(i in 1:50) { 
  nam <- paste("d", i, sep = "")
  assign(nam, rnorm(3)+d)
  nam = different.days.gaps.returns(i)
  
  value1=nam$Return
  
  value2=data.base[,t]
  t=t+1
  
  
  y <- c(value1, value2)
  
  group <- as.factor(c(rep(1, length(value1)), rep(2, length(value2))))
  Levene_test=append(Levene_test,leveneTest(y, group)$`Pr(>F)`[1])
}

df=data.frame(Number=c(1:50),"Pr(>F)"=Levene_test)
write.csv(Levene_test,"50SD.csv")

