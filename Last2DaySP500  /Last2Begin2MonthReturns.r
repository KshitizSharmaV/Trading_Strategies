library(Rcmdr)
rm(list=ls())
data=read.csv("Workbook3.csv")

dates <- as.Date(as.character(data[,1]),"%d/%m/%y")
dates=format(dates, format="%Y/%m/%d")
dates

new.data.frame=data.frame(Dates=dates,Close.Price=data$Close,Returns=(data$Daily.Returns)*100)


final.data.frame=data.frame(Dates=character(),Close.Price=double(),Returns=double())
final.data.frame2=data.frame(Dates=character(),Close.Price=double(),Returns=double())
months=c(1:12)
years=c(1991:2018)
for(i in 1:length(years)){
  temp1=new.data.frame[format.Date(new.data.frame$Dates, "%Y")==years[i],]
  for(j in 1:length(months)){
    temp2=temp1[as.integer(format.Date(temp1$Dates, "%m"))==months[j],]
    if(nrow(temp2)>0){
      final.data.frame=rbind(final.data.frame,head(temp2,2))
      final.data.frame=rbind(final.data.frame,tail(temp2,2))
      
      final.data.frame2=rbind(final.data.frame2,temp2[3:(nrow(temp2)-2),])
    }
  }
}
write.csv(final.data.frame,"AnalysisDB.csv")


new.data.frame=final.data.frame2

rownames(new.data.frame)=c(1:nrow(new.data.frame))
rownames(final.data.frame)=c(1:nrow(final.data.frame))


# 2012
mean(new.data.frame$Returns[4267:length(new.data.frame$Returns)])
mean(final.data.frame$Returns[1003:length(final.data.frame$Returns)])

sd(new.data.frame$Returns[4267:length(new.data.frame$Returns)])
sd(final.data.frame$Returns[1003:length(final.data.frame$Returns)])


# 2013
mean(new.data.frame$Returns[4469:length(new.data.frame$Returns)])
mean(final.data.frame$Returns[1051:length(final.data.frame$Returns)])

sd(new.data.frame$Returns[4469:length(new.data.frame$Returns)])
sd(final.data.frame$Returns[1051:length(final.data.frame$Returns)])


# 2014
mean(new.data.frame$Returns[4673:length(new.data.frame$Returns)])
mean(final.data.frame$Returns[1099:length(final.data.frame$Returns)])

sd(new.data.frame$Returns[4673:length(new.data.frame$Returns)])
sd(final.data.frame$Returns[1099:length(final.data.frame$Returns)])

# 2015
mean(new.data.frame$Returns[4877:length(new.data.frame$Returns)])
mean(final.data.frame$Returns[1147:length(final.data.frame$Returns)])

sd(new.data.frame$Returns[4877:length(new.data.frame$Returns)])
sd(final.data.frame$Returns[1147:length(final.data.frame$Returns)])

# 2016
mean(new.data.frame$Returns[5081:length(new.data.frame$Returns)])
mean(final.data.frame$Returns[1195:length(final.data.frame$Returns)])

sd(new.data.frame$Returns[5081:length(new.data.frame$Returns)])
sd(final.data.frame$Returns[1195:length(final.data.frame$Returns)])



NTOM=c()
Dates1=c()
for(i in 1:(nrow(new.data.frame)-3)){
  NTOM[i]=new.data.frame$Returns[i]+new.data.frame$Returns[i+1]+new.data.frame$Returns[i+2]+new.data.frame$Returns[i+3]
  Dates1[i]=as.character(new.data.frame$Dates[i])
}
NTOM.returns=c()
NTOM.returns$Returns=NTOM
NTOM.returns$Dates=Dates1
NTOM.returns=as.data.frame(NTOM.returns)

write.csv(NTOM.returns,"NTOM.csv")










i=3
t=1
TOM=c()
Dates2=c()

while(i<nrow(final.data.frame)){
  TOM[t]=final.data.frame$Returns[i]+final.data.frame$Returns[i+1]+final.data.frame$Returns[i+2]+final.data.frame$Returns[i+3]
  Dates2[t]=as.character(final.data.frame$Dates[i])
  i=i+4
  t=t+1
}

TOM.returns=c()
TOM.returns$Returns=TOM
TOM.returns$Dates=Dates2
TOM.returns=as.data.frame(TOM.returns)
TOM.returns=TOM.returns[-nrow(TOM.returns),]

write.csv(TOM.returns,"TOM.csv")



TOM.returns=as.data.frame(TOM.returns)
NTOM.returns=as.data.frame(NTOM.returns)







TOM.returns[TOM.returns < 0] <- NA
#NTOM.returns[NTOM.returns < 0] <- NA

TOM.returns <- TOM.returns[!is.na(TOM.returns$Returns),]
#NTOM.returns <- NTOM.returns[!is.na(NTOM.returns$Returns),]


y <- c(TOM.returns$Returns, NTOM.returns$Returns)
group <- as.factor(c(rep(1, length(TOM.returns$Returns)), rep(2, length(NTOM.returns$Returns))))
test=leveneTest(y, group)
test


# I am comparing over here only those TOM events which are +ve and 
# comparing with all the rest of days.

