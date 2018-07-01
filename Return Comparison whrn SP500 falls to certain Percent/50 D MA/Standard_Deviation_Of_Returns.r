rm(list=ls())

data1=read.csv("Part1_Breaking200dmovingaverage.csv")
data2=read.csv("Part1_Breaking100dmovingaverage.csv")
data3=read.csv("Part1_Breaking50dmovingaverage.csv")


sd.200=c()
sd.100=c()
sd.50=c()
for(i in 14:23){
  sd.200=append(sd.200,sd(data1[,i]))
  sd.100=append(sd.100,sd(data2[,i]))
  sd.50=append(sd.50,sd(data3[,i]))
}





sd.breaking.point