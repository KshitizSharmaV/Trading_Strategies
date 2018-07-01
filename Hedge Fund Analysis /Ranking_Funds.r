rm(list=ls())
data=read.csv("Final Fund Performances.csv")
names=colnames(data)

# Change the year over here to get results from particular set

find.ranking.with.percentile = function(x){
  # Change the Percentile ranking from here
  percentile.ranking=x
  percentile.ranking.list=c()
  
  i = which(names=="X1995")
  while(i<ncol(data)){
    temp = na.omit(data[,c(1,i)])
    temp = temp[order(temp[,2],decreasing = TRUE),]
    pos = percentile.ranking/100 * nrow(temp)
    if(!is.integer(pos)){
      pos=round(pos)  
    }
    if(pos==0){pos=1}
    temp=temp[1:pos,]
    #print(temp)
    percentile.ranking.list[[i]]=temp
    print(length(temp$X))
    i=i+1
  }
  
  
  
  # No need to pay attention, code to store the data in files csv
  i = which(names=="X1995")
  max=1
  while(i<ncol(data)){
    l=length(percentile.ranking.list[[i]][,1])
    if(l>max){
      max=l
    }
    i=i+1
  }
  
  i = which(names=="X1995")
  check = which(names=="X1995")
  l=c(1:max)
  final.data=data.frame(Counter=integer(),Years=character(),Sharpe.Ratios=double(),Spaces=character())
  nam.list=c()
  spaces=c(rep(" ",max))
  
  while(i<ncol(data)){
    Yea=c(as.character(percentile.ranking.list[[i]][,1]),rep(NA,max-length(percentile.ranking.list[[i]][,1])))
    Ratios=c(percentile.ranking.list[[i]][,2],rep(NA,max-length(percentile.ranking.list[[i]][,2])))
    
    nam <- paste("", names[i-1], sep = "")
    nam <- substring(nam,2,5)
    nam <- paste("Year", nam, sep = " ")
    
    nam.list=append(nam.list,nam)
    
    df=data.frame(Counter=l,Years=Yea,Sharpe.Ratios=Ratios,Spaces=spaces)  
    if(i==check){
      final.data=rbind(final.data,df[,c(2,3,4)])
    }else{
      final.data=cbind(final.data,df[c(2,3,4)])
    }
    i=i+1
    colnames(final.data)[ncol(final.data)-5]=nam
    colnames(final.data)[ncol(final.data)-3]=" "
  }
  return(final.data)
}


value=find.ranking.with.percentile(10)
write.csv(value,"FinalResult10.csv")

value=find.ranking.with.percentile(25)
write.csv(value,"FinalResult25.csv")

value=find.ranking.with.percentile(50)
write.csv(value,"FinalResult50.csv")



