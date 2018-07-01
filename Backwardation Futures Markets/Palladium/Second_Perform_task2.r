rm(list=ls())

# Reade from the file we created in previous step
data=read.csv("backwards.csv")

# Add the File code symbol
symbol = "F.US."

df = data.frame(Date=integer(),Lead.Contract=character(),CL0=character(),CL1=character(),CL2=character(),CL3=character(),CL0.value=double(),CL1.value=double(),CL2.value=double(),CL3.value=double())

# Check for errors
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

each.row = data[1,] 
each.row

previous="F.US.PAH94.b01"
listofdfs <- list()

date=each.row[2]

CL0.t = capture.output(cat(c(symbol,as.character(each.row$CL0)), sep = ""))
CL1.t = capture.output(cat(c(symbol,as.character(each.row$CL1)), sep = ""))
CL2.t = capture.output(cat(c(symbol,as.character(each.row$CL2)), sep = ""))
CL3.t = capture.output(cat(c(symbol,as.character(each.row$CL3)), sep = ""))

listofdfs[[1]]=read.table(paste("./b01Files/",CL0.t,sep=""),header=FALSE)
listofdfs[[2]]=read.table(paste("./b01Files/",CL1.t,sep=""),header=FALSE)
listofdfs[[3]]=read.table(paste("./b01Files/",CL2.t,sep=""),header=FALSE)
listofdfs[[4]]=read.table(paste("./b01Files/",CL3.t,sep=""),header=FALSE)


for(i in 1:nrow(data)){
  if(i!=1){
    each.row = data[i,] 
    date=each.row[2]
    
    CL0.t = capture.output(cat(c(symbol,as.character(each.row$CL0)), sep = ""))
    CL1.t = capture.output(cat(c(symbol,as.character(each.row$CL1)), sep = ""))
    CL2.t = capture.output(cat(c(symbol,as.character(each.row$CL2)), sep = ""))
    CL3.t = capture.output(cat(c(symbol,as.character(each.row$CL3)), sep = ""))
    CL4.t = capture.output(cat(c(symbol,as.character(each.row$CL4)), sep = ""))
    }
  
  
  if(!identical(CL0.t,previous)){
    listofdfs[[1]]=listofdfs[[2]]
    listofdfs[[2]]=listofdfs[[3]]
    listofdfs[[3]]=listofdfs[[4]]
    listofdfs[[4]]=read.table(paste("./b01Files/",CL4.t,sep=""),header=FALSE)
    
    
    if(paste(listofdfs[[1]]$V1[1],".b01",sep="") != CL0.t){
      listofdfs[[1]]=read.table(paste("./b01Files/",CL0.t,sep=""),header=FALSE)
    }
    if(paste(listofdfs[[2]]$V1[1],".b01",sep="") != CL1.t){
      listofdfs[[2]]=read.table(paste("./b01Files/",CL1.t,sep=""),header=FALSE)
    }
    if(paste(listofdfs[[3]]$V1[1],".b01",sep="") != CL2.t){
      listofdfs[[3]]=read.table(paste("./b01Files/",CL2.t,sep=""),header=FALSE)
    }
    if(paste(listofdfs[[4]]$V1[1],".b01",sep="") != CL3.t){
      listofdfs[[4]]=read.table(paste("./b01Files/",CL3.t,sep=""),header=FALSE)
    }
  }
  
  
  listofdfs.temp <- list()
  for(j in 1:4){
    listofdfs.temp[[j]]=listofdfs[[j]][listofdfs[[j]]$V2 %in% date,]
    listofdfs.temp[[j]] = listofdfs.temp[[j]][listofdfs.temp[[j]]$V3<=1200,]  
  }
  
  
  CL0.values = listofdfs.temp[[1]]$V7
  CL0.values = CL0.values[length(CL0.values)]
  
  
  CL1.values = listofdfs.temp[[2]]$V7
  CL1.values = CL1.values[length(CL1.values)]
  
  
  CL2.values = listofdfs.temp[[3]]$V7
  CL2.values = CL2.values[length(CL2.values)]
  
  
  CL3.values = listofdfs.temp[[4]]$V7
  CL3.values = CL3.values[length(CL3.values)]
  
  
  
  
  
  if(is.integer0(as.integer(CL0.values))){CL0.values=NA}else{CL0.values=CL0.values}
  if(is.integer0(as.integer(CL1.values))){CL1.values=NA}else{CL1.values=CL1.values}
  if(is.integer0(as.integer(CL2.values))){CL2.values=NA}else{CL2.values=CL2.values}
  if(is.integer0(as.integer(CL3.values))){CL3.values=NA}else{CL3.values=CL3.values}
  
  
  df.temp = data.frame(Date=date,Lead.Contract=as.character(each.row$CL0),CL0=as.character(each.row$CL0),CL1=as.character(each.row$CL1),CL2=as.character(each.row$CL2),CL3=as.character(each.row$CL3),CL0.value=CL0.values,CL1.value=CL1.values,CL2.value=CL2.values,CL3.value=CL3.values)
  df=rbind(df,df.temp)
  previous=CL0.t
}

write.csv(df,"Backwardation_for_Palladium.csv")

