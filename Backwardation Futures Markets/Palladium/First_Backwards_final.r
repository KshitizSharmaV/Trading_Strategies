rm(list=ls())

# Symbol Being Used in the files
symbol = "PA"

# Months
months.1=c('H','M','U','Z')
months=c('H','M','U','Z','H','M','U','Z')
# Reading the file given
data = read.csv("PALLADIUM_prices_ALLMONTHS.csv")
#*# Change here for start row
data = data[4:nrow(data),]

# Get the lead contracts
lead.contracts = data$X.1
lead.contracts=gsub("LC", "GLE", lead.contracts)
lead.contracts
#Get the dates from contracts
lead.dates = data$X

# Data.Frame Building 
df = data.frame(Date=integer(),Lead.Contract=character(),CL0=character(),CL1=character(),CL2=character(),CL3=character(),CL4=character(),CL5=character())

for(i in 1:length(lead.contracts)){
  #print(lead.contracts[i])
  #print("****")
  lead.month = substr(lead.contracts[i],3,3)
  month = as.integer(substr(lead.contracts[i],4,5))
  for(j in 1:length(months.1)){
    if((pmatch(lead.month,months[j],nomatch = 0)==1) & (pmatch(lead.month,months.1[1],nomatch=0)==1 | pmatch(lead.month,months.1[2],nomatch=0)==1 | pmatch(lead.month,months.1[3],nomatch=0)==1 | pmatch(lead.month,months.1[4],nomatch=0)==1 | pmatch(lead.month,months.1[5],nomatch=0)==1)){
      #print("2**2")
      old.month=as.character(month)
      if(month>=90 & month<99){
        month = month+1
        month=as.character(month)
      }else if(month==99){
        month=0
        month=as.character(paste(0,month,sep=""))
      }else if(month<9){
        old.month=as.character(paste(0,month,sep=""))
        month = month+1
        month=as.character(paste(0,month,sep=""))
      }else if(month==9){
        old.month=as.character(paste(0,month,sep=""))
        month = month+1
        month=as.character(month)
      }else if(month>=10){
        old.month=as.character(month) 
        month=month+1
        month=as.character(month)
      }
      
      CL0.t = capture.output(cat(c(symbol,months[j],month,".b01"), sep = ""))
      CL1.t = capture.output(cat(c(symbol,months[j+1],month,".b01"), sep = ""))
      CL2.t = capture.output(cat(c(symbol,months[j+2],month,".b01"), sep = ""))
      CL3.t = capture.output(cat(c(symbol,months[j+3],month,".b01"), sep = ""))
      CL4.t = capture.output(cat(c(symbol,months[j+4],month,".b01"), sep = ""))
      CL5.t = capture.output(cat(c(symbol,months[j+5],month,".b01"), sep = ""))
      
      if(pmatch(lead.month,months.1[1],nomatch = 0)==1){
        CL0.t = capture.output(cat(c(symbol,months[j],old.month,".b01"), sep = ""))
        CL1.t = capture.output(cat(c(symbol,months[j+1],old.month,".b01"), sep = ""))
        CL2.t = capture.output(cat(c(symbol,months[j+2],old.month,".b01"), sep = ""))
        CL3.t = capture.output(cat(c(symbol,months[j+3],old.month,".b01"), sep = ""))
      }else if(pmatch(lead.month,months.1[2],nomatch = 0)==1){
        CL0.t = capture.output(cat(c(symbol,months[j],old.month,".b01"), sep = ""))
        CL1.t = capture.output(cat(c(symbol,months[j+1],old.month,".b01"), sep = ""))
        CL2.t = capture.output(cat(c(symbol,months[j+2],old.month,".b01"), sep = ""))
      }else if(pmatch(lead.month,months.1[3],nomatch = 0)==1){
        CL0.t = capture.output(cat(c(symbol,months[j],old.month,".b01"), sep = ""))
        CL1.t = capture.output(cat(c(symbol,months[j+1],old.month,".b01"), sep = ""))
      }else if(pmatch(lead.month,months.1[4],nomatch = 0)==1){
        CL0.t = capture.output(cat(c(symbol,months[j],old.month,".b01"), sep = ""))
      }
      df.temp=data.frame(Date=lead.dates[i],Lead.Contract=CL0.t,CL0=CL0.t,CL1=CL1.t,CL2=CL2.t,CL3=CL3.t,CL4=CL4.t,CL5=CL5.t)
      df=rbind(df,df.temp)
    }
  }
}
df=df[1:7]
write.csv(df,"Backwards.csv")
