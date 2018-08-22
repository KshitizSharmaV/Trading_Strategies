library(quantmod)
stockData <- new.env()

# KFT, AIG, SBC, UK KODK DD-PB 
lookup.symb=c("AAPL","AXP","BA", "CAT","CSCO","CVX","KO","DWDP","XOM","GS","HD",
"IBM","INTC","JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX",
"V","VZ","WMT","DIS","WBA","GE","T","BAC","NKE","AA","BAC","C","MO","IP","GT",
"SHLD","HPQ")



getSymbols(lookup.symb, from="1997-03-12", env=stockData, src="yahoo")

v=getSymbols("HON", from="1997-03-12", src="yahoo")

lookup.symb=c("HON")
ReturnMatrix=NULL


for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  #ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  ReturnMatrix=cbind(ReturnMatrix,tmp[,4])
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}

rownames(ReturnMatrix)=tmp[,0]
ReturnMatrix=as.data.frame(ReturnMatrix)
write.csv(ReturnMatrix,"Returns2.csv")
