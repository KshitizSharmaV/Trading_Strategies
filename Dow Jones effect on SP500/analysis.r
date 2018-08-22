# The next step is to identify those dates on which all 30 stocks in the DOW 30 are down, 
# and identify those dates on which all 30 of the stocks in the DOW 30 are up . 
# As a  follow-up, can you please send me these dates?


# The hypothesis is that when all 30 stocks are down, this is potentially bullish for the stock market.  
# Similarly, when all 30 stocks are up, this is potentially bearish for the stock market. 


rm(list=ls())
data1= read.csv("Dow.csv")
data2= read.csv("final_data.csv")
data2=as.data.frame(data2)

dates=as.character(data2$Row.names)

dates = c("12/03/97","01/11/99","08/04/04","21/11/05","19/02/08","22/09/08","08/06/09","24/09/12","23/09/13","19/03/15","01/09/17","26/06/18","05/08/18")
indexes=c(1,667,1780,2189,2751,2901,3079,3911,4160,4534,5154,5358)


for(i in 11:1){
  val=data1[,11]
  checkfordates=c()
  tempdb=c()
  for(j in val){
    print(head(data2[j],5))
    #tempdb=cbind(tempdb,data2[[j]])
  }
  colnames(tempdb)=val
}
