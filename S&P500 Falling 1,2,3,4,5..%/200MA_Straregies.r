rm(list=ls())
# Load the data
data=read.csv("Workbook2.csv")

# Finding MAs for 200, 100 & 50 and copy those values into a Workbook 2 csv file
daily.prices = data$Close
# Function to find MA
function.for.MA.calculation = function(ma.length){
  temp=c()
  for(i in 1:(length(daily.prices)-ma.length)){
    temp[i]=mean(daily.prices[i:(i+ma.length-1)])
  }  
  return(temp)
}
# Call above function for calculating MA
MA.200 = function.for.MA.calculation(200)
MA.100 = function.for.MA.calculation(100)
MA.50 = function.for.MA.calculation(50)
MA.data=NULL
MA.data=cbind(MA.data,MA.50)
MA.data=cbind(MA.data,MA.100)
MA.data=cbind(MA.data,MA.200)

# Values have been written you can now copy values to workbook 2
# Values have been alreay copied so we are going to just import it down 
# For Futher calculations
write.csv(MA.data,"MA_Values.csv")


# Reading MA CSV value
# Made my own Workbook to have a refined data
MA.50=as.numeric(data$X50.D.MA.Price)
MA.100=as.numeric(data$X100.D.MA.Price)

dates <- as.Date(as.character(data[,1]),"%d/%m/%Y")
days.returns = as.numeric(data[,2])
daily.prices=data$Close

MA.50.returns=c()
MA.100.returns=c()
MA.200.returns = as.numeric(data[,3])

# calculate returns
for(i in 101:length(days.returns)){
  MA.50.returns[i]=(daily.prices[i]/MA.50[i]-1)
  MA.100.returns[i]=(daily.prices[i]/MA.100[i]-1)
}

length(MA.200.returns)
length(MA.100.returns)
length(MA.50.returns)

# Storing data in variables 
solve_for_assignment = function(MA.returns.data){
  # A Data Frame to store the corresponding returns and 1, 0 sequence 
  complete.data=NULL
  complete.data=cbind(complete.data,as.character(dates[101:length(dates)]))
  complete.data=cbind(complete.data,days.returns[101:length(days.returns)])
  complete.data=cbind(complete.data,MA.returns.data)
  complete.data
  
  # A for loop running from 0 to 10 to store all the returns from 0 to 10 % scale
  for(values_temp in 0:10){
    # temp variable to store each 1 0 sequence
    temp=c()
    # new.days.returns to store returns for 0 to 10  
    new.days.returns=days.returns[101:length(days.returns)]
    # t is a check variable to control i incrementation
    t=0
    # 101 Since all values are available only after 101, our 200 MA is complete
    i=101
    while(i<=length(dates)){
      t=0
      # This IF is a condition to check if the support barrier is crossed
      if( MA.returns.data[i] < -(values_temp/100)){
        temp[i]=1
        temp[i+1]=1
        for(j in (2+i):length(dates)){
          if(MA.returns.data[j] < (values_temp/100)){
            temp[j]=0
            new.days.returns[j]=0
          }else{
            temp[j]=0
            new.days.returns[j]=0
            temp[j+1]=1
            i=j+2
            t=1
            # Break statement is issued because when price reclimbs and cross +1,2,3,4..10% level
            # We want have to get long again
            break
          }
        }
      }else{
        temp[i]=1
      }
      if(t==0){
        i=i+1
      }
      else{
        i=j+2
      }
    }
    complete.data=cbind(complete.data,new.days.returns)
    complete.data=cbind(complete.data,temp)
  }
  complete.data=tail(complete.data,-100)
  colnames(complete.data) = c("dates","days.returns","MA.200.Returns","Days.Returns.0","temp.0","Days.Returns.1","temp.1","Days.Returns.2","temp.2","Days.Returns.3","temp.3","Days.Returns.4","temp.4","Days.Returns.5","temp.5","Days.Returns.6","temp.6","Days.Returns.7","temp.7","Days.Returns.8","temp.8","Days.Returns.9","temp.9","Days.Returns.10","temp.10")
  write.csv(complete.data,"Return_table_for_differnet_barriers.csv")

  
  i=4
  t=1
  temp=c()
  while(i<=ncol(complete.data)){
    temp[t]=sum(as.integer(complete.data[,i]))
    plot(complete.data[,i],type="l")
    i=i+2
    t=t+1
  }
}

solve_for_assignment(MA.200.returns)
solve_for_assignment(MA.100.returns)
solve_for_assignment(MA.50.returns)


plot(c(0:10),temp,type="l",xlab="% barrier on either side",ylab="Daily Return %")



ma.price.daily = data$X200.D.MA.Price
plot(daily.prices,type="l",col="blue", ylab="S&P 500 Prices")
par(new=T)
plot(ma.price.daily,type="l",lty=2,col="red",axes=F,ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, '200 MACD Lline')
