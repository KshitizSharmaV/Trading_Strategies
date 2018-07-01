# Quadruple Whitching 
# Quadruple witching refers to the third Friday of every March, June, September and December. 
# On these days, market index futures, market index options, stock options and stock futures 
# expire, usually resulting in increased volatility.

# Formatting Data

rm(list=ls())
# Read the data
data=as.data.frame(read.csv("Workbook2.csv"))
dates <- as.Date(as.character(data[,1]),"%d/%m/%y")
dates=format(dates, format="%d/%m/%Y")
data$Dates=dates

# Formatting Data


# Getting dates of 3rd Friday of every month

library("RcppBDT")
dates=c()
for(i in 1991:2017){
  dates=append(dates,format(getNthDayOfWeek(third, Fri, Mar, i), format="%d/%m/%Y"))
  dates=append(dates,format(getNthDayOfWeek(third, Fri, Jun, i), format="%d/%m/%Y"))
  dates=append(dates,format(getNthDayOfWeek(third, Fri, Sep, i), format="%d/%m/%Y"))
  dates=append(dates,format(getNthDayOfWeek(third, Fri, Dec, i), format="%d/%m/%Y"))
}
dates=dates[1:(length(dates)-2)]
dates.needed=data[data$Dates %in% dates,]

# Getting dates of 3rd Friday of every month



# Levene Test function
library(Rcmdr)

# store values in variables
value1=dates.needed$Daily.Returns
value2=data$Daily.Returns
# Combine them
y <- c(value1, value2)
  
# group 1's for them
group <- as.factor(c(rep(1, length(value1)), rep(2, length(value2))))

# perform test
leveneTest(y, group)$`Pr(>F)`[1]

# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value Pr(>F)
# group    1  2.5474 0.1105
# 6974          

# 0.1105245




