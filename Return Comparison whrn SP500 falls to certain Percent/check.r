rm(list=ls())
data=read.csv("sd2.csv")

# Plot for SD on 50, 100, 200 D MA returns

plot(data$X50.SD,xlab="Days",ylab="SD on 50,100,200Returns ",type="l",col="blue")
par(new=T)
plot(data$X100.SD,col="red",axes=F,ylab="",xlab="",type="l",lwd=1)
par(new=T)
plot(data$X200.SD,type="l",ylab="",xlab="",col="green",axes=F,lwd=1)
legend("topleft",
       legend=c("50 D MA", "100 D MA","200 D MA"),
       lty=c(1,1,1), col=c("blue", "red","green"),cex=0.55)



# Plot for differneces SD on 50, 100, 200 D MA returns and 1,2,3.. 47 days returns

plot(data$X50.Change.SD,xlab="Days",ylab="Change in SD on 50,100,200Returns ",type="l",col="blue")
par(new=T)
plot(data$X100.Change.SD,col="red",axes=F,ylab="",xlab="",type="l",lwd=1)
par(new=T)
plot(data$X200.Change.SD,type="l",ylab="",xlab="",col="green",axes=F,lwd=1)
legend("topleft",
       legend=c("50 D MA", "100 D MA","200 D MA"),
       lty=c(1,1,1), col=c("blue", "red","green"),cex=0.55)
