#rm(list = ls())
#list.files()
graphics.off()
#dev.off()
setwd("E:/CIND820/New folder/Mechandise export")
Data<- read.csv(file='us_trademerchbalance_csvn.csv', header = FALSE)
Data<-(Data[,2:73])
dim(Data)
Data <- na.omit(Data)
dim(Data)
#head(Data,2:2)
#head(Data, 3)
#tail(Data)
#summary(Data)
#str(Data)
nrow(Data)
ncol(Data)
dim(Data)

#par(mfrow=c(3,3)) 

plot(t(Data[1,]), t(Data[19,]),  
     xlab="Year", ylab="Asian Economy", type="o", col="blue")
par(new=TRUE)
lines(t(Data[1,]), t(Data[20,]), col="green")
par(new=TRUE)
lines(t(Data[1,]), t(Data[21,]), col="yellow")
par(new=TRUE)
lines(t(Data[1,]), t(Data[22,]), col="red")
par(new=TRUE)
lines(t(Data[1,]), t(Data[23,]), col="magenta")

# Add a legend
legend("topleft", legend=c("Central", "Eastern","Middle", "South-eastern","Western" ),
       col=c("blue","green","yellow","red","magenta"), lty=1:2, cex=0.8)



# Add a legend
#legend("topleft", legend=c("Line 1", "Line 2"),
#       col=c("red", "blue"), lty=1:2, cex=0.8)

#legend("topleft", 
#       legend = c("Group 1", "Group 2"),
#       col = c("red", "blue"))


#plot(t(Data[1,1:10]),t(Data[4,1:10]))
#plot(Data[1,],Data[4,])
#dev.off()