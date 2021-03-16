#rm(list = ls())
#list.files()
graphics.off()
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

plot(t(Data[1,]), t(Data[13,]),  
     xlab="Year", ylab="American Economy", type="o", col="blue")
par(new=TRUE)
lines(t(Data[1,]), t(Data[15,]), col="green")
par(new=TRUE)
lines(t(Data[1,]), t(Data[16,]), col="magenta")
par(new=TRUE)
lines(t(Data[1,]), t(Data[17,]), col="red")


# Add a legend
legend("topleft", legend=c("Northern", "Caribbean","Central", "South"),
       col=c("blue","green","magenta","red"), lty=1:2, cex=0.8)


# Add a legend
#legend("topleft", legend=c("Line 1", "Line 2"),
#       col=c("red", "blue"), lty=1:2, cex=0.8)

#legend("topleft", 
#       legend = c("Group 1", "Group 2"),
#       col = c("red", "blue"))


#plot(t(Data[1,1:10]),t(Data[4,1:10]))
#plot(Data[1,],Data[4,])
#dev.off()