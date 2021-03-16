#rm(list = ls())
#list.files()
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
#par(mar=rep(2,4))
#par(mar=c(1,1,1,1))
par(mfrow=c(3,3)) 

plot(t(Data[1,]), t(Data[4,]),  
     xlab="Year", ylab="African Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[13,]), main="World Economy (Continent wise)", 
     xlab="Year", ylab="North American Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[14,]), 
     xlab="Year", ylab="Latin American and Caribbean Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[18,]), 
     xlab="Year", ylab="Asian Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[24,]), 
     xlab="Year", ylab="European Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[30,]), 
     xlab="Year", ylab="Asia and Oceania Economy", type="o", col="blue")
plot(t(Data[1,]), t(Data[24,]), 
     xlab="Year", ylab="European Economy", type="o", col="blue")


#plot(t(Data[1,1:10]),t(Data[4,1:10]))
#plot(Data[1,],Data[4,])
#dev.off()