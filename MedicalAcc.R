graphics.off()
setwd('E:/CIND820/New folder/Medical accessories')
Data<- read.csv(file='WtoDataNN.csv', header = FALSE)
D<-Data[,2:7]
Dt=t(D)

#for (row in 1:nrow(Data[1:5.])) {
#  print(row)
  #print(Data[row,1])
#}



#for (row in 1:nrow(Data[1:900,])) {
  
#  if (Data[row,1] == 'Belize') {
#    print("Belize")
 # } else {
#    print("Not the Belize")
#  } 
  
#}

#for (row in 1:nrow(Data[1:900,])) {
  
#  if (Data[row,1] == 'Belize') {
#    #print("Belize")
#    newdf <- rbind(df, de)
#  } else {
#    print("Not the Belize")
#  } 
  
#}

#newdf <- rbind(df, de)

#################
#Creating empty data frame
#df <- data.frame(Doubles=double(),
#                 Ints=integer(),
#                 Factors=factor(),
#                 Logicals=logical(),
#                 Characters=character(),
#                 stringsAsFactors=FALSE)
df<-data.frame()
#################

  first=1
for (row in 1:((nrow(Data)/96))) {
  i=first
  last=96*row
  #print(row)
  #print(first)
  print(last)
  sect<-t(Data[i:last,])
  newdf <- rbind(df, sect)
  first=last+1
}
