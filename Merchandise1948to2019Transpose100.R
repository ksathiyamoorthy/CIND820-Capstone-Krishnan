setwd("C:/Users/ksath/CIND820/USB/CIND820/New folder/Commodeties Data/DataModified29March2021/Submitted2April2020")
getwd()
rm(list = ls())
library(factoextra)
library(caret)
library(corrplot)
library(plyr)

MedAccdata <- read.csv(file="ustrademerchbalanceTranspose100.csv", header=TRUE, sep=",")

MedAccdataCountry<-MedAccdata
MedAccdata <- na.omit(MedAccdata)
##########################################
# Identifying numeric variables
numericData <- MedAccdata[sapply(MedAccdata, is.numeric)]
# Calculate correlation matrix
descrCor <- cor(numericData)
# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)
# print indexes of highly correlated attributes
print(highlyCorrelated)
# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]
# Print highly correlated attributes
highlyCorCol
# Remove highly correlated variables and create a new dataset
dat3 <- MedAccdata[, -which(colnames(MedAccdata) %in% highlyCorCol)]
dim(dat3)
#pca1=as.data.frame(CommoditydataTrainSet.pca.normdata$x[,1])
Year<-MedAccdata[,1]
World.GDP.growth.rate<-MedAccdata[,2]
dat4 <- cbind(Year, dat3, World.GDP.growth.rate)
################################################
#--------
str(dat4)
summary(dat4)
#class(GDPdata_x$GDPinUSDInMillions)
#--------
#Data normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# load the package
#library(varhandle)
MedAccdata_norm <-normalize(dat4[,2:7])#Removing year 

#------------
MedAccdata_lm <-MedAccdata_norm # Data for regression analysis
MedAccdata_pca <-MedAccdata_norm  #Data for PCA analysis
MedAccdata_x <-MedAccdata_norm
#MedAccdata_x <-MedAccdata_norm[,2:193]# Data for PCA analysis, removing the dependent variable GDP Millions (USD)
MedAccdata_y <-MedAccdata_norm[,1]# GDP data
#-------------

# Splitting the GDPdata dataset (GDPdata_x) into training and test set.
train_index <- sample(1:nrow(MedAccdata_x), 0.7 * nrow(MedAccdata_x))
MedAccdata.train.set <-MedAccdata_x[train_index,-6]#removing the dependent variable GDP (USD Billions)
MedAccdata.test.set  <- MedAccdata_x[-train_index,-6]# removing the dependent variable GDP (USD in Billions)
MedAccdata_y_train.set<- MedAccdata_x[train_index,6]
MedAccdata_y_test.set  <-  MedAccdata_x[-train_index,6]
MedAccdataCountrytrainset=MedAccdataCountry[train_index,]

#Apply normalization (scaling and centering) to the data.
#Calculate PCA with normalized data. 
MedAccdataTrainSet.pca.normdata <- prcomp(MedAccdata.train.set, scale = TRUE, center= TRUE)
MedAccdataTrainSet.pca.normdata$rotation      # eigen vector / rotation matrix / tranformation matrix
head(MedAccdataTrainSet.pca.normdata$x)       # Transformed data
plot(MedAccdataTrainSet.pca.normdata)
#pca 1 have the most variance
#To confirm it do summary (pca)
summary(MedAccdataTrainSet.pca.normdata)

# We obtained 55 principle component 
#The first 19 components account for >95% of total variance in the data

# Biplot the first two PCs. 
#dim(GDPdataTrainSet.pca.normdata$x)
boxplot(MedAccdataTrainSet.pca.normdata$x, main='Norm Data Transformation')

#Visualize first two components of your PCA. 

biplot(MedAccdataTrainSet.pca.normdata, choices = 1:2, main='Norm Data')

#correlations of the PCs

cor(MedAccdataTrainSet.pca.normdata$x)

#-----------------
screeplot(MedAccdataTrainSet.pca.normdata, type = "l", npcs = 15, main = "Screeplot of the first 26 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)



cumpro <- cumsum(MedAccdataTrainSet.pca.normdata$sdev^2 / sum(MedAccdataTrainSet.pca.normdata$sdev^2))
plot(cumpro[0:30], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 26, col="blue", lty=5)
abline(h = 0.8, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)
#-----------------
#Predict using PCA

PredictingData<- predict(MedAccdataTrainSet.pca.normdata, newdata = MedAccdata.test.set )

#-----------------

fviz_pca_ind(MedAccdataTrainSet.pca.normdata,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Color individuals by groups
#Plots: quality and contries
fviz_pca_ind(MedAccdataTrainSet.pca.normdata, label="none")
fviz_pca_ind(MedAccdataTrainSet.pca.normdata, label="none", habillage=GDPdataCountrytrainset$Country)




#--------------------
#Variable correlation plots
# Plot of train data
p <- fviz_pca_ind(MedAccdataTrainSet.pca.normdata, repel = TRUE)
# Add predicted data
fviz_add(p, PredictingData, color ="blue")
fviz_cos2(MedAccdataTrainSet.pca.normdata, choice = "var", axes = 1:2)
fviz_contrib(MedAccdataTrainSet.pca.normdata, choice = "var", axes = 1, top = 30)
fviz_contrib(MedAccdataTrainSet.pca.normdata, choice = "var", axes = 2, top = 25)
#Comparing PC1 and PC2
fviz_contrib(MedAccdataTrainSet.pca.normdata, choice = "var", axes = 1:2, top = 25)
#The most important (or, contributing) variables are highlighted on the following correlation plot
fviz_pca_var(MedAccdataTrainSet.pca.normdata, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#----------------


#-----------------
#Investigated how the variables are related to one another
plot(MedAccdata[,1:15])
#-----------------
#Regression analysis
#Regression helps show the relationship between  a response variable with respect to predictor varaiables
GDPresults = lm (World.GDP.growth.rate ~ . , data = MedAccdata_lm )
GDPVsYear = lm (Year ~  -World.GDP.growth.rate, data = dat4 ) 
summary(GDPresults)
summary(GDPVsYear)
ggplot(dat4, aes(World.GDP.growth.rate, Year)) +
  geom_point() +
  stat_smooth(method = lm)
ggplot(dat4, aes( Year, -World.GDP.growth.rate)) +
  geom_point() +
  stat_smooth(method = lm)
summary(GDPresults)$coefficient
summary(GDPVsYear)$coefficient
#----------------
# Calculate Relative Importance for Each Predictor
#library(relaimpo)
if (FALSE){
  calc.relimp(GDPdata_x,type=c("lmg","last","first","pratt"),
              rela=TRUE)
  
  # Bootstrap Measures of Relative Importance (1000 samples)
  boot <- boot.relimp(GDPdata_x, b = 1000, type = c("lmg",
                                                    "last", "first", "pratt"), rank = TRUE,
                      diff = TRUE, rela = TRUE)
  #booteval.relimp(boot) # print result
  plot(booteval.relimp(boot,sort=TRUE)) # plot result
}
#----------------
#Regression analysis using PCA
#PCA is due to various explanatory variables 
#Standard linear models potentially more powerful when using PCA
#combining both the explanatory variables - PCs, and explained variable - y
pcs <- as.data.frame(MedAccdataTrainSet.pca.normdata$x)
pcs.data <- cbind(MedAccdata_y_train.set, pcs)
pcs.data<-pcs.data[,1:30]
pcsmodel <- lm(MedAccdata_y_train.set ~ ., data = pcs.data)
summary(pcsmodel)
#----------------

#----------------
#Next analysis using only first principle component
#Pca 1 is used as it has the most variance
#Obtained pca 1 for your analyses with the following
#Built into a linear regression model using pca1
#pca1=GDPdataTrainSet.pca.normdata$x[,1]
pca1=as.data.frame(MedAccdataTrainSet.pca.normdata$x[,1])
pca1.data <- cbind(MedAccdata_y_train.set, pca1)
pca1model <- lm(MedAccdata_y_train.set ~ ., data = pca1.data)
summary(pca1model)
#---------------

