#CommodityGDPGrowthRateVolumeIndex
setwd("C:/Users/ksath/CIND820/USB/CIND820/New folder/Commodeties Data/DataModified29March2021/Submitted2April2020")
getwd()
rm(list = ls())
library(factoextra)
#Reading CommodityGDPGrowthRateVolumeIndex

Commoditydata <- read.csv(file="CommodityGDPGrowthRateVolumeIndex.csv", header=TRUE, sep=",")#Analysed based on World GDP growth rate 
CommoditydataCountry<-na.omit(Commoditydata)
Commoditydata <- na.omit(Commoditydata)
#--------
summary(Commoditydata)
#class(Commoditydata)
#--------
#Data normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#chk=normalize(Commoditydata[,20])
Commoditydata_norm <-normalize(Commoditydata[,2:20])#Removing country, GDP and Rank column 
#------------
Commoditydata_lm <-Commoditydata_norm # Data for regression analysis
Commoditydata_pca <-Commoditydata_norm  #Data for PCA analysis
Commoditydata_x <-Commoditydata_norm[,1:19]# Data for PCA analysis, removing the dependent variable GDP 
Commoditydata_y <-Commoditydata_norm[,19]# GDP data
#Commoditydata_x <-Commoditydata_norm[,1:18]
#Commoditydata_y <-Commoditydata_norm[,18]
if (FALSE){
  Commoditydata_x <-Commoditydata_norm[,1:19]# Data for PCA analysis, removing the dependent variable GDP Millions (USD)
  Commoditydata_y <-Commoditydata_norm[,19]# GDP data
}
#-------------

# Splitting the GDPdata dataset (GDPdata_x) into training and test set.
train_index <- sample(1:nrow( Commoditydata_x), 0.7 * nrow( Commoditydata_x))
Commoditydata.train.set <- Commoditydata_x[train_index,-19]#removing the dependent variable GDP (USD Billions)
Commoditydata.test.set  <-  Commoditydata_x[-train_index,-19]# removing the dependent variable GDP (USD in Billions)
Commoditydata_y_train.set<-  Commoditydata_x[train_index,19]
Commoditydata_y_test.set  <-   Commoditydata_x[-train_index,19]

#Commoditydata.train.set <- Commoditydata_x[train_index,-18]
#Commoditydata.test.set  <-  Commoditydata_x[-train_index,-18]
#Commoditydata_y_train.set<-  Commoditydata_x[train_index,18]
#Commoditydata_y_test.set  <-   Commoditydata_x[-train_index,18]

CommoditydataCountrytrainset=CommoditydataCountry[train_index,]

#Apply normalization (scaling and centering) to the data.
#Calculate PCA with normalized data. 
CommoditydataTrainSet.pca.normdata <- prcomp(Commoditydata.train.set, scale = TRUE, center= TRUE)
CommoditydataTrainSet.pca.normdata$rotation      # eigen vector / rotation matrix / tranformation matrix
dim(CommoditydataTrainSet.pca.normdata$rotation)
head(CommoditydataTrainSet.pca.normdata$x)       # Transformed data
#plot(CommoditydataTrainSet.pca.normdata)
#pca 1 have the most variance
#To confirm it do summary (pca)
summary(CommoditydataTrainSet.pca.normdata)

# Biplot the first two PCs. 
#dim(GDPdataTrainSet.pca.normdata$x)
boxplot(CommoditydataTrainSet.pca.normdata$x, main='Norm Data Transformation')

#Visualize first two components of your PCA. 

biplot(CommoditydataTrainSet.pca.normdata, choices = 1:2, main='Norm Data')
biplot(CommoditydataTrainSet.pca.normdata, scale = 0)

#correlations of the PCs

cor(CommoditydataTrainSet.pca.normdata$x)

#-----------------
screeplot(CommoditydataTrainSet.pca.normdata, type = "l", npcs = 7, main = "Screeplot of the first 7 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
#PCs which have eigen values lesser than 1 are discarded because these PCs explain less
#than a single explanatory variable


cumpro <- cumsum(CommoditydataTrainSet.pca.normdata$sdev^2 / sum(CommoditydataTrainSet.pca.normdata$sdev^2))
plot(cumpro[0:10], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)
#-----------------
#Predict using PCA

PredictingData<- predict(CommoditydataTrainSet.pca.normdata, newdata = Commoditydata.test.set )

# we tend to look at the first few principal components in order to find interesting patterns in the data.
PredictingData<-PredictingData[,1:4]
#-----------------

#-----------------

fviz_pca_ind(CommoditydataTrainSet.pca.normdata,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Color individuals by groups
#Plots: quality and countries
fviz_pca_ind(CommoditydataTrainSet.pca.normdata, label="none")
fviz_pca_ind(CommoditydataTrainSet.pca.normdata, label="none", habillage=CommoditydataCountrytrainset$ï..Period)




#--------------------
#Variable correlation plots
# Plot of train data
p <- fviz_pca_ind(CommoditydataTrainSet.pca.normdata, repel = TRUE)
# Add predicted data
fviz_add(p, PredictingData, color ="blue")

fviz_cos2(CommoditydataTrainSet.pca.normdata, choice = "var", axes = 1:2)

#-----------------
#The top variables contributing to the principal components:
# Contributions of variables to PC1
fviz_contrib(CommoditydataTrainSet.pca.normdata, choice = "var", axes = 1, top = 20)
#Note:The red dashed line on the graph indicates the expected average contribution. 
#For a given component, the variables whose values greater than threshold (red line) are important contributers
# Contributions of variables to PC2
fviz_contrib(CommoditydataTrainSet.pca.normdata, choice = "var", axes = 2, top = 15)
#Comparing PC1 and PC2
#The total contribution to PC1 and PC2
fviz_contrib(CommoditydataTrainSet.pca.normdata, choice = "var", axes = 1:2, top = 10)
#The most important (or, contributing) variables are highlighted on the following correlation plot
fviz_pca_var(CommoditydataTrainSet.pca.normdata, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#----------------


#-----------------
#Investigated how the variables are related to one another
#plot(Commoditydata)
#plot(Commoditydata[,1:8])
plot(Commoditydata[,14:20])
#-----------------
#Regression analysis
#Regression helps show the relationship between  a response variable with respect to predictor varaiables
#Building a model for estimating GDP of a country based on the monetary values of all the goods available in that country
#GDPresults = lm (World.GDP.growth.rate ~ . , data = Commoditydata_lm ) 
#GDPresults = lm (Volume.index ~ . , data = Commoditydata_lm )
#GDPresults = lm (Total.medtech.growth.per.year.in.Percentage ~ . , data = Commoditydata_lm )
GDPresults = lm (Volume.growth.rates.of.merchandise.exports.and.imports ~ . , data = Commoditydata_lm )
summary(GDPresults)
summary(GDPresults)$coefficient
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
pcs <- as.data.frame(CommoditydataTrainSet.pca.normdata$x)
pcs.data <- cbind(Commoditydata_y_train.set, pcs)
pcs.data <-pcs.data [,1:2]
pcsmodel <- lm(Commoditydata_y_train.set ~ ., data = pcs.data)
summary(pcsmodel)
#----------------

#----------------
#Next analysis using only first principle component
#Pca 1 is used as it has the most variance
#Obtained pca 1 for your analyses with the following
#Built into a linear regression model using pca1
pca1=as.data.frame(CommoditydataTrainSet.pca.normdata$x[,1])
pca1.data <- cbind(Commoditydata_y_train.set, pca1)
pca1model <- lm(Commoditydata_y_train.set ~ ., data = pca1.data)
summary(pca1model)
#---------------

