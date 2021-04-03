setwd("C:/Users/ksath/CIND820/USB/CIND820/New folder/Commodeties Data/DataModified29March2021/Submitted2April2020")
getwd()
rm(list = ls())
library(factoextra)

Merchanddata <- read.csv(file="us_trademerchbalance_csvnTranspose.csv", header=TRUE, sep=",")#Merchandise data (Trade balance, annual)
Merchanddata <- na.omit(Merchanddata)
MerchanddataCountry<-Merchanddata#used to grouping data of similar contributers in the individual factor map

#--------
#is.na(MedAccdata)
#check=sum(is.na(Merchand))
str(Merchanddata)
summary(Merchanddata)
#class(Merchanddata)
#--------
#Data normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# load the package
#library(varhandle)
#chk=normalize(MedAccdata[,170])#For random check
Merchanddata_norm <-normalize(Merchanddata[,3:195])#Removing Year column 

#------------
Merchanddata_lm <-Merchanddata_norm # Data for regression analysis
Merchanddata_pca <-Merchanddata_norm  #Data for PCA analysis
#-------------

# Splitting the Merchandisedataset (Merchanddata_pca) into training and test set.
train_index <- sample(1:nrow(Merchanddata_pca), 0.7 * nrow(Merchanddata_pca))
Merchanddata.train.set <-Merchanddata_pca[train_index,-1]#Train set minus the dependent variable, World 
Merchanddata.test.set  <- Merchanddata_pca[-train_index,-1]# Test set contains 30% of data and -1 removes the dependent variable World 
Merchanddata_y_train.set<- Merchanddata_pca[train_index,1]
Merchanddata_y_test.set  <-  Merchanddata_pca[-train_index,1]
MerchanddataCountrytrainset=MerchanddataCountry[train_index,]

#Apply normalization (scaling and centering) to the data.
#Calculate PCA with normalized data. 
MerchanddataTrainSet.pca.normdata <- prcomp(Merchanddata.train.set, scale = TRUE, center= TRUE)
MerchanddataTrainSet.pca.normdata$rotation      # eigen vector / rotation matrix / tranformation matrix
head(MerchanddataTrainSet.pca.normdata$x)       # Transformed data
plot(MerchanddataTrainSet.pca.normdata)
#pca 1 have the most variance
#To confirm it do summary (pca)
summary(MerchanddataTrainSet.pca.normdata)

# We obtained 50 principle component 
#The first 18 components account for >95% of total variance in the data

# Biplot the first two PCs. 
#dim(GDPdataTrainSet.pca.normdata$x)
boxplot(MerchanddataTrainSet.pca.normdata$x, main='Norm Data Transformation')

#Visualize first two components of your PCA. 

biplot(MerchanddataTrainSet.pca.normdata, choices = 1:2, main='Norm Data')

#correlations of the PCs

cor(MerchanddataTrainSet.pca.normdata$x)

#-----------------
screeplot(MerchanddataTrainSet.pca.normdata, type = "l", npcs = 26, main = "Screeplot of the first 26 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)



cumpro <- cumsum(MerchanddataTrainSet.pca.normdata$sdev^2 / sum(MerchanddataTrainSet.pca.normdata$sdev^2))
plot(cumpro[0:30], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 13, col="blue", lty=5)
abline(h = 0.95, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)
#Note: The first 18 components has an Eigenvalue >1 and explains almost 90% of variance 
#We can effectively reduce dimensionality from 160 to 18 if we consider 90 % variance
#-----------------



#-----------------
#Predict using PCA

PredictingData<- predict(MerchanddataTrainSet.pca.normdata, newdata = Merchanddata.test.set )
#PredictingData<-PredictingData[,1:19]
#-----------------

#-----------------
#Draw the graph of individuals/variables from the output of Principal Component Analysis (PCA).
# cos2 = the quality of the individuals on the factor map
fviz_pca_ind(MerchanddataTrainSet.pca.normdata,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Color individuals by groups
#Plots: quality and countries
fviz_pca_ind(MerchanddataTrainSet.pca.normdata, label="none")
#fviz_pca_ind(MerchanddataTrainSet.pca.normdata, label="none", habillage=MerchanddataCountrytrainset$YEAR)
fviz_pca_ind(MerchanddataTrainSet.pca.normdata, label="none", habillage=MerchanddataCountrytrainset[,2])

# Select and visualize individuals with cos2 > 0.96
fviz_pca_ind(MerchanddataTrainSet.pca.normdata, select.ind = list(cos2 = 0.80))

# Select the top 20 contributing individuals
fviz_pca_ind(MerchanddataTrainSet.pca.normdata, select.ind = list(contrib = 10))

#--------------------
#Variable correlation plots
# Plot of train data
p <- fviz_pca_ind(MerchanddataTrainSet.pca.normdata, repel = TRUE)
# Add predicted data
fviz_add(p, PredictingData, color ="blue")
fviz_cos2(MerchanddataTrainSet.pca.normdata, choice = "var", axes = 1:2)
#-----------------
#The top variables contributing to the principal components:
# Contributions of variables to PC1
fviz_contrib(MerchanddataTrainSet.pca.normdata, choice = "var", axes = 1, top = 30)
#Note:The red dashed line on the graph indicates the expected average contribution. 
#For a given component, the variables whose values greater than threshold (red line) are important contributers
# Contributions of variables to PC2
fviz_contrib(MerchanddataTrainSet.pca.normdata, choice = "var", axes = 2, top = 25)
#Comparing PC1 and PC2
#The total contribution to PC1 and PC2
fviz_contrib(MerchanddataTrainSet.pca.normdata, choice = "var", axes = 1:2, top = 25)
#The most important (or, contributing) variables are highlighted on the following correlation plot
fviz_pca_var(MerchanddataTrainSet.pca.normdata, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#----------------


#-----------------
#Investigated how the variables are related to one another
plot(Merchanddata[,1:6])
#-----------------
#Regression analysis
GDPresults = lm (World ~ . , data = Merchanddata_lm ) 
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
#PCA is due to various explanatory variables 
#Standard linear models potentially more powerful when using PCA
#combining both the explanatory variables - PCs, and explained variable - y
pcs <- as.data.frame(MerchanddataTrainSet.pca.normdata$x)
pcs.data <- cbind(Merchanddata_y_train.set, pcs)
pcs.data<-pcs.data[,1:18]
pcsmodel <- lm(Merchanddata_y_train.set ~ ., data = pcs.data)
summary(pcsmodel)
#----------------

#----------------
#Next analysis using only first principle component
#Pca 1 is used as it has the most variance
#Obtained pca 1 for your analyses with the following
#Built into a linear regression model using pca1
#pca1=GDPdataTrainSet.pca.normdata$x[,1]
pca1=as.data.frame(MerchanddataTrainSet.pca.normdata$x[,1])
pca1.data <- cbind(Merchanddata_y_train.set, pca1)
pca1model <- lm(Merchanddata_y_train.set ~ ., data = pca1.data)
summary(pca1model)
#---------------

