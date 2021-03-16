setwd("E:/CIND820/New folder")
getwd()
graphics.off()

GDPdata <- read.csv(file="GDPCountrywise.csv", header=TRUE, sep=",")
#GDPdata <- read.csv(file="WtoDataNN.csv",header=TRUE, sep=",")
#GDPdata<-t(GDPdata)
#-------
#Splitting the data*
# Let's split the GDPdata dataset into training and test set.

train_index <- sample(1:nrow(GDPdata), 0.7 * nrow(GDPdata))
train.set <-GDPdata[train_index,]
test.set  <- GDPdata[-train_index,]

# Now, we remove the GDP column from our training and test datasets.

train.set_new <- train.set[-1]
test.set_new <- test.set[-1]

# Now, we store the labels from our training and test datasets.

stars_train_labels <- train.set$GDP(USD)(In_Millions)
stars_test_labels <- test.set$GDP(USD)(In_Millions)
#-------

# Apply PCA on the variables of GDPdata
data(GDPdata)
#GDPdata_x <-GDPdata[,3:57]
GDPdata_x <-GDPdata[,3:57]
GDPdata_x<-cbind(ChkNumeric, GDPdata_x)
#is.na(GDPdata)
#check=sum(is.na(GDPdata))
#class(GDPdata_x$GDPinUSDInMillions)

#Calculating PCAs from raw data
GDPdata_x <- na.omit(GDPdata_x)
GDPdata.pca.rawdata <- prcomp(GDPdata_x, scale = FALSE, center= FALSE)
GDPdata.pca.rawdata

#Transform the dataset using calculated PCAs.

head(as.matrix(GDPdata_x)%*%GDPdata.pca.rawdata$rotation)

#Apply normalization (scaling and centering) to the data.
#Recalculate PCA with normalized data. 
GDPdata.pca.normdata <- prcomp(GDPdata_x, scale = TRUE, center= TRUE)
GDPdata.pca.normdata$rotation      # eigen vector / rotation matrix / tranformation matrix
head(GDPdata.pca.normdata$x)       # Transformed data
summary(GDPdata.pca.normdata)

# Biplot the first two PCs. 

boxplot(GDPdata.pca.normdata$x, main='Norm Data Transformation')

#Visualize first two components of your PCA. 

biplot(GDPdata.pca.normdata, choices = 1:2, main='Norm Data')

#correlations of the PCs

cor(GDPdata.pca.normdata$x)
