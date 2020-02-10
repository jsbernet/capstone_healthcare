###################################################
# K-Means Clustering Algorithm for Cardio_Train
# Reference: Customer Segmentation Part 1: K Means Clustering
#             Written on August 7, 2016
#             https://www.business-science.io/business/2016/08/07/CustomerSegmentationPt1.html
###################################################

library(dplyr)
library(cluster) # Needed for silhouette function
library(ggplot2)
library(bindr)

################# SECTION 1 - DATA LOAD #################
# set working directory for FACILITY DATA
setwd("C:/Users/jyosh/OneDrive/Documents/Northwestern/MSDS_498_Capstone/Data/Prediction Data")

# read cardio_train
cardio<- read.csv("Cardio_Train_Final.csv")
head(cardio)
colnames(cardio)


# identify NA/NAN/INF Records
sum(sapply(cardio, is.na)) ##11
sum(sapply(cardio, is.infinite)) ##0 
sum(sapply(cardio, is.nan)) ##0

# remove NA records
complete_cardio <- cardio[complete.cases(cardio),]
sum(sapply(complete_cardio, is.infinite)) ##0

# create new attribute to bucket age attribute
# hist(complete_cardio$age_years)
# complete_cardio$age35to39 <- ifelse(complete_cardio$age_years %between% c(35,36,37,38,39), 1, 0)
# complete_cardio$age40to44 <-
# complete_cardio$age45to49 <-
# complete_cardio$age50to54 <-
# complete_cardio$age55to59 <-
# complete_cardio$age60to64 <-
# complete_cardio$age65to70 <-

################# SECTION 2 - Running the k-means algorithm ###############
# remove categorical columns from combined cardio set
# cluster on cholesterol, gluc, ap_hi, ap_lo, BMI
kmeansDat <- complete_cardio[,c(7:11)]
colnames(kmeansDat)

# scale data before clustering 
kmeansDat_scaled <- scale(kmeansDat)

# Transform data structure to get cardio attributes as the row name and individal patients as columns
kmeansDat.t <- t(kmeansDat_scaled)    

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2    # Hypothesized minimum number of segments - cluster on at least 2 attributes
maxClust <- 4    # Hypothesized maximum number of segments - max n row attributes 

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeansDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies (complete records, trained on Chol, Gluc, AP HI/LO, BMI")



################# SECTION 3 - Get cardio records that are in each segment ###############
# Get attributes of optimal k-means output
maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)

################# SECTION 5 - Combine cluster centroids with cardio records for feature inspection ###############
cardioSegmentCntrs <- t(km.out.best$centers)  # Get centroids for groups
colnames(cardioSegmentCntrs) <- make.names(colnames(cardioSegmentCntrs))
cntrs_df<-as.data.frame(cardioSegmentCntrs)
cardioTrends.clustered <- cbind(complete_cardio, cntrs_df)
head(cardioTrends.clustered)


################# SECTION 6 - Investigate Clusters for Insights ###############
# Cluster values represent the 

# 1. create cluster histograms to understand range
par(mfrow=c(2,2))
hist(cardioTrends.clustered$X1) 
hist(cardioTrends.clustered$X2)
hist(cardioTrends.clustered$X3)

# HISTOGRAM INSIGHTS
# Cluster X1 (ap_hi/lo) distances are normally distributed - 68% of the patients are within 1 standard deviation from the center of the cluster. 
# Cluster X2 (cholesterol/gluc) distances are mostly bucketed across 5 main groupings. 
# Cluster X3 (BMI) are mostly normally distributed with some right skew. 

summary(cardioTrends.clustered)
sd(cardioTrends.clustered$X1) ## 0.9085382
sd(cardioTrends.clustered$X2) ## 0.8514531
sd(cardioTrends.clustered$X3) ## 1


## EXPORT
write.csv(cardioTrends.clustered, "C:/Users/jyosh/OneDrive/Documents/Northwestern/MSDS_498_Capstone/Data/cardioTrends.clustered.csv")



#show the first ten rows of the clusters
#cluster 1 
plot(cardioTrends.clustered$X3)

#cluster 2
#$X2 "cholesterol" "gluc"     
head(customerTrends.clustered[order(-X2), c(1:2,5:18, 20)], 10)

#cluster 3
#$X3 "age_years"
head(customerTrends.clustered[order(-X3), c(1:2,5:18, 21)], 10)

#cluster 4
#$X4 "ap_hi" "ap_lo"
head(customerTrends.clustered[order(-X4), c(1:2,5:18, 22)], 10)

#cluster 5
#$X5 "weight_kg" "BMI"      
head(customerTrends.clustered[order(-X5), c(1:2,5:18, 23)], 10)





################# APPENDIX TEST - Ward Hierarchical Clustering ###############
# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")



################# APPENDIX TEST - Split Data on Cardio 1/0 ###############
y_cardio <- cardio[which(cardio$cardio ==1), ]
n_cardio <- cardio[which(cardio$cardio ==0), ]


# kmeans can only be used on numerical columns, because it needs to compute the mean.
# Remove "ID" columns, text columns etc. where it does not make sense to compute the mean. 
# Keep columns: age_years, height_cm, weight_kg, BMI, ap_hi, ap_lo, cholesterol, gluc

# remove categorical columns from split cardio sets
y_kmeansDat <- y_cardio[,c(3,5:11)]
n_kmeansDat <- n_cardio[,c(3,5:11)]



########### Yes-Cardio ########### 
# scale data before clustering 
y_kmeansDat_scaled <- scale(y_kmeansDat)
y_kmeansDat.t <- t(y_kmeansDat_scaled)

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2 # Hypothesized minimum number of segments
maxClust <- 6    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(y_kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(y_kmeansDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Yes-Cardio - Silhouette Average Width as Cluster Center Varies")


########### No-Cardio ########### 
# scale data before clustering 
n_kmeansDat_scaled <- scale(n_kmeansDat)
n_kmeansDat.t <- t(n_kmeansDat_scaled)

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2 # Hypothesized minimum number of segments
maxClust <- 6    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(n_kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(n_kmeansDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("No-Cardio - Silhouette Average Width as Cluster Center Varies")