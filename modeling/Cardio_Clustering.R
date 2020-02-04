###################################################
# K-Means Clustering Algorithm for Cardio_Train
# Reference: Customer Segmentation Part 1: K Means Clustering
#             Written on August 7, 2016
#             https://www.business-science.io/business/2016/08/07/CustomerSegmentationPt1.html
###################################################

library(dplyr)
library(cluster) # Needed for silhouette function
library(ggplot2)

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

# kmeans can only be used on numerical columns, because it needs to compute the mean.
# Remove "ID" columns, text columns etc. where it does not make sense to compute the mean. 
# Keep columns: age_years, height_cm, weight_kg, BMI, ap_hi, ap_lo, cholesterol, gluc
kmeansDat <- cardio[,c(3,5:11)]


################# SECTION 2 - Running the k-means algorithm #################

kmeansDat <- cardio[,(5:11)]  # Extract only cardio columns
kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 5 # Hypothesized minimum number of segments
maxClust <- 10    # Hypothesized maximum number of segments

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
  ggtitle("Silhouette Average Width as Cluster Center Varies")
