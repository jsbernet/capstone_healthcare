#MSDS 498
#Capstone

#https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
set.seed(1680) # for reproducibility

library(dplyr) # for data cleaning
library(ggplot2) # for visualization
library(plyr)
#install.packages("devtools")
#library(devtools)
#devtools::install_github("hadley/dplyr#2190")

setwd("~/Desktop/R/")

# read cardio_train
cardio_cluster<- read.csv("cardio_with_cluster.csv")
head(cardio_cluster)
colnames(cardio_cluster)
str(cardio_cluster)

#convert cluster column from int to factor
cardio_cluster$cluster <- as.factor(cardio_cluster$cluster)

#check result
glimpse(cardio_cluster)

#count clusters and save 
cluster_count <- count(cardio_cluster, 'cluster')

#add percent column for each count
cluster_count$percent <- prop.table(cluster_count$freq)

#add percent column by dplyr
cluster_count %>% mutate(Percentage = round((freq / sum(freq))*100,1))