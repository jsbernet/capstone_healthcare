#MSDS 498
#Capstone

#https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
set.seed(1680) # for reproducibility

install.packages('ISLR')
install.packages('Rtsne')

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(plyr)

setwd("~/Desktop/R/")

# read cardio_train
cardio<- read.csv("Cardio_Train_Final.csv")
head(cardio)
colnames(cardio)
str(cardio)

glimpse(cardio)

# remove NA records
complete_cardio <- cardio[complete.cases(cardio),]
sum(sapply(complete_cardio, is.infinite)) ##0
sum(sapply(complete_cardio, is.na)) ##0

#modify indicators as factors
complete_cardio$smoke <- as.factor(complete_cardio$smoke) 
complete_cardio$alcohol <- as.factor(complete_cardio$alcohol) 
complete_cardio$active <- as.factor(complete_cardio$active) 
complete_cardio$cardio <- as.factor(complete_cardio$cardio) 

#rename factors
levels(complete_cardio$smoke) <- c("non-smoker", "smoker")
levels(complete_cardio$alcohol) <- c("non-drinker", "drinker")
levels(complete_cardio$active) <- c("not-active", "active")
levels(complete_cardio$cardio) <- c("non-cardio", "cardio")
glimpse(complete_cardio)

#only want rows where there is cardio disease
cardio_male <- complete_cardio[which(complete_cardio$gender == "male"),]

# Remove id, age_days, height,  geo, zip before clustering
complete_cardio_male <- cardio_male[, c(-1,-2,-4,-5,-16, -17, -18)]

glimpse(complete_cardio_male)



#this is computationally expensive and takes a few minutes
gower_dist_male <- daisy(complete_cardio_male,
                     metric = "gower")
#this results in a large dissimiliarity (287532190 elements, 2.1 Gb)

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
summary(gower_dist_male)

#287532190 dissimilarities, summarized :
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1338  0.2110  0.2142  0.2879  0.7504 
#Metric :  mixed ;  Types = I, I, I, I, I, I, I, N, N, N, N 
#Number of objects : 23981

#this is computationally expensive and takes a few minutes
gower_mat_male <- as.matrix(gower_dist_male)

# Output most similar pair
complete_cardio_male[
  which(gower_mat_male == min(gower_mat_male[gower_mat_male != min(gower_mat_male)]),
        arr.ind = TRUE)[1, ], ]

#      age_years weight_kg      BMI ap_hi ap_lo cholesterol gluc      smoke     alcohol     active     cardio
#62522        58      74.2 25.67474   120    80           1    1 non-smoker non-drinker not-active non-cardio
#1333         58      74.0 25.60554   120    80           1    1 non-smoker non-drinker not-active non-cardio


# Output most dissimilar pair
complete_cardio_male[
  which(gower_mat_male == max(gower_mat_male[gower_mat_male != max(gower_mat_male)]),
        arr.ind = TRUE)[1, ], ]

#      age_years weight_kg      BMI ap_hi ap_lo cholesterol gluc      smoke     alcohol     active     cardio
#18160        42        60 21.77384    12    80           1    1 non-smoker non-drinker     active non-cardio
#14619        51       115 36.29592   200   130           3    3     smoker     drinker not-active     cardio


# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit_male <- pam(gower_dist_male,
                  diss = TRUE,
                  k = i)
  
  sil_width[i] <- pam_fit_male$silinfo$avg.width
  
}


# Plot silhouette width (higher is better)
png(filename="silhouette_male.png")
plot(1:10, sil_width,
     xlab = "Number of clusters for Males",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
plot(silhouette_male)
dev.off()


#we can see that 9 clusters is optimal

pam_fit_male <- pam(gower_dist_male, diss = TRUE, k = 9)

pam_results_male <- complete_cardio_male %>%
  mutate(cluster_male = pam_fit_male$clustering) %>%
  group_by(cluster_male) %>%
  do(the_summary = summary(.))

#summary of clusters
pam_results_male$the_summary

#medoids serve as exemplars of each cluster
complete_cardio_male[pam_fit_male$medoids, ]


#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, or t-SNE. #This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D #visualization. While it typically utilizes Euclidean distance, it has the ability to handle a custom distance metric like the one we #created above. In this case, the plot shows the  clusters that PAM was able to detect. 

tsne_obj_m <- Rtsne(gower_dist_male, is_distance = TRUE)

#merge the clusters with the cardio_male
tsne_data_m <- tsne_obj_m$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_male$clustering),
         id = cardio_male$id)

#plot the clusters
png("tsne_plot_m.png", 1200, 480, res=96)
ggplot(aes(x = X, y = Y), data = tsne_data_m) +
  geom_point(aes(color = cluster))
#plot(tsne_plot_m)
dev.off()

#merge cluster information with male rows based on id column in both sets
cardio_male_cluster <- merge(x=tsne_data_m, y=cardio_male, by = "id", all = TRUE)

#compare cluster #1 summary with summary from above
check_m <- cardio_male_cluster %>%
  group_by(cluster) %>%
  do(summary_male = summary(.))

#summary of clusters
check_m$summary_male
#yes, this matches the summary above

#save cardio_with_cluster to csv file
write.csv(cardio_male_cluster, "~/Desktop/R/cardio_male_cluster.csv")


save.image(file="cardio_male.RData")
