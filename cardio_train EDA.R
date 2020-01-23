#install and load needed packages
install.packages("ggplot2")
install.packages('plyr')
install.packages('tidyverse')
install.packages("gridExtra")
install.packages('psych')
install.packages('ggmap')
install.packages('GGally')
install.packages('cdata')

library(tidyverse)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(ggmap)
library(dplyr)
library(GGally)
library(cdata)

# set working directory
setwd("C:/Users/jmr411")


####################################################################

# load data set 
# source data in csv file
# if there are any blank entries, then replace with NA
cardio_stats <- read.csv("cardio_train_clean.csv", header=T)

# examine the first six rows of the data set
head(cardio_stats)

# provide summary descriptive statistics of num data by column
# mean,median,25th and 75th quartiles,min,max
# frequency counts for other columns, up to six rows
# this will also tell you the number of NA's in each column
summary(cardio_stats)


# examine the datatype of the
describe(cardio_stats)

#BMI quartile analysis.  
#We will remove anything above the 99.5th percentile
BMI <- cardio_stats$BMI
quantile(BMI, c(.8, .85, .9, .95, .98, .99, .995))

bar1 <- ggplot(cardio_stats) +
  geom_point( aes(x = weight_lb, y= age_years) ) +
  ggtitle("Number of houses per Lotshape") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
bar1

set.seed(1234)
hist1 <- ggplot(cardio_stats, aes(x=BMI)) + geom_histogram()
hist1

test1 <- ggplot(cardio_stats, aes(x=weight_lb, y=age_years)) + 
  geom_boxplot(color="blue", shape=1) +
  ggtitle("Scatter Plot of Total Floor SF vs QualityIndex") +
  theme(plot.title=element_text(lineheight=0.6, face="bold", hjust=0.5))
test1

# adjust data to generate counts 
dam1 <- count(cardio_stats, c("cardio", "gender"))
dam1

# basic stacked plot for gender break down of cardio 
plt1 <- ggplot(dam1, aes(x= gender, y=freq, fill=cardio)) + geom_bar(stat="identity")
plt1

# adjust data to generate counts 
dam2 <- count(cardio_stats, c("age_years"))
dam2

#basic bar chart for age distribution
plt2 <- ggplot(cardio_stats, aes(age_years)) + geom_bar()
plt2

#scatterplot age and weight
scatter1 <- ggplot(cardio_stats, aes(x= Age_years, y= BMI)) + geom_point(shape=1) 
scatter1

#boxplot age and weight
box1 <- ggplot(cardio_stats, aes(x = gender, y = )) + geom_boxplot()
box1

bplot <- ggplot(cardio_stats, aes(x = weight_kg , y = age_years)) + 
  geom_boxplot()
bplot



