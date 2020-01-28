#install and load needed packages
install.packages("ggplot2")
install.packages('plyr')
install.packages('tidyverse')
install.packages("gridExtra")
install.packages('psych')
install.packages('ggmap')
install.packages('GGally')
install.packages('cdata')
install.package('corrplot')

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
library(corrplot)


# set working directory
setwd("C:/Users/jmr411")
setwd("~/Desktop/R/")

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


# examine the datatype
describe(cardio_stats)

#BMI quartile analysis.  
#We will remove anything above the 99.5th percentile
BMI <- cardio_stats$BMI
quantile(BMI, c(.8, .85, .9, .95, .98, .99, .995))

bar1 <- ggplot(cardio_stats) +
  geom_point( aes(x = weight_lb, y= age_years) ) +
  ggtitle("Weight by Age") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
bar1

set.seed(1234)
hist1 <- ggplot(cardio_stats, aes(x=BMI)) + geom_histogram()
hist1

test1 <- ggplot(cardio_stats, aes(x=weight_lb, y=age_years)) + 
  geom_boxplot(color="blue", shape=1) +
  ggtitle("Box Plot of Weight by Age") +
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


#################### Histogram matrix
#https://ggplot2.tidyverse.org/reference/geom_histogram.html

cardiolong <- reshape2::melt(cardio_stats[, c(2:11)])
cardiolong <- select(cardiolong, -starts_with("gender"))

hist_all <- ggplot(cardiolong, aes(value)) + facet_wrap(~variable, scales = 'free_x') +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
hist_all


###################################
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# age_years
################## Group 1
# side-by-side histograms
# hist on left has mean as dashed line
# hist on right has density curve overlay
# Add mean line
hstmean_age_yrs <- ggplot(cardio_stats, aes(x=age_years)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(age_years)),
             color="blue", linetype="dashed", size=1)
hstmean_age_yrs
# Histogram with density plot
hist_age_yrs <- ggplot(cardio_stats, aes(x=age_years)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
hist_age_yrs 

ggarrange(hstmean_age_yrs, hist_age_yrs, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")

################## Group 2
# stacked histograms
# variable separated by gender
# mean of variable as dashed line
#library(plyr)
mu <- ddply(cardio_stats, "gender", summarise, grp.mean=mean(age_years))
head(mu)

p<-ggplot(cardio_stats, aes(x=age_years))+
  geom_histogram(color="black", fill="white")+
  facet_grid(gender ~ .) 
p
p +  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                linetype="dashed")

################## Group 3
# overlay of histograms by gender
# mean of variable by gender as dashed line
p <- ggplot(cardio_stats, aes(x=age_years, color=gender, fill=gender)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Age_years histogram plot",x="Age(years)", y = "Count")+
  theme_classic()
p

###################


###################################
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# height_cm
################## Group 1
# side-by-side histograms
# hist on left has mean as dashed line
# hist on right has density curve overlay
# Add mean line
hstmean_heightcm2 <- ggplot(cardio_stats, aes(x=height_cm)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(height_cm)),
             color="blue", linetype="dashed", size=1)
hstmean_heightcm2
# Histogram with density plot
hist_heightcm2 <- ggplot(cardio_stats, aes(x=height_cm)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
hist_heightcm2

ggarrange(hstmean_heightcm2, hist_heightcm2, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")

################## Group 2
# stacked histograms
# variable separated by gender
# mean of variable as dashed line
library(plyr)
mu <- ddply(cardio_stats, "gender", summarise, grp.mean=mean(height_cm))
head(mu)

p<-ggplot(cardio_stats, aes(x=height_cm))+
  geom_histogram(color="black", fill="white")+
  facet_grid(gender ~ .) 
p
p +  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                linetype="dashed")

################## Group 3
# overlay of histograms by gender
# mean of variable by gender as dashed line
p <- ggplot(cardio_stats, aes(x=height_cm, color=gender, fill=gender)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Height_cm histogram plot",x="Height(cm)", y = "Count")+
  theme_classic()
p

###################



###################################
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# weight_kg
################## Group 1
# side-by-side histograms
# hist on left has mean as dashed line
# hist on right has density curve overlay
# Add mean line
hstmean_weight_kg <- ggplot(cardio_stats, aes(x=weight_kg)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(weight_kg)),
             color="blue", linetype="dashed", size=1)
hstmean_weight_kg
# Histogram with density plot
hist_weight_kg <- ggplot(cardio_stats, aes(x=weight_kg)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
hist_weight_kg

ggarrange(hstmean_weight_kg, hist_weight_kg, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")

################## Group 2
# stacked histograms
# variable separated by gender
# mean of variable as dashed line
#library(plyr)
mu <- ddply(cardio_stats, "gender", summarise, grp.mean=mean(weight_kg))
head(mu)

p<-ggplot(cardio_stats, aes(x=weight_kg))+
  geom_histogram(color="black", fill="white")+
  facet_grid(gender ~ .) 
p
p +  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                linetype="dashed")

################## Group 3
# overlay of histograms by gender
# mean of variable by gender as dashed line
p <- ggplot(cardio_stats, aes(x=weight_kg, color=gender, fill=gender)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight_kg histogram plot",x="Weight(kg)", y = "Count")+
  theme_classic()
p

###################

###################################
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# BMI
################## Group 1
# side-by-side histograms
# hist on left has mean as dashed line
# hist on right has density curve overlay
# Add mean line
hstmean_bmi <- ggplot(cardio_stats, aes(x=BMI)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(BMI)),
             color="blue", linetype="dashed", size=1)
hstmean_bmi
# Histogram with density plot
hist_bmi <- ggplot(cardio_stats, aes(x=BMI)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
hist_bmi

ggarrange(hstmean_bmi, hist_bmi, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")

################## Group 2
# stacked histograms
# variable separated by gender
# mean of variable as dashed line
#library(plyr)
mu <- ddply(cardio_stats, "gender", summarise, grp.mean=mean(BMI))
head(mu)

p<-ggplot(cardio_stats, aes(x=BMI))+
  geom_histogram(color="black", fill="white")+
  facet_grid(gender ~ .) 
p
p +  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                linetype="dashed")

################## Group 3
# overlay of histograms by gender
# mean of variable by gender as dashed line
p <- ggplot(cardio_stats, aes(x=BMI, color=gender, fill=gender)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="BMI histogram plot",x="BMI", y = "Count")+
  theme_classic()
p

###################

###################################
#http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

# ap_hi
################## Group 1
# side-by-side histograms
# hist on left has mean as dashed line
# hist on right has density curve overlay
# Add mean line
hstmean_ap_hi <- ggplot(cardio_stats, aes(x=ap_hi)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(ap_hi)),
             color="blue", linetype="dashed", size=1)
hstmean_ap_hi
# Histogram with density plot
hist_ap_hi <- ggplot(cardio_stats, aes(x=ap_hi)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
hist_ap_hi

ggarrange(hstmean_ap_hi, hist_ap_hi, ncol=2, nrow=1,
          common.legend = TRUE, legend = "bottom")

################## Group 2
# stacked histograms
# variable separated by gender
# mean of variable as dashed line
#library(plyr)
mu <- ddply(cardio_stats, "gender", summarise, grp.mean=mean(BMI))
head(mu)

p<-ggplot(cardio_stats, aes(x=BMI))+
  geom_histogram(color="black", fill="white")+
  facet_grid(gender ~ .) 
p
p +  geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                linetype="dashed")

################## Group 3
# overlay of histograms by gender
# mean of variable by gender as dashed line
p <- ggplot(cardio_stats, aes(x=BMI, color=gender, fill=gender)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=gender),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="BMI histogram plot",x="BMI", y = "Count")+
  theme_classic()
p

###################
# Looking at ap_hi and ap_lo
# blood pressure data
# there are errors with this data that should be dropped
#https://www.heartfoundation.org.au/your-heart/know-your-risks/blood-pressure/is-my-blood-pressure-normal

# adjust data to generate counts 
aph_count <- count(cardio_stats, c("ap_hi"))
aph_count
#recommend dropping ap_hi > 250
#recommend dropping ap_hi < 10

# adjust data to generate counts 
apl_count <- count(cardio_stats, c("ap_lo"))
apl_count
#recommend dropping ap_lo > 200
#recommend dropping ap_lo < 10

#####################
# Correlation plot

#select which variables to use for corrplot
numeric_cardio <- subset(cardio_stats, select = c(age_days, age_years, height_cm, height_m,
                                   height_inches, height_feet, weight_kg, weight_lb, BMI,
                                   ap_hi, ap_lo, cholesterol, gluc, smoke, alcohol, active,
                                   cardio), na.rm = TRUE)
c <- cor(numeric_cardio)


#Correlation plot
corrplot::corrplot(c, method = "square", col=brewer.pal(n=8, name="RdBu"),
                   diag = FALSE, tl.cex = 0.7, number.cex = 0.5,
                   type= "upper")




