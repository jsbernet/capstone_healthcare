# Jennifer Wanat, Capstone
# MSDS 498

# install and load needed packages
install.packages("ggplot2")
install.packages('plyr')
install.packages('tidyverse')
install.packages("gridExtra")
install.packages('psych')
install.packages('ggmap')


library(tidyverse)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(ggmap)
library(dplyr)

# set working directory
setwd("~/Desktop/R/")


####################################################################

# load data set 
# source data in csv file
# if there are any blank entries, then replace with NA
rehab_facility <- read.csv("Inpatient_Rehabilitation_Facility_Conditions.csv", header=T, na.strings=c("","NA"))

# examine the first six rows of the data set
head(rehab_facility)

# provide summary descriptive statistics of num data by column
# mean,median,25th and 75th quartiles,min,max
# frequency counts for other columns, up to six rows
# this will also tell you the number of NA's in each column
summary(rehab_facility)


# examine the datatype of the columns in the dataframe
str(rehab_facility)

####################################################################

# Preparing the data set for use
# Renaming the data columns
names(rehab_facility) <- c('CMS_Number', 'Facility_Name', 'Address_1', 'Address_2', 'City',
                           'State', 'Zip_Code', 'County', 'Phone_Number', 'CMS_Region', 'Condition',
                           'Count', 'Footnote', 'Location')
#checking result
str(rehab_facility)

# Changing data type of columns
rehab_facility$CMS_Number <- as.numeric(rehab_facility$CMS_Number)
rehab_facility2$Facility_Name <- as.character(rehab_facility2$Facility_Name)
rehab_facility$Address_1 <- as.character(rehab_facility$Address_1)
rehab_facility$Address_2 <- as.character(rehab_facility$Address_2)
rehab_facility$City <- as.character(rehab_facility$City)
rehab_facility$State <- as.character(rehab_facility$State)
rehab_facility$Zip_Code <- as.numeric(rehab_facility$Zip_Code)
rehab_facility$County <- as.character(rehab_facility$County)
rehab_facility$Phone_Number <- as.charcter(rehab_facility$Phone_Number)
rehab_facility$CMS_Region <- as.factor(rehab_facility$CMS_Region)
rehab_facility$Condition <-  as.character(rehab_facility$Condition)
rehab_facility$Count <- as.numeric(rehab_facility$Count)
rehab_facility$Footnote <- as.numeric(rehab_facility$Footnote)
rehab_facility$Location <- as.character(rehab_facility$Location)
# Checking result
str(rehab_facility)


#This is useful for variables that are numerical and continuous
#table of n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se
#from psych package
#not so useful here, but the code is here if you need it
rehab_table <- (round(describe(rehab_facility), 1))


#create a png of table
#saved to same location where the cvs file is located
#https://stackoverflow.com/questions/23365096/r-save-table-as-image
#edit the height and width as necessary
png('t1.png', height = 30*nrow(rehab_table), width = 60*ncol(rehab_table))
t1 <- tableGrob(rehab_table)
grid.arrange(t1)
dev.off()


#function to describe variables with missing data 
myNA.variable.summary <- function(variable){
  NAsum.summary <- sum(is.na(variable))
  NAmean.summary <- (round(mean(is.na(variable)), 2)*100)
  return(c(NAsum.summary, NAmean.summary))
}

#table of variables with missing data
NA.CMS <- myNA.variable.summary(rehab_facility$CMS_Number)
NA.Facility <- myNA.variable.summary(rehab_facility$Facility_Name)
NA.Add1 <- myNA.variable.summary(rehab_facility$Address_1)
NA.Add2 <- myNA.variable.summary(rehab_facility$Address_2)
NA.City <- myNA.variable.summary(rehab_facility$City)
NA.State <- myNA.variable.summary(rehab_facility$State)
NA.Zip <- myNA.variable.summary(rehab_facility$Zip_Code)
NA.County <- myNA.variable.summary(rehab_facility$County)
NA.Phone <- myNA.variable.summary(rehab_facility$Phone_Number)
NA.Region <- myNA.variable.summary(rehab_facility$CMS_Region)
NA.Condition <- myNA.variable.summary(rehab_facility$Condition)
NA.Count <- myNA.variable.summary(rehab_facility$Count)
NA.Footnote <- myNA.variable.summary(rehab_facility$Footnote)
NA.Location <- myNA.variable.summary(rehab_facility$Location)

# table of NA counts for variables
overview.NA <- rbind(NA.CMS, NA.Facility, NA.Add1, NA.Add2, NA.City, NA.State,
                     NA.Zip, NA.County, NA.Phone, NA.Region, NA.Condition,
                     NA.Count, NA.Footnote, NA.Location)
colnames(overview.NA) <- c('Number of missing values', 'Percent of missing values')
rownames(overview.NA) <- c('CMS_Number', 'Facility_Name', 'Address_1', 'Address_2', 'City',
                           'State', 'Zip_Code', 'County', 'Phone_Number', 'CMS_Region', 'Condition',
                           'Count', 'Footnote', 'Location')

#create a png of table
#saved to same location where the cvs file is located
#https://stackoverflow.com/questions/23365096/r-save-table-as-image
png('t2.png')
t2 <- tableGrob(overview.NA)
grid.arrange(t2)
dev.off()


#when you are finished with your work you can 
#write dataframe to new csv file
write.csv(rehab_facility,'rehab_facility.csv')


###################################
#working on creating lat and long for Stroke data

# create new dataframe of Condition = Stroke
condition_location <- subset(rehab_facility, subset = rehab_facility$Condition == 'Stroke')

#removing rows from dataframe where Location is equal to NA
condit_loc_only <- condition_location[!is.na(condition_location$Location), ]


#https://stackoverflow.com/questions/41551719/spliting-the-location-column-into-zipcode-latitude-and-longitude
condit_loc_only$latitude <- gsub('.*\\((.*),.*', '\\1', condit_loc_only$Location)
condit_loc_only$longitude <- gsub('.*, (.*)\\).*', '\\1', condit_loc_only$Location)

condit_loc_only$latitude <- as.numeric(condit_loc_only$latitude)
condit_loc_only$longitude <- as.numeric(condit_loc_only$longitude)
str(condit_loc_only)

