###################################################
# Join Healthcare Facility Data Attributes 
# w/Cardio_Train Health Individual Records
###################################################

library(dplyr)

################# SECTION 1 - DATA LOAD #################
#set working directory for FACILITY DATA
setwd("C:/Users/jyosh/OneDrive/Documents/Northwestern/MSDS_498_Capstone/Data/Supplemental Data/edited_target_columns")

#read in files
hospital <- read.csv("hospitals.csv",fileEncoding="UTF-8-BOM")
inpatient_rehab <- read.csv("inpatient_rehab.csv",fileEncoding="UTF-8-BOM")
nursing_homes <- read.csv("nursing_homes.csv",fileEncoding="UTF-8-BOM")
nyc_hospitals <- read.csv("nyc_hospitals.csv",fileEncoding="UTF-8-BOM")
urgent_care <- read.csv("urgent_care.csv",fileEncoding="UTF-8-BOM")
veterans_health <- read.csv("veterans_health.csv",fileEncoding="UTF-8-BOM")

#check file import
head(hospital) 

str(inpatient_rehab)
str(nursing_homes)
str(nyc_hospitals)
str(urgent_care)
str(veterans_health)


#set working directory for CARDIO_TRAIN str(hospital)
setwd("C:/Users/jyosh/OneDrive/Documents/Northwestern/MSDS_498_Capstone/Data/Prediction Data")

#read in file
cardio_train <- read.csv("Cardio_Train_Final.csv")


################# SECTION 2 - FACILITY DATA JOIN #################
# Creates 1 single data frame with all location data 

bind1<- bind_rows(hospital, inpatient_rehab)
bind2<- bind_rows(bind1, nursing_homes)
bind3<- bind_rows(bind2, nyc_hospitals)
bind4<- bind_rows(bind3, urgent_care)
bind5<- bind_rows(bind4, veterans_health)


################# SECTION 3 - CARDIO DATA JOIN #################
# Left join cardio_train with single facility data frame
join1<- left_join(cardio_train, bind5, by = c("ZIP" = "ZIP", "LATITUDE" = "LATITUDE",  "LONGITUDE" = "LONGITUDE"))

# Check for any bad joins - where Facility Name is NULL
sum(is.na(join1$FACILITY_NAME)) ## 255
data_null <- subset(join1, is.na(join1$FACILITY_NAME))


################# SECTION 3 - EXPORT CONSOLIDATED DATASET #################
# Create CSV
write.csv(join1,"C:/Users/jyosh/OneDrive/Documents/Northwestern/MSDS_498_Capstone/Data/cardio_facility_combine.csv", row.names = FALSE)
