setwd("C:/Users/Syamala.srinivasan/Google Drive/NorthWestern/LectureMaterials410/RAssignments/Assignments")

#####################################################################

mydata <- read.csv(file="anscombe.csv",head=TRUE,sep=",")

str(mydata)
head(mydata)
names(mydata)

SLRresult = lm(Y1 ~ X1, data=mydata)
summary(SLRresult)
anova(SLRresult)

SLRresult = lm(Y2 ~ X2, data=mydata)
summary(SLRresult)
anova(SLRresult)

SLRresult = lm(Y3 ~ X3, data=mydata)
summary(SLRresult)
anova(SLRresult)

SLRresult = lm(Y4 ~ X4, data=mydata)
summary(SLRresult)
anova(SLRresult)

library(ggplot2)
ggplot(mydata, aes(x=X1, y=Y1)) + 
  geom_point(color="red", size=4) +
  ggtitle("Scatter Plot of Y vs X - data set 1") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)

ggplot(mydata, aes(x=X2, y=Y2)) + 
  geom_point(color="red", size=4) +
  ggtitle("Scatter Plot of Y vs X - data set 2") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)

ggplot(mydata, aes(x=X3, y=Y3)) + 
  geom_point(color="red", size=4) +
  ggtitle("Scatter Plot of Y vs X - data set 3") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)

ggplot(mydata, aes(x=X4, y=Y4)) + 
  geom_point(color="red", size=4) +
  ggtitle("Scatter Plot of Y vs X - data set 4") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5)) +
  geom_smooth(method=lm)



