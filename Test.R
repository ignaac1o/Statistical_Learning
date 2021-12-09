library(dplyr)
library(caret)
library(ggplot2)


getwd()
setwd("/Users/ignacioalmodovarcardenas/Desktop/Statistical Learning/Statistical_Learning/")

data=read.csv2("TravelInsurancePrediction.csv",sep=",")

####### PREPROCESS DATA###
## https://www.section.io/engineering-education/data-preprocessing-in-r/
##########################
## 1
#We check for missing values
cbind(lapply(lapply(data, is.na), sum))
#As we do not have anny missing values we do not have to do anything in this step.


## 2
#Encode categorical data

data$Employment.Type %>% factor(levels = c("Government Sector","Private Sector/Self Employed"),
                                  labels = c(1,2))

data$GraduateOrNot %>% factor(levels = c("Yes","No"),labels = c(1,0))
data$FrequentFlyer %>% factor(levels = c("Yes","No"),labels = c(1,0))
data$EverTravelledAbroad %>% factor(levels = c("Yes","No"),labels = c(1,0))

data$FamilyMembers=as.factor(data$FamilyMembers)

## 3 
# Split data into Training and Test
index=createDataPartition(y=data$Employment.Type,times = 1,p=0.8,list = FALSE)

trainSet=data[index,]
testSet=data[-index,]

## 4
# Standardize continuous data

trainSet[,c(2,5)] %>% scale()
testSet[,c(2,5)] %>% scale()


#############################
#### ANALYZE DATA
#############################

ggplot(trainSet,aes(x=Age, y =AnnualIncome)) + geom_point()

ggplot(trainSet,aes(x=Age,fill=GraduateOrNot)) + geom_density(alpha=0.6)

pairs(x = trainSet,)



