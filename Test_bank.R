library(dplyr)
library(caret)
library(ggplot2)


getwd()
setwd("/Users/ignacioalmodovarcardenas/Desktop/Statistical Learning/Statistical_Learning/")

data=read.csv("data.csv")

####### PREPROCESS DATA###
## https://www.section.io/engineering-education/data-preprocessing-in-r/
##########################
## 1
#We check for missing values

#As this data set is very long, we are going to create a function that tells how many
#missing values we have in each column.

missingValues=function(data){
  count=0
  a=cbind(lapply(lapply(data, is.na), sum))
  for(i in 1:ncol(data)){
    if(a[i]!=0){
      cat("There are", a[i], "missing values in column ", i,"\n" )
      count=count+1
    }
  }  
    if(count==0){
      cat("There are No missing values in this dataset")
    }
}

#Use the function
missingValues(data)

#As we do not have any missing values we do not have to do anything in this step.

#Make bankcrupcy as a factor
data$Bankrupt.=as.factor(data$Bankrupt.)
summary(data)

## 2
#Select variables that we are going to use 

##UNDERSTAND DATA SET
## https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.55.5309&rep=rep1&type=pdf
#####################



#colum 1- Brankcrupcy factor
data$Bankrupt.
#column 58 - Cash/total assets
data$Cash.Total.Assets
#column 38 - Liability/total assets
data$Debt.ratio..
#Column 59 - quiack Assets/current Liability
data$Quick.Assets.Current.Liability


#Create a data.frame just using the columns that are useful
data_m1= data %>% dplyr::select(c(1,38,58,59))
names(data_m1)
head(data_m1)
######################3
#2nd Model

#x73 - Working capital to sales
#x74 - Cash to sales
#x81 -Cash to debt

data_m2= data %>% dplyr::select(c(1,74,75,82))

names(data_m2)

# Encode categorical data
#Make variable bankcrupcy factor


## 3 
# Split data into Training and Test
set.seed(1234)
index1=createDataPartition(y=data_m1$Bankrupt.,times = 1,p=0.7,list = FALSE)

trainSet1=data_m1[index1,]
testSet1=data_m1[-index1,]

############### 2nd model

index2=createDataPartition(y=data_m2$Bankrupt.,times = 1,p=0.8,list = FALSE)

trainSet2=data_m2[index2,]
testSet2=data_m2[-index2,]

## 4
# Standardize continuous data

#check if data_m1 is normalized
apply(trainSet1,MARGIN = 2,min)
apply(trainSet1,2,max)

#We can consider that our data is already normalized


#############################
#### ANALYZE DATA
#############################

pairs(trainSet1[,2:4],pch=21,col=c("blue","orange")[trainSet$Bankrupt.])

pairs(trainSet2[,2:4],pch=19,col=c("blue","orange")[trainSet$Bankrupt.])

