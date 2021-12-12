library(dplyr)
library(caret)
library(ggplot2)
library(klaR)


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
#column 58 - Cash/total assets
#column 38 - Liability/total assets
#Column 59 - quiack Assets/current Liability

data %>% ggplot(aes(x =Total.income.Total.expense)) +  
  geom_density(aes( colour = Bankrupt., fill = Bankrupt.),alpha = 0.2)


#Create a data.frame just using the columns that are useful
data_m1= data %>% dplyr::select(c(1,2,58,38,84,86))
pairs(data_m1[,2:6],pch=21,col=c("blue","orange")[data_m1$Bankrupt.])

#Now we take the variables that I found more interesting
data_m1_1=dplyr::select(data_m1,c(1,2,3,6,7))
names(data_m1_1)

pairs(data_m1_1[,2:5],pch=19,col=c("blue","orange")[data_m1_1$Bankrupt.])

######################
#2nd Model

#x73 - Working capital to sales
#x74 - Cash to sales
#x81 -Cash to debt

data_m2= data %>% dplyr::select(c(1,74,75,82))
names(data_m2)

######################
#3rd Model

#x6 - operating Cashflow to sales
#x36 - debt/equity
#x59 -Cash to debt

data_m3= data %>% dplyr::select(c(1,7,37,59))
names(data_m3)

#As we are considering ratios in all the cases, we do not need to normalize the data


#############################
#### ANALYZE DATA
#############################

pairs(data_m1[,2:7],pch=19,col=c("blue","orange")[data_m1$Bankrupt.])
pairs(trainSet2[,2:4],pch=19,col=c("blue","orange")[trainSet2$Bankrupt.])
pairs(data_m3[,2:4],pch=21,col=c("blue","red")[data_m3$Bankrupt.])

#I'm going to stick with the first model as it is the one that seems to be good for classifying

## 3 
# Split data into Training and Test
set.seed(1234)
index1=createDataPartition(y=data_m1$Bankrupt.,times = 1,p=0.7,list = FALSE)

trainSet1=data_m1[index1,]
testSet1=data_m1[-index1,]



ggplot(trainSet1,aes(x=Bankrupt.,y =Cash.Total.Assets )) + geom_boxplot()


data_m1 %>% ggplot(aes(x =ROA.C..before.interest.and.depreciation.before.interest)) +  
  geom_density(aes( colour = Bankrupt., fill = Bankrupt.),alpha = 0.2)

trainSet1 %>% ggplot(aes(x =Debt.ratio..)) +  
  geom_density(aes( colour = Bankrupt., fill = Bankrupt.),alpha = 0.2)

trainSet1 %>% ggplot(aes(x =Cash.Flow.to.Equity)) +  
  geom_density(aes( colour = Bankrupt., fill = Bankrupt.),alpha = 0.2)

ggplot(trainSet1,aes(x=Bankrupt.,y = ROA.C..before.interest.and.depreciation.before.interest)) + geom_boxplot()

ggplot(trainSet1,aes(x=Bankrupt.,y = Debt.ratio..)) + 
  geom_boxplot() #+ coord_cartesian(ylim = c(0, 0.5)) 

#4D QDA

qda.class.bnk <- qda(Bankrupt. ~ ., trainSet1)
partimat(Bankrupt. ~ ., data=trainSet1, method="qda")
pred.qda = predict(qda.class.bnk, testSet1)$class
colors.qda.bnk.good.bad <- c("black","red")[1*(testSet1[,1]==pred.qda)+1]
pairs(testSet1[,1:5],main="Bad (in black) classifications for Iris flowers with QDA",
      pch=19,col=colors.qda.bnk.good.bad,lower.panel=NULL)

ConfMat.qda = table(pred.qda, testSet1$Bankrupt.)
ConfMat.qda

n = dim(testSet1)[1]
error.qda <- (n - sum(diag(ConfMat.qda))) / n
error.qda # 3% 

lda.class.bnk <- lda(Bankrupt. ~ ., trainSet1)
partimat(Bankrupt. ~ .,data=trainSet1,method="lda")





