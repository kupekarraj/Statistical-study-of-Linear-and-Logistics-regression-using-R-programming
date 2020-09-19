#US-Germany Relationship Survey
dframe<-read.csv("C:/Users/kupekarraj/Desktop/Statistics/Statitics/Us_Germany.csv",header = TRUE,na.string = "")
head(dframe,10)

#deleting the sex,age and country variables, as because this are the irrelevant variables for the case .
#dframe<-dframe[-13:-15]
View(dframe)
str(dframe)
summary(dframe)

#converting the data type of variables from int to factor, for DV(Q1c) and IV's.
dframe$rid<-as.numeric(dframe$rid)
dframe$Q1a<-as.factor(dframe$Q1a)
dframe$Q1b<-as.factor(dframe$Q1b)
dframe$Q1c<-as.factor(dframe$Q1c) #Dependent Variable
dframe$Q2a<-as.factor(dframe$Q2a)
dframe$Q2b<-as.factor(dframe$Q2b)
dframe$Q4<-as.factor(dframe$Q4)
dframe$Q5<-as.factor(dframe$Q5)
dframe$Q7<-as.factor(dframe$Q7)
dframe$Q8<-as.factor(dframe$Q8)
dframe$Q10<-as.factor(dframe$Q10)
dframe$Q11<-as.factor(dframe$Q11)
dframe$age<-as.numeric(dframe$age)
dframe$sex<-as.factor(dframe$sex)
dframe$country<-as.factor(dframe$country)
summary(dframe)

library(car)
dframe$Q1c<-recode(dframe$Q1c,"c('1','2')='yes';c('3','4','9')='no'")
dframe$Q1c<-factor(dframe$Q1c,levels = c("yes","no"),labels = c("1","0"))
table(dframe$Q1c)

#Looking for the missing values
sapply(dframe,function(x)sum(is.na(x)))

#Looking for any outliers in the data
boxplot(dframe)


#Creating data partition
library(caret) #library for data partition

"As the DV is having binomial category, thus to perform binominal Logistics Regression
it is necessary to provide the reference (base) for the DV. By default it will take the 1st value as reference"
table(dframe$Q1c)
dframe$Q1c<-relevel(dframe$Q1c,ref = "1")
table(dframe$Q1a)
dframe$Q1a<-relevel(dframe$Q1a,ref="2")
table(dframe$Q1b)
dframe$Q1b<-relevel(dframe$Q1b,ref = "2")
table(dframe$Q4)
dframe$Q4<-relevel(dframe$Q4,ref = "2")
table(dframe$Q7)
dframe$Q7<-relevel(dframe$Q7,ref = "2")
table(dframe$Q8)
dframe$Q8<-relevel(dframe$Q8,ref = "2")
table(dframe$Q10)
dframe$Q10<-relevel(dframe$Q10,ref = "2")

Train<-createDataPartition(dframe$Q1c,p=0.7,list= FALSE)
training<-dframe[Train,]
testing<-dframe[-Train,]

#MOdel building
#library(nnet) #library for Multinominal LOgistic Regression
lmodel<-step(glm(Q1c~.,data = training,family = "binomial"),direction = "both")
summary(lmodel)

#Odds Ratio
exp(lmodel$coefficients)
exp(confint(lmodel))
#checking the Multicolinearity using the variance inflation factor
library(car)
vif(lmodel)

"Neither of the variable is having vif value greater than 5.
Thus the assumption of absence of multicolinearity is statisfied."

#Prediction 
testing$predict<-predict(lmodel,testing,type = "response")
testing$Predict<-as.factor(ifelse(testing$predict>0.70,0,1))
#Accuracy
library(e1071)
confusionMatrix(testing$Predict,testing$Q1c)



                                                                          
