ldata<-read.csv("C:\\Users\\kupekarraj\\Desktop\\Statitics\\SDGMALARIA.csv",header = FALSE,na.strings = " ")
head(ldata)
colnames(ldata)<-unlist(ldata[2,])
ldata<-ldata[-1:-2,]
rownames(ldata)<-c(1:107)
ldata$CountryInt<-as.numeric(rownames(ldata))
ldata<-ldata[-1]

str(ldata)
ldata$` 2017`<-as.numeric(ldata$` 2017`)
ldata$` 2016`<-as.numeric(ldata$` 2016`)
ldata$` 2015`<-as.numeric(ldata$` 2015`)
ldata$` 2014`<-as.numeric(ldata$` 2014`)
ldata$` 2013`<-as.numeric(ldata$` 2013`)
ldata$` 2012`<-as.numeric(ldata$` 2012`)
ldata$` 2011`<-as.numeric(ldata$` 2011`)
ldata$` 2010`<-as.numeric(ldata$` 2010`)
ldata$` 2005`<-as.numeric(ldata$` 2005`)
ldata$` 2000`<-as.numeric(ldata$` 2000`)

sapply(ldata, function(x)sum(is.na(x)))
boxplot(ldata)

plot(ldata)
cor(ldata)

library(caret)
Train<-createDataPartition(ldata$` 2017`,p=0.7,list = FALSE)
testing<-ldata[Train,]
training<-ldata[-Train,]

model<-lm(` 2017`~.,data=testing)
summary(model)
vif(model)

model1<-step(lm(` 2017`~.,data=testing),direction="both")
summary(model1)

par(mfrow=c(2,2))
plot(model1)

testing$transDV<-(352-ldata$` 2017`)^1/3
testing2<-ldata[-1]

model3<-step(lm(transDV~.,data=testing2),direction = "both")
summary(model3)
plot(model3)


library(e1071)
library(car)
vif(ldata)
mod<-lm(` 2017`~.,data = ldata)
vif(mod)
par(mfrow=c(2,2))
plot(mod)

mod1<-step(lm(` 2017`~.,data = ldata),direction = "both")
vif(mod1)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)

ldata$transDV<-(352-ldata$` 2017`)^1/3
ldata2<-ldata[-1]

hist(ldata$` 2017`)

hist((352-ldata$` 2017`)^1/3)
ldata$transDV<-(352-ldata$` 2017`)^1/3
ldata2<-ldata[-1]

mod3<-step(lm(transDV~.,data =ldata2 ),direction = "both")
summary(mod3)
plot(mod3)
hist((352-ldata$` 2016`)^1/3)
hist((25697-ldata$` 2017`)^1/9)




hist(log(ldata$` 2017`))
hist(1/ldata$` 2017`)
hist((ldata$` 2017`)^-1)
hist(log10(((ldata$` 2017`)^-2)^1/3))
plotNormalHistogram(transformTukey(ldata$` 2017`, plotit=FALSE))
library(rcompanion)
     