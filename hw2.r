#1
n<-dim(d)[1];

ind<-sample.int(n, size=91)

dtrain<-d[-ind,];
dtest<-d[ind,]

#2&3

RMSPE=function (d, p){
  y1=mean((d-p)^2) ;
  y2=sqrt(y1)
  return(y2) ;
}
#linear model
m1<-lm(Registered~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain);
p1<-predict(m1, newdata=dtest)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p1, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Registered, p1)
m2<-lm(Casual~Index+as.factor(season)+as.factor(holiday)+meanatemp+meanwindspeed+meanhumidity, data=dtrain);
p2<-predict(m2, newdata=dtest)
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p2, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Casual, p2)

#stepwise
m3<-glm(Registered~1, data=dtrain, family="gaussian")
m4<-glm(Registered~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, family="gaussian")
s1<-step(m3, scope=list(lower=m3, upper=m4), direction="forward")
summary(s1)
m5<-glm(Casual~1, data=dtrain, family="gaussian")
m6<-glm(Casual~Index+year+as.factor(month)+as.factor(day)+as.factor(season)+as.factor(holiday)+as.factor(workingday)+meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed+maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, family="gaussian")
s2<-step(m5, scope=list(lower=m5, upper=m6), direction="forward")
summary(s2)
p3=predict(s1, newdata=dtest);
plot(dtest$Index, dtest$Registered, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p3, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Registered, p3);
p4=predict(s2, newdata=dtest);
plot(dtest$Index, dtest$Casual, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p4, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Casual, p4);



#random forest
library(randomForest)

dtrain$month=as.factor(dtrain$month);
dtrain$day=as.factor(dtrain$day);
dtrain$season=as.factor(dtrain$season);
dtrain$holiday=as.factor(dtrain$holiday);
dtrain$workingday=as.factor(dtrain$workingday);
#Registered
r1<-randomForest(Registered~Index+year+month+day+season+holiday+workingday
                 +meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed
                 +maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, ntree=500,
                 do.trace=1, importance=TRUE, proximity=TRUE)

dtest$month=as.factor(dtest$month);
dtest$day=as.factor(dtest$day);
dtest$season=as.factor(dtest$season);
dtest$holiday=as.factor(dtest$holiday);
dtest$workingday=as.factor(dtest$workingday);

p5<-predict(r1, newdata=dtest, type="response");
plot(dtest$Index, dtest$Registered, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p5, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Registered, p5);
#Casual
r2<-randomForest(Casual~Index+year+month+day+season+holiday+workingday
                 +meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed
                 +maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, ntree=500,
                 do.trace=1, importance=TRUE, proximity=TRUE)
p6<-predict(r2, newdata=dtest, type="response");
plot(dtest$Index, dtest$Casual, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p6, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Casual, p6);

#SVM
library(e1071)

s3<-svm(for1, data=dtrain)

p7<-predict(s3, newdata=dtest, type="response");
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p7, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Registered, p7);

s4<-svm(for1, data=dtrain)

p8<-predict(s4, newdata=dtest, type="response");
plot(dtest$Index, dtest$Total, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p8, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Casual, p8);

#4
RMSPE(dtest$Total, p5+p6)

#5
library(randomForest)

dtrain$month=as.factor(dtrain$month);
dtrain$day=as.factor(dtrain$day);
dtrain$season=as.factor(dtrain$season);
dtrain$holiday=as.factor(dtrain$holiday);
dtrain$workingday=as.factor(dtrain$workingday);
#Total
r3<-randomForest(Total~Index+year+month+day+season+holiday+workingday
                 +meanatemp+maxatemp+minatemp+sdatemp+meanhumidity+maxhumidity+minhumidity+sdhumidity+meanwindspeed
                 +maxwindspeed+minwindspeed+sdwindspeed, data=dtrain, ntree=500,
                 do.trace=1, importance=TRUE, proximity=TRUE)

dtest$month=as.factor(dtest$month);
dtest$day=as.factor(dtest$day);
dtest$season=as.factor(dtest$season);
dtest$holiday=as.factor(dtest$holiday);
dtest$workingday=as.factor(dtest$workingday);

p9<-predict(r3, newdata=dtest, type="response");
plot(dtest$Index, dtest$Registered, pch="*", col=2, xlab="Day", ylab="Demand");
points(dtest$Index, p9, pch="o", col=4)
legend("topleft", legend=c("Observed Demand", "Predicted Demand"), pch=c("*","o"), col=c(2,4))
RMSPE(dtest$Total, p9);

#6
#Aggregate method is better because it has lower RMSPE

