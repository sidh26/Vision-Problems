PV<-read.csv("C://Users//Sidh Satam//Downloads//poor vision.csv")
PV<-na.omit(PV)

head(PV)

glm.fit<-glm(Y~., data = PV, family = "binomial")
summary(glm.fit)
glm.fit1<-glm(Y~(Gender+Reading_hours+poor_vision_fam+student), data = PV, family = "binomial")
summary(glm.fit1)

plot(glm.fit1, which = 1)
PV<-PV[-c(30,105,132),]
PV<-PV[-c(7,85,124),]
PV<-PV[-c(113,85,124),]

library(caTools)
library(caret)
library(e1071)
set.seed(240299)

#60-40
split = sample.split(PV$Y, SplitRatio = 0.60)
train_set1 = subset(PV, split == TRUE)
test_set1 = subset(PV, split == FALSE)
glm.fitrain10<-glm(Y~., data = train_set1, family = "binomial")
summary(glm.fitrain10)
glm.fitrain1<-glm(Y~(Gender+Reading_hours+poor_vision_fam+student+excessive_tearing), data = train_set1, family = "binomial")
summary(glm.fitrain1)
predTrain1<-predict(glm.fitrain1,type="response")
predTest1 <- predict(glm.fitrain1,type="response",newdata = test_set1)

confusionMatrix(data=factor(as.numeric(predTrain1>0.5)),reference=factor(train_set1$Y))
confusionMatrix(data=factor(as.numeric(predTest1>0.5)),reference=factor(test_set1$Y))

#70-30
split = sample.split(PV$Y, SplitRatio = 0.70)
train_set2 = subset(PV, split == TRUE)
test_set2 = subset(PV, split == FALSE)
glm.fitrain20<-glm(Y~., data = train_set2, family = "binomial")
summary(glm.fitrain20)

glm.fitrain2<-glm(Y~(Gender+poor_vision_fam+student), data = train_set2, family = "binomial")
summary(glm.fitrain2)
predTrain2<-predict(glm.fitrain2,type="response")
predTest2 <- predict(glm.fitrain2,type="response",newdata = test_set2)

confusionMatrix(data=factor(as.numeric(predTrain2>0.5)),reference=factor(train_set2$Y))
confusionMatrix(data=factor(as.numeric(predTest2>0.5)),reference=factor(test_set2$Y))

#80-20
split = sample.split(PV$Y, SplitRatio = 0.80)
train_set3 = subset(PV, split == TRUE)
test_set3 = subset(PV, split == FALSE)
glm.fitrain30<-glm(Y~., data = train_set3, family = "binomial")
summary(glm.fitrain30)

glm.fitrain3<-glm(Y~(Reading_hours+poor_vision_fam+student+Dry_Eyes+excessive_tearing), data = train_set3, family = "binomial")
summary(glm.fitrain3)
predTrain3<-predict(glm.fitrain4,type="response")
predTest3 <- predict(glm.fitrain4,type="response",newdata = test_set3)

confusionMatrix(data=factor(as.numeric(predTrain3>0.5)),reference=factor(train_set3$Y))
confusionMatrix(data=factor(as.numeric(predTest3>0.5)),reference=factor(test_set3$Y))
