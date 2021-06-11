discrim <- read_csv("ECON861_Baier/discrim.csv")

summary(discrim$prpblck)
summary(discrim$income)

mean(discrim$prpblck) #Why am I having problems?
mean(discrim$income)

prpblck = discrim$prpblck
income = discrim$income
mean(prpblck,na.rm=TRUE)
mean(income,na.rm=TRUE)
sd(prpblck,na.rm=TRUE)
sd(income,na.rm=TRUE)

0.20*0.1215803
____

#Question 4

summary(hprice1$price, na.rm=TRUE)

lm1 <- lm(hprice1$lprice~hprice1$lassess+hprice1$llotsize+hprice1$lsqrft)
summary(lm1)
((0.758)^2/(1-(0.758)^2 ))*((88-4-1)/4)

qf(.95,df=4,df2=85)

_
lpa<-hprice1$lprice-hprice1$lassess
lm2 <- lm(lpa~1)
summary(lm2)
mean()

anova(lm2)

lpa<-hprice1$lprice-hprice1$lassess
lm4c<- lm(lpa~hprice1$lassess+hprice1$llotsize+hprice1$lsqrft+hprice1$bdrms)
summary(lm4c)
mean(lpa)
sd(lpa)

#SSR:
deviance(lm1)

anova(lm1)
anova(lm4c)
SSRr <- sum((predict(lm1) - mean(lpa))^2)
SSRr
SSRu <- sum((predict(lm1) - mean(hprice1$lprice))^2)
SSRu


Fstat <- ((SSRr-SSRu)/SSRu)*(83/4)
Fstat

Fstat <- ((1.8730-1.82153)/1.82153)*(83/4)
Fstat
1.82153
1.8730

qf(.95,df1=4,df2=83)
___________
#Question 5

50*0.001149882
spt<- Hmwk_2_Exercise_5_Speeding_tickets

lm5a <- lm(spt$Amount~spt$Age)
summary(lm5a)

lm5b <-lm(spt$Amount~spt$Age+spt$MPHover)
summary(lm5b)

spt1000 <- spt[1:1000,]
lm5c <-lm(spt1000$Amount~spt1000$Age+spt1000$MPHover)
summary(lm5c)

sptadj<-na.omit(spt)

sptadj1000<-sptadj[1:1000,]
adjlm5c<-lm(sptadj1000$Amount~sptadj1000$Age+sptadj1000$MPHover)
summary(adjlm5c)
____

clph<-Hmwk_2_Exercise_6_Cell_phone_subscriptions
lm6a<-lm(clph$numberofdeaths~clph$cell_subscription)
summary(lm6a)

lm6b<-lm(clph$numberofdeaths~clph$cell_subscription+clph$population)
summary(lm6b)

lm6c<-lm(clph$numberofdeaths~clph$cell_subscription+clph$population+clph$total_miles_driven)
summary(lm6c)

library(tidyverse)
library(caret)
library(car)
vif(lm6c)
