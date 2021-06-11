#Exam

#Question 1
lm1a <- lm( AK1991$logwage ~ AK1991$edu )
summary(lm1a)

#1b
tapply(AK1991$logwage, AK1991$edu, FUN=mean)

#Phoebe's code:
new.data<- data.frame(edu=0:20)
pred<- predict(AK1991, newdata = new.data)
pred
avglogwage<- as.data.table(pred, nrow(20))
numobs<-plyr::count(AK1991, vars="edu")


Yschooling = subset(ak, ak$edu==0)
mean(Yschooling$logwage)

results <- 0
for (i in 0:20){
  results <- 4.9951823 + 0.0708510*i
  print(results[i])
}

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)

print(i)
mean(AK1991$logwage)

years_educ = c(0:20)
log_wage = c(5.0220, 5.0642, 5.1667, 5.1730, 5.2644, 5.3308, 5.4039, 5.4702, 5.5724, 5.6462, 5.6853, 5.7317, 5.8443, 5.9171, 5.9629, 6.0075, 6.2319, 6.2295, 6.2681, 6.2114, 6.3109) 
plot(years_educ,log_wage)

#partc
wls<- c(0.00181, 0.00065, 0.00184, 0.00330, 0.00408, 0.00608, 0.01176, 0.01875, 0.04892, 0.04001, 0.05017, 0.04150, 0.37308, 0.05541, 0.07794, 0.03027,0.10849, 0.03661, 0.03537, 0.01914, 0.03484)
lmc<- lm(log_wage~years_educ, weights=wls)
summary(lmc)

years_educ=c(0:20)
log_wage=c()
plot(years_educ, log_wage)

w<- c(0.00181, 0.00065, 0.00184,)
lm1c = lm(log_wage ~ years_educ, weights=w)

lm1d = lm(logwage ~ edu + black, data=AK1991)
lm1d = lm(logwage ~ black, data=AK1991)
summary(lm1d)

lm1e = lm(logwage ~ black + edu + black*edu, data=AK1991)

data_1ei = data.frame(edu=8, black=1)
predict(lm1e,data_1ei)
data_1ei = data.frame(edu=8, black=0)
predict(lm1e,data_1ei)

data_1eii = data.frame(edu=12, black=1)
predict(lm1e,data_1eii)
data_1eii = data.frame(edu=12, black=0)
predict(lm1e,data_1eii)

data_1eiii = data.frame(edu=16, black=1)
predict(lm1e,data_1eiii)
data_1eiii = data.frame(edu=16, black=0)
predict(lm1e,data_1eiii)  

5.601765-5.31039
5.871329-5.587906
6.140893-5.865422

data_1eiii = data.frame(edu=20, black=1)
predict(lm1e,data_1eiii)
data_1eiii = data.frame(edu=20, black=0)
predict(lm1e,data_1eiii)  

summary(AK1991$edu)
#Phoebe's way:
new.data<-data.frame(edu=0:20)
pred<-predict(SLRak, newdata=new.data)
avglogwage <-as.data.table(pred, rnow(20))
numobs<-plyr::count(AK, vars="edu")

#Number of observations
b<-cbind(avglogwage,numobs)
B<- b[,c(3,2,4)]
B
library(ggplot2)
ggplot(B, aes(x=edu, y=pred)) +geom_point(color="green")

#log???(wage)=??_0+??_1 female+??_2 totcoll+??_3 female*totcoll

twoyear$ftotcoll = twoyear$totcoll*twoyear$female
lm3c <- lm(twoyear$lwage ~ twoyear$female + twoyear$totcoll + twoyear$ftotcoll )
summary(lm3c)
-0.357265/0.029910
summary(twoyear$totcoll)

lm4b <-lm(Card_95$lwage ~ Card_95$educ + Card_95$exper + Card_95$expersq +
            Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
            Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
            Card_95$reg666 + Card_95$reg667 + Card_95$reg668, data=Card_95)
summary(lm4b)

lm4c <-lm(Card_95$educ ~ Card_95$exper + Card_95$expersq +
            Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
            Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
            Card_95$reg666 + Card_95$reg667 + Card_95$reg668 + Card_95$nearc4, data=Card_95)
summary(lm4c)
confint(lm4b)
confint(lm4d)

install aerpackage

lm4d <-ivreg(Card_95$lwage ~ Card_95$educ + Card_95$exper + Card_95$expersq +
                Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
                Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
                Card_95$reg666 + Card_95$reg667 + Card_95$reg668
             |Card_95$exper + Card_95$expersq +
               Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
               Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
               Card_95$reg666 + Card_95$reg667 + Card_95$reg668 + Card_95$nearc4, data=Card_95)
summary(lm4d)

lm4e <-lm(Card_95$educ ~ Card_95$nearc2 + Card_95$nearc4 + Card_95$exper + Card_95$expersq +
            Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
            Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
            Card_95$reg666 + Card_95$reg667 + Card_95$reg668 + Card_95$nearc4, data=Card_95)
summary(lm4e)

lm4eiv<-ivreg(Card_95$lwage ~ Card_95$educ + Card_95$exper + Card_95$expersq +
               Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
               Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
               Card_95$reg666 + Card_95$reg667 + Card_95$reg668
             |Card_95$exper + Card_95$expersq +
               Card_95$black + Card_95$south + Card_95$smsa + Card_95$reg661 + 
               Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
               Card_95$reg666 + Card_95$reg667 + Card_95$reg668 + 
               Card_95$nearc4 + Card_95$nearc2 , data=Card_95)
summary(lm4eiv)

lm4f <-lm(Card_95$IQ ~ Card_95$nearc4)
summary(Card_95$IQ)
summary(lm4f)

lm4g <-lm(Card_95$IQ ~ Card_95$smsa66 + Card_95$reg661 + 
            Card_95$reg662 + Card_95$reg663 + Card_95$reg664 + Card_95$reg665 +
            Card_95$reg666 + Card_95$reg667 + Card_95$reg668 + Card_95$nearc4, data=Card_95)
summary(lm4g)


#Question 5
#colGPA= ??_0+??_1 sat+??_2 hsperc+??_3 hsize+??_4 hsize^2+u

lm5a <- lm ( colgpa ~ sat + hsperc + hsize + hsizesq, data=collGPA)
summary(lm5a)

3.078505-1.96*((0.658634 )/(4137)^(1/2))
3.078505+1.96*((0.658634 )/(4137)^(1/2))

#5e

1.493+(0.001492*1300)-(0.01386*15)-(0.06088*3.5)+(0.005460*((3.5)^2))

newdata = data.frame(sat=1300, hsperc=15, hsize=3.5, hsizesq=12.25)
predict(lm5a, newdata, interval="confidence",level=.95)


0.06088/0.00546 
sd(collGPA$colgpa)

#Question6
lm6a = lm( lsalary ~ lsales + lmktval + ceoten, data=ceosal )
summary(lm6a)

lsalary_predicted = predict(lm6a)
  summary(lsalary_predicted)
salary.badhat = exp(lsalary_predicted)
  summary(salary.badhat)

lm6b = lm( salary ~ 0 + salary.badhat, data=ceosal )
summary(lm6b)
salary_predicted = predict(lm6b)
summary(salary_predicted)

summary(salary_predicted)
summary(ceosal$salary)

#lm6b_comparison = lm(salary~salary_predicted, data=ceosal)
#summary(lm6b_comparison)

#c
res6b = resid(lm6b)

new_salary.badhat = (1.1169*salary.badhat)#lecture12 slide 13
difference = ceosal$salary - new_salary.badhat
lm6c_1 = lm(res6b ~ 0 + difference)
summary(lm6c_1)

#no lm6c = (res6b ~ salary.badhat, data=ceosal)
#part d

rsq <- sum(res6b)
y<-sum(ceosal$salary)
ybar<-mean(ceosal$salary)

1-((rsq^2)/(y-ybar)^2)

#Question8

cell_sub<-data.frame(Hmwk_2_Exercise_6_Cell_phone_subscriptions_1_)
lm8a = lm(numberofdeaths~cell_subscription + population+ total_miles_driven, data=cell_sub)
summary(lm8a)

lm8b = lm(numberofdeaths ~ cell_ban + text_ban+ cell_subscription + population+ total_miles_driven, data=cell_sub)
summary(lm8b)

lm8c = lm(numberofdeaths ~ cell_ban + text_ban + population + cell_ban * population + text_ban*population  + cell_subscription + total_miles_driven, data=cell_sub)
summary(lm8c)
lnumberofdeaths <- log(cell_sub$numberofdeaths)
  lpop <- log(cell_sub$population)
lm8c = lm(lnumberofdeaths ~ cell_ban + text_ban + lpop + cell_ban * lpop + text_ban * lpop  + cell_subscription + total_miles_driven, data=cell_sub)
  summary(lm8c)  
  
-1.1+0.06127
  
lm8c = lm(numberofdeaths ~ cell_ban + text_ban + population + cell_ban * population + text_ban*population  + cell_subscription + total_miles_driven, data=cell_sub)
summary(lm8c)

lm8d = lm(numberofdeaths ~ cell_ban + text_ban+ cell_subscription + population+ total_miles_driven + total_miles_driven*cell_ban + total_miles_driven*text_ban, data=cell_sub)
summary(lm8d)
#Effect of cellphone ban for California 
-20.09 -0.001462*326271.651
#Effect of cellphone ban for Wyoming: 
-20.09 -0.001462*9270.994
#Effect of text ban for California : 
21.83 -0.002925*326271.651
#Effect of text ban for Wyoming : 
21.83 -0.002925*9270.994

#Question 9
speeding <- Hmwk_2_Exercise_5_Speeding_tickets
speeding[is.na(speeding)]=0
summary(speeding)

#a
lm9a = lm(Amount ~ Female, data = speeding)
summary(lm9a)
#b
lm9b = lm(Amount ~ Female + Age + MPHover, data = speeding)
summary(lm9b)
#c
lm9c = lm(Amount ~ Female + Age + MPHover + Black + Hispanic, data = speeding )
summary(lm9c)
