#Question 2:
#a-c (conceptual questions)
#d 
#install plm package and stargazer package
library(plm)
library(stargazer)

library(haven)
job_train_scrap <- read_dta("ECON861_Baier/Final_exam/job_train_scrap.dta")
View(job_train_scrap)

JTS.p <- pdata.frame(job_train_scrap,index=c("fcode","year"))
JTS_p <-plm(lscrap~factor(d88)+factor(d89)+grant+grant_1,data=JTS.p,model="pooling")
summary(JTS_p)

JTS_fd <- plm(lscrap~factor(d89)+grant+grant_1,data=JTS.p,model="fd")

summary(JTS_fd)

JTS_fe <- plm(lscrap~factor(d88)+factor(d89)+grant+grant_1,data=JTS.p,model="within")

summary(JTS_fe)


stargazer(JTS_p,JTS_fd,JTS_fe, type="text",
          column.labels = c("Pooled","FD","FE"),
          keep.stat=c("n","rsq"),
          keep =c("grant","grant_1","d88","d89"))

#Question 3
library(stargazer) # create tables to compare results

library(haven)
ath_gpa <- read_dta("ECON861_Baier/Final_exam/ath_gpa.dta")
View(ath_gpa)

#a. (question about intuition)
#b. there are two ways of computing that: plm or lm . Both yield same estimates
# By plm 
library(plm)
#generate panel data frame:
ath_gpa.p <- pdata.frame(ath_gpa,index=c("id","term"))
pdim(ath_gpa.p) # Provides details of panel (N,T, balanced, N*T)'
summary(plm( trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = ath_gpa.p, model= "pooling"))
# Or, by lm:
summary(lm( trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = ath_gpa))

#c (football players): some investigation that supports argument
#Including football as a covariate shows that its coefficient is positive and stat sign
# It makes season coef stat sign and of higher magnitude
# Computing by plm:
summary(plm( trmgpa ~ football + season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = ath_gpa.p, model= "pooling"))
#or by lm
summary(lm( trmgpa~ football + season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = ath_gpa))
# To make a table to compare both :
a<- plm( trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa + season, data = ath_gpa.p, model= "pooling")
b <- plm( trmgpa ~ football + season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa + season, data = ath_gpa.p, model= "pooling")

stargazer(a, b, type="text",
          column.labels = c("Without football","With footbal"),
          keep.stat=c("n","rsq"),
          keep =c("football","trmgpa","spring","sat","hsperc","frstsem","tothrs","female","black","white","crsgpa","season")
          )
# Or, I might create two subsets of the data and compare:
n_football <- subset(ath_gpa.p, ath_gpa.p$football==0)
football <- subset(ath_gpa.p, ath_gpa.p$football==1)
# running lm on subsets of data:
library(plm)
summary(plm( trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = n_football))
summary(plm( trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + black + white + crsgpa, data = football))
# See the football
mean(football$female)
View(football)

summary(plm(trmgpa ~ season + spring + sat + hsperc + frstsem + tothrs + female + black + white + crsgpa, data = ath_gpa.p, model="fd"))

#part d:
#summary(lm( ctrmgpa ~ cseason + spring + sat + hsperc + frstsem + ctothrs + female + black + white + ccrsgpa, data = ath_gpa))
#or:
#summary(plm( ctrmgpa ~ cseason + spring + sat + hsperc + frstsem + ctothrs + female + black + white + ccrsgpa, data = ath_gpa.p))
#summary(lm( ctrmgpa ~ cseason + spring + sat + hsperc + frstsem + ctothrs + female + black + white + ccrsgpa, data = ath_gpa))
ath_gpa.p$cfrstsem <- diff(ath_gpa.p$frstsem, lag = 1, differences = 1)
View(ath_gpa.p)
summary(lm( ctrmgpa ~ cseason + factor(cfrstsem) + ctothrs + ccrsgpa, data = ath_gpa.p))
#this didn't work:
library(plm)
summary(plm( ctrmgpa ~ cseason + factor(cfrstsem) + ctothrs + ccrsgpa, data = ath_gpa.p))
summary(plm(ctrmgpa ~ factor(frstsem) + ctothrs + ccrsgpa + cseason , data=ath_gpa.p , model = "pooling"))
summary(lm(ctrmgpa ~ factor(cfrstsem) + ctothrs + ccrsgpa + cseason , data=ath_gpa.p))

summary(lm( ctrmgpa ~ cseason + ctothrs + ccrsgpa, data = ath_gpa.p))


# Question 4:
library(haven)
catsup_class <- read_dta("ECON861_Baier/Final_exam/catsup_class.dta")
View(catsup_class)
# a. (intuition for coefficients)
# b. 
mean(catsup_class$buy_sb)
#c
library(glm)
probit <- glm( buy_sb ~ price_diff + display_sb + display_oth + coup_sb + coup_oth + hhld_size, 
                  family = binomial(link = "probit"), 
                  data = catsup_class)
summary(probit)
#d 
# Idea : 
# If prediction was >0.5, set y = 1 and compare with actual 
# If actual and y are equal => correct
# If not => incorrect 
# count correct 

#Saving predicted values (like predict r in stata)
#catsup_class$pred <- predict(probit)
catsup_class$fit_buy <- fitted(probit)
catsup_class$yes_buy <- if(catsup_class$fit_buy > 0.50, 1, 0)
table(catsup_class$yes_buy, catsup_class$buy_sb)

catsup_class$no_buy <- if(catsup_class$fit_buy < 0.50 , 1 , 0)

mean(catsup_class$yes_buy)
mean(catsup_class$buy_sb)

#"Correct purchases predictions"
correct_predictions <- if (catsup_class$yes_buy == catsup_class$buy_sb) {
  print(1)
} else {
  print(0)
}
sum(catsup_class$correct_purchases_predictions)
library(caret)
confusionMatrix(catsup_class$higher, catsup_class$buy_sb)

below <- catsup_class$pred<0.5
table(catsup_class[catsup_class$pred>0.5, "x"], catsup_class[catsup_class$buy_sb, "y"])
309/14951 
295909/299466     
3557/299466
14642/14951

(309 + 295909)/314417

#e
254059/299466     

##############

##############

#Question 5:
#a
lm5a <- lm(lfp ~ age + agesq + educ + black + kids + lhinc, data = lfp_class)
summary(lm5a)
probit5a <- glm(lfp ~ age + agesq + educ + black + kids + lhinc, data = lfp_class, family = binomial(link = "probit"))
summary(probit5a)
logit5a <- glm(lfp ~ age + agesq + educ + black + kids + lhinc, data = lfp_class, family = binomial(link = "logit"))
summary(logit5a)


# The rule of thumb:
# 
ols_coef<- coef(lm5a)
ols_coef
probit_coef <- coef(probit5a)
probit_coef
logit_coef<- coef(logit5a)
logit_coef
#logit/probit should be roughly 1.60(0.4/0.25):
lp<- logit_coef/probit_coef
lp
# logit/ols should be roughly 4 (0.4/0.1):
lo<-logit_coef/ols_coef
lo
# probit/ols should be roughly 2.25(0.25/0.1):
po<-probit_coef/ols_coef
po

library(stargazer)
stargazer(list(lm5a, probit5a, logit5a), type = "text")
stargazer(list(lp, lo, po), type = "text")

#b
library(margins)
summary(margins(probit5a, type="response", variables= "black"))

#QUESTION: IS THERE A WAY TO CALL JUST 1 VARIABLE ? 

#d
margins(probit5a)
margins(logit5a)

#e
#Calculate the AME (from his file, but we didn't use it)
ME_black <- (dnorm(probit5a$coef[1]+probit5a$coef[2]*(age)
                   +probit5a$coef[3]*(age)^2
                   + probit5a$coef[4]*(educ)+probit5a$coef[5]*(black)
                   +probit5a$coef[6]*(kids) + probit5a$coef[7]*(lhinc)))
AME <- round(mean(ME_black),4)
#(be careful: don't use agesq, but use age and then square its mean)

#e
#(be careful: don't use agesq, but use age and then square its mean)
first <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
#(Now, ADD a standard deviation on age, kids, husband, educ)

#For adding one standard deviation of age
age.model <- pnorm(probit5a$coef[1]+probit5a$coef[2]*(mean(lfp_class$age)+sd(lfp_class$age))+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
age.model-first

#For number of kids:
numberofkids <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*(mean(lfp_class$kids)+sd(lfp_class$kids))+probit5a$coef[7]*mean(lfp_class$lhinc))
numberofkids-first

#For husband income:
hub <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*(mean(lfp_class$lhinc)+sd(lfp_class$lhinc)))
hub-first

#For education:
ed <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*(mean(lfp_class$educ)+sd(lfp_class$educ))+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
ed-first

#f
first <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
#(Now, SUBTRACT a standard deviation on age, kids, husband, educ)

#For adding one standard deviation of age
age.model2 <- pnorm(probit5a$coef[1]+probit5a$coef[2]*(mean(lfp_class$age)-sd(lfp_class$age))+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
age.model2-first

#For number of kids:
numberofkids2 <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*(mean(lfp_class$kids)-sd(lfp_class$kids))+probit5a$coef[7]*mean(lfp_class$lhinc))
numberofkids2-first

#For husband income:
hub2 <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*mean(lfp_class$educ)+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*(mean(lfp_class$lhinc)-sd(lfp_class$lhinc)))
hub2-first

#For education:
ed2 <- pnorm(probit5a$coef[1]+probit5a$coef[2]*mean(lfp_class$age)+probit5a$coef[3]*mean(lfp_class$age)^2+probit5a$coef[4]*(mean(lfp_class$educ)-sd(lfp_class$educ))+probit5a$coef[5]*mean(lfp_class$black)+probit5a$coef[6]*mean(lfp_class$kids)+probit5a$coef[7]*mean(lfp_class$lhinc))
ed2-first
