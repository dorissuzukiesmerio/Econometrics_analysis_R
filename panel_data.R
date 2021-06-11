  rm(list = ls()) # This line removes variables from another program 
cat("\014") # Clears the console  screen

library(plm)  # This is the package for linear panel data models
library(stargazer)

#Homework 3:
#Question 1:
library(haven)
cell <- read_dta("ECON861_Baier/Hm_3/Cell_phones.dta")
View(cell)

#generate panel data frame:
cell.p <- pdata.frame(cell,index=c("state","year"))
pdim(cell.p) # Provides details of panel (N,T, balanced, N*T)'

deaths_bmiles <- cell.p$DeathsPerBillionMiles

# a. Pooled OLS
summary(plm(deaths_bmiles ~ cell_ban + text_ban , data=cell.p , model="pooling"))

#b. State FE that causes endogeneity?
#c.Estimate with one-way FE
summary(plm(deaths_bmiles ~ cell_ban + text_ban , data=cell.p , model="within"))

#d.Year FE that causes endogeneity ? 
#e. Estimate with two-way FE
summary(plm(deaths_bmiles ~ cell_ban + text_ban ,data=cell.p,model="within", effect = "twoways"))
summary(plm(deaths_bmiles ~ cell_ban + text_ban + factor(year), data=cell.p,model="within"))

#f. With additional controls
summary(plm(deaths_bmiles ~ cell_ban + text_ban + cell_per10thous_pop + urban_percent ,data=cell.p,model="within", effect = "twoways"))
summary(plm(deaths_bmiles ~ cell_ban + text_ban + factor(year) + cell_per10thous_pop + urban_percent ,data=cell.p,model="within", effect = "twoways"))

# Question 2:
library(haven)
HOPE_scholarship <- read_dta("ECON861_Baier/Hm_3/HOPE_scholarship.dta")
View(HOPE_scholarship)
hope <- HOPE_scholarship
# a. 
summary(lm( InCollege ~ Georgia + After + AfterGeorgia , data=hope))

#b.
mean (hope$InCollege [hope$After==0 & hope$Georgia==0]) #i
mean (hope$InCollege [hope$After==1 & hope$Georgia==1]) #ii
mean (hope$InCollege [hope$After==1 & hope$Georgia==0]) #iii
mean (hope$InCollege [hope$After==0 & hope$Georgia==1]) #iv

40.57-0.44
30.05+8.93 
#c
summary(lm( InCollege ~ AfterGeorgia + factor(StateCode) + factor(Year) , data=hope))


# D. adding Age18 and Black
summary(lm( InCollege ~ AfterGeorgia + factor(StateCode) + factor (Year) + Age18 + Black, data=hope))

# E. LowIncome
Low <-subset (hope, hope$LowIncome==1)
High <- subset (hope, hope$LowIncome==0)
summary(lm (InCollege ~ AfterGeorgia + factor(StateCode) + factor(Year) + Black+ Age18, data=Low) )
summary(lm (InCollege ~ AfterGeorgia + factor(StateCode) + factor(Year) + Black+ Age18, data=High) )


# Question 3:
#load("~/ECON861_Baier/Hm_3/Teaching_Evals.RData")

library(haven)
Teaching_evals <- read_dta("ECON861_Baier/Hm_3/Teaching_evals.dta")
View(Teaching_evals)
teach <- Teaching_evals
library(plm)  # This is the package for linear panel data models

#summary(lm( Eval ~ Enrollment + factor(Required) + Apct, data=teach))
#summary( teach$Eval)

#a. 
summary(lm(Eval ~ Enrollment + Required + Apct, data=teach ))

#b
summary(teach$Year)

#c

summary(lm( Eval ~ Enrollment + Required + Apct + factor(InstrID), data=teach))
#or:
library(plm)  # This is the package for linear panel data models
#generate panel data frame:
teach.p <- pdata.frame(teach,index=c("InstrID"))
pdim(teach.p) # Provides details of panel (N,T, balanced, N*T)'
summary(plm( Eval ~ Enrollment + Required + Apct, data=teach.p , model="within"))

#d
summary(lm( Eval ~ Enrollment + Required + Apct + factor(InstrID) + factor(Year), data=teach))
