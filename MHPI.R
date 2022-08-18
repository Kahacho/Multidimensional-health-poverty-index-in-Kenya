rm(list = ls()); graphics.off()

#Packages
library(ggplot2)
library(survival)
library(survminer)
library(broom)
library(knitr)

#Example 1: Single group
head(lung)

#Basic Plot
fit1 <-survfit(Surv(time, status)~1, 
               data=lung)
fit1
plot(fit1, xlab="Days", ylab="Overall survival probability")

#Nicer Plot
fit2 <- ggsurvplot(
  fit = survfit(Surv(time, status)~ 1, data=lung),
  xlab="Days",
  ylab="Overall survival probability",
  palette ="orange"
)
fit2

#Median survival time
fit1 #310 days

#Survival time based on years
summary(fit1, times=182.625) #six months
summary(fit1, times=365.25) #one year
summary(fit1, times=730.50) #two years
#-------------------------------------------------------------------------------


#Example 2: Two groups
head(rats)
names(rats)

#Basic Plot
fit3 <- survfit(Surv(time, status)~1, data = rats)
plot(fit3, xlab="Days", ylab="Tumor-free progression")

#Males and Female Plot
surv_obj1 <- Surv(time=rats$time, event=rats$status)
fit4 <-survfit(surv_obj1 ~ sex, data=rats)
ggsurvplot(fit4, data=rats, pval=TRUE)

#Treatment and Control Plot -> females only
f.rats <-rats[rats$sex=="f",]
surv_obj2 <- Surv(time=f.rats$time, event=f.rats$status)
fit5 <-survfit(surv_obj2 ~ rx, data=f.rats)
fit5
ggsurvplot(fit5, data=f.rats, pval=TRUE, conf.int=TRUE,
           xlab="Days",
           ylab="Tumor-free progression",)

#Rank test
survdiff(surv_obj2~rx, data=f.rats)

#Hazards ratio
fit6 <- coxph(surv_obj2~ rx, data=f.rats)
broom::tidy(fit6, exp=TRUE) %>% kable() #around 2.5 times as many with drug are getting tumor as those with control
#Here in brooom, the estimate is the hazard ratio
