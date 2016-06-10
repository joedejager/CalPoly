## 2SLS in R ##

library(foreign)

AllData <- read.dta("newdail2002v2.dta",  convert.factors=FALSE)
CampaignData <- na.omit(subset(AllData, select=c(votes1st,incumb,spend_regular,spend_regularXinc,spend_public,partyquota1997,electoraK,dublin,senator,councillor,wonseat)))

## OLS ##

olsmodel <- lm(votes1st ~ incumb + spend_regular + spend_regularXinc, data=CampaignData)
summary(olsmodel)

olsmodel2 <- lm(votes1st ~ incumb + spend_regular + spend_regularXinc + spend_public, data=CampaignData)
summary(olsmodel2)

## 2SLS by hand -- this replicates models 1 and 2, Table 3 in Benoit and Laver 2008

first.stage.1 <- lm(spend_regular ~ partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(first.stage.1)

CampaignData$instrumented.spending <- first.stage.1$fitted.values
CampaignData$inst.spend.inc <- CampaignData$instrumented.spending * CampaignData$incumb

second.stage.1 <- lm(votes1st ~ incumb + instrumented.spending + inst.spend.inc, data=CampaignData)
summary(second.stage.1)


first.stage.2 <- lm(spend_public ~ partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(first.stage.2)

CampaignData$instrumented.pspending <- first.stage.2$fitted.values

second.stage.2 <- lm(votes1st ~ incumb + instrumented.spending + inst.spend.inc + instrumented.pspending, data=CampaignData)
summary(second.stage.2)


## Issue to consider -- is interaction also endogenous?
## Example of difference in standard errors

second.stage.1v2 <- lm(votes1st ~ instrumented.spending, data=CampaignData)
summary(second.stage.1v2)

## 2SLS ##
## compare standard errors 

install.packages("AER")
library(AER)

tslsmodel <- ivreg(votes1st  ~ spend_regular | partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(tslsmodel)





## Complete models with 2SLS
## note incumbency is also an instrument

tslsmodel.1 <- ivreg(votes1st  ~ incumb + spend_regular + spend_regularXinc | incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(tslsmodel.1)

tslsmodel.2 <- ivreg(votes1st  ~ incumb + spend_regular + spend_regularXinc + spend_public | incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(tslsmodel.2)


## An alternate package 

install.packages("sem")
library(sem)

tslsmodel.3 <- tsls(votes1st  ~ incumb + spend_regular + spend_regularXinc, ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(tslsmodel.3)

tslsmodel.4 <- tsls(votes1st  ~ incumb + spend_regular + spend_regularXinc + spend_public , ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
summary(tslsmodel.4)


## 2 stage estimator with a binary dv

probit.model <- glm(wonseat ~ incumb + spend_regular + spend_regularXinc + spend_public, family=binomial(link=probit), data=CampaignData)
summary(probit.model)


## (sort of) replication of table 5

tspmodel.1 <- glm(wonseat ~ incumb + instrumented.spending + inst.spend.inc + instrumented.pspending, family=binomial(link=probit), data=CampaignData)
summary(tspmodel.1)

## 2SCML

reg.inst1 <- lm(spend_regular ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
reg.inst2 <- lm(spend_regularXinc ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)
reg.inst3 <- lm(spend_public ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=CampaignData)

CampaignData$inst1 <- reg.inst1$residuals
CampaignData$inst2 <- reg.inst2$residuals
CampaignData$inst3 <- reg.inst3$residuals

tscml.model <- glm(wonseat ~ incumb + spend_regular + spend_regularXinc + spend_public + inst1 + inst2 + inst3, family=binomial(link=probit), data=CampaignData)
summary(tscml.model)

## test for endogeneity

install.packages("lmtest")
library(lmtest)

lrtest(probit.model, tscml.model)

