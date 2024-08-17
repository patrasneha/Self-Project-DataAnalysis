#---Assignment3-----

#---1----
#--install.packages("sandwich")
library(sandwich)
data(PublicSchools)
View(PublicSchools)

#--(a)---
PS=na.omit(PublicSchools)
View(PS)
#---(b)----
PS$Income <- PS$Income * 0.0001
#--(c)---
str(PublicSchools)
plot(Expenditure~Income,data=PS,main="Scatterplot of Expenditure on Income")
lmodel=lm(Expenditure~Income,data=PS)
abline(lmodel,col="grey")
#--(d)--
#--Basic Diagonstics Plots---
plot(residuals(lmodel)~PS$Income,main="Residuals against Covariates")
abline(h=0,col="grey")
#--Residuals may depend on covariates----
qqnorm(residuals(lmodel))
qqline(residuals(lmodel))
#---(e)---
#--Identifying potential outliers---
PS.inf=influence.measures(lmodel)
PS.inf
#--Plotting with indexes---
plot(Expenditure~Income,data=PS,main="Scatterplot of Expenditure on Income")
with(PS,text(Income,Expenditure,labels=1:length(Income),pos=3))
#--Plotting Leverages---
plot(1:length(PS$Income),hatvalues(lmodel),ylab="Leverages")
with(PS,text(1:length(Income),hatvalues(lmodel),pos=3))
abline(h=2/length(PS$Income),col="grey")
#--Influential Obs. are 2,24,48---
#--(f)---
#--Fitting regression with and without influences--
plot(Expenditure~Income,data=PS,main="Scatterplot of Expenditure on Income")
PS.notinf=PS[-c(2,24,48),]
lmodel2=lm(Expenditure~Income,data=PS.notinf)
abline(lmodel,col="blue")
abline(lmodel2,lty="dashed",col="red")
#--Blue-with influences--
#--Red-not with influences---



#---2-----
#--install.packages("AER",type="binary")
library(AER)
data(Journals)
str(Journals)
#---(a)---
Journals$citeprice <- Journals$price/Journals$citations
#---(b)--
#--Regression of log-sub on log-citeprice---
lmodel3=lm(log(subs) ~ log(citeprice), data = Journals)
plot(log(subs)~log(citeprice),data = Journals)
abline(lmodel3)
#--(c)---
#--Diagonstics Plot----
plot(residuals(lmodel3)~log(Journals$citeprice),main="Residuals against Covariate")
abline(h=0,col="grey")
#--May be Heteroscedastic----
#---(d)---
#--Glejser test for heteroscedasticity---
resid.abs=abs(residuals(lmodel3))
lmodel4=lm(resid.abs~log(citeprice),data=Journals)
summary(lmodel4)
#--Null Hypothesis of homoscedasticity rejected---
#---(e)---
#--Estimating the variance---
lmodel5=lm((resid.abs^2)~log(citeprice),data=Journals)
lmodel3.wls=lm(log(subs) ~ log(citeprice), weight=1/abs(fitted(lmodel5)),data = Journals)
summary(lmodel3.wls)

#-----3--------
data(longley)
str(longley)
L=longley
#---(a)---
lmodel6=lm(Employed~Population+GNP-1,data=L)
#---(b)---
#---Diagonstic Plot---
plot(residuals(lmodel6)~fitted(lmodel6))
abline(h=0)
qqnorm(residuals(lmodel6))
qqline(residuals(lmodel6))
plot(head(residuals(lmodel6),-1),tail(residuals(lmodel6),-1))
#--Mayn't be Homoscedastic-----
#---(c)----
library(car)
durbinWatsonTest(lmodel6,reps=20000)
#--Null hypothesis for homoscedasticity rejected---
#---(d)---

#---Cochrane Orcutt Method-----
#--Estimating model parametres---
#--1st Iteration----
resid=residuals(lmodel6)
rho1=sum(resid[-1]*resid[-length(lmodel6)])/sum(resid[-length(resid)]^2)	#--Initial Value-----
y1=with(L,Employed[-1]-rho1*Employed[-length(Employed)])
x1=with(L,Population[-1]-rho1*Population[-length(Population)])
x2=with(L,GNP[-1]-rho1*GNP[-length(GNP)])
mod1=lm(y1~x1+x2-1)
beta1=coef(mod1)
y2=with(L,head(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
x3=with(L,tail(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
mod2=lm(y2~x3-1)
rho2=coef(mod2)
y1=with(L,Employed[-1]-rho2*Employed[-length(Employed)])
x1=with(L,Population[-1]-rho2*Population[-length(Population)])
x2=with(L,GNP[-1]-rho2*GNP[-length(GNP)])
mod1=lm(y1~x1+x2-1)
beta2=coef(mod1)

while(any(abs(rho1-rho2)>0.00001 || abs(beta1[1]-beta2[1])>0.00001 || abs(beta1[2]-beta2[2])>0.00001))
{
	beta1=beta2
	y2=with(L,head(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
	x3=with(L,tail(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
	mod2=lm(y2~x3-1)
	rho1=coef(mod2)
	y1=with(L,Employed[-1]-rho1*Employed[-length(Employed)])
	x1=with(L,Population[-1]-rho1*Population[-length(Population)])
	x2=with(L,GNP[-1]-rho1*GNP[-length(GNP)])
	mod1=lm(y1~x1+x2-1)
	beta1=coef(mod1)
	y2=with(L,head(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
	x3=with(L,tail(Employed-beta1[1]*Population-beta1[2]*GNP,-1))
	mod2=lm(y2~x3-1)
	rho2=coef(mod2)
	y1=with(L,Employed[-1]-rho2*Employed[-length(Employed)])
	x1=with(L,Population[-1]-rho2*Population[-length(Population)])
	x2=with(L,GNP[-1]-rho2*GNP[-length(GNP)])
	mod1=lm(y1~x1+x2-1)
	beta2=coef(mod1)
}

rho=rho1
beta=beta1

#---4-------
#---install.packages("archdata")
library(archdata)
data(Handaxes)
View(Handaxes)
str(Handaxes)
#--(a)---
summary(Handaxes)
H=Handaxes[,-1]
boxplot(H)
#---(b)-----
cor(H)
pairs(H)
#--Covariates are highly correlated among others----

#---(c)-----
log.H=log(H,base=exp(1))
#--(d)--
max(cor(log.H[,-1])-diag(1,6))
which(cor(log.H[,-1])==max(cor(log.H[,-1])-diag(1,6)))
#maximum cor among B,B2--

lmodel7=lm(L~.,data=log.H)
lmodel8=lm(L~.,data=log.H[,-c(3,5)])

#---(e)-----
srows=sample(1:nrow(log.H),size=550,replace=F)
log.H=log.H[,-c(3,5)]
train.log.H=log.H[srows,]
test.log.H=log.H[-srows,]
#---(f)---
lmodel9=lm(L~.,data=train.log.H)
#---(g)---
summary(lmodel9)
#---(h)---
lmodel10=lm(L~.,data=train.log.H[,-5])
#---(i)----
summary(lmodel10)
#---(j)----
#----(k)-----
library(car)
covmodel=hccm(lmodel10)
lmodel10.w=lm(solve(covmodel)%*%L~solve(covmodel)%*%.,data=train.log.H[,-5])
#----(l)-----
influence.measures(lmodel10)
rlmodel=rlm(L~.,data=train.log.H[,-5])
#---(m)-----
dwt(lmodel10)
#---(n)----