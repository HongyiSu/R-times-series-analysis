mydata = read.csv("Covid19April13.csv") 
#Create a daily object
inds <- seq(as.Date("2020-03-05"), as.Date("2020-04-13"), by = "day")
covid19.ts=ts(mydata[,2],start = c(2020,as.numeric(format(inds[1],"%j"))),frequency = 365)
#covid19.ts = ts(mydata[,2],frequency = 365,start=c(2020,3,5))
plot(covid19.ts)
log19 = log(covid19.ts) #transform data to log
#Fit an ARIMA model to St
par(mfrow=c(3,1))
plot.ts(log19,lwd=1.5)
acf(log19,lag.max = 50,lwd=1.5)
#ACF decay slowly, indicating non-stationary
pacf(log19,lag.max = 50,lwd=1.5)

diflog19 =diff(log19)
par(mfrow=c(3,1))
plot.ts(diflog19)
acf(diflog19)
pacf(diflog19)
#ACF:lag=3 siginificant,PACF lag=3 significant
#q <=3, p<=3
install.packages("aTSA")
library(aTSA)
adf.test(diflog19) # can not reject H0, it is not stationary

diflog19_2 = diff(diflog19) #differencing again until it becomes stationary
#install.packages("aTSA")
#library(aTSA)
adf.test(diflog19_2) 
# reject H0, and we claim now it is stationary process
par(mfrow=c(3,1))
plot.ts(diflog19_2)
acf(diflog19_2)
pacf(diflog19_2)
#guess: p<=2 q<=4
#find lowest value for AIC and BIC
AIC1=BIC1=matrix(0,4,4) #create a 4 by 4 matrix
for (i in 1:4){for (j in 1:4){
  fit=arima(diflog19_2, order = c(i-1,2,j-1))
  AIC1[i,j]=AIC(fit);
  BIC1[i,j]=BIC(fit);        }}
AIC1
BIC1
#extract lowest AIC,BIC value
#ARIMA(2,2,2)
idxminA=which(AIC1 == min(AIC1), arr.ind = TRUE)
idxminA#suggest ARIMA(2,2,3)
idxminB=which(BIC1 == min(BIC1), arr.ind = TRUE)
idxminB#suggest ARIMA(0,2,3)

fit=arima(log19, order = c(2,2,3),method="ML")
library(lmtest) # install.packages("lmtest")
coeftest(fit)
#this model is not valid, ar1 is not significant
tsdiag(fit)

fit1=arima(log19, order = c(0,2,3),method="ML")
library(lmtest) # install.packages("lmtest")
coeftest(fit1)
#this model is not valid, ma2 is not significant
tsdiag(fit1)

fit2=arima(log19, order = c(0,2,2),method="ML")
library(lmtest) # install.packages("lmtest")
coeftest(fit2)
#this model is not valid, ma2 is not significant
tsdiag(fit2)

fit3=arima(log19, order = c(0,2,1),method="ML")
library(lmtest) # install.packages("lmtest")
coeftest(fit3)
#this model is valid, ma1 is significant, we can accept this model
tsdiag(fit3)

library(forecast)
plot(forecast(fit3,h=10))
#a time series model for the (log-) 
#total number of confirmed cases of COVID-19 in Alberta
pred=predict(fit,n.ahead=50)
pred
#extract predict logrithm of covid19 cases in next 10 days
logcovid = c(7.6156574, 7.8686940, 8.1244625, 8.3829344, 8.6440820,8.9078785, 9.1742976, 9.4433140,9.7149032, 9.9890410)
covid_pred =  exp(logcovid)
#prediction
covid_pred #the number of covid19 cases prediction in next 10 days in Albert
#conclusion:The model, ARIMA(0,2,2), predicts that if we don't do anything
#the number of comfirmed covid19 in Alberta will surge to 21786 cases in next 10 days.

