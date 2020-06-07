ct = c(2317907, 713314.7, 1165570, 198735, 113136,
135076.1, 53068.57, 25555.28, 11174.05, 14395.05,
3118.826, 1772.714, 809.2468, 2113.465, 278.6778,
223.4281, 164.6113, 64.1501, 34.05971, 22.26015,
10.55634, 4.836766, 1.728635, 4.15825, 0.8075457,
0.5317843, 0.1987265, 0.2262507, 0.1204367)
#(a)
ct.ts = ts(ct,frequency = 1,start=1980,end = 2008)#create a time series
plot(ct.ts) #plot ct
#I see the ct versus time plot is a curved line, meaning non-linear relationship
#and it has large value in the beginning, then become stable later

#(b)
logct = log(ct)
logct.ts = ts(logct,frequency = 1,start=1980,end = 2008)
plot(logct.ts)
#now the plot appears with a linear shape
tim = time(logct.ts) #grab t from the time series
year = 1:length(tim)
fit=lm(year~logct.ts) #fit logct on t
summary(fit)#Adjusted R-squared:  0.9929 
# the curve ct versus t behaves like ct ≈ α*exp(βt) because the correlation is 0.9929,
# suggest a strong linear relationship bewteen time and log(ct)
# comment for the plot: it is a negative linear correlation


# (c) 
resid=fit$residuals
par(mfrow=c(3,1))
acf(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5);
pacf(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5);
plot(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5);
#This fit uses OLS method, which is the best when the errors are uncorrelated
#we can see from the ACF, the value at lag 2 is significant
#from the PACF plot, the value at lag 1, lag 2 and lag 3 are significant
#from the residual versus t plot, we can see a cyclical pattern
#thus the error is not White Noise, the errors are likly correlated 




#(d)
install.packages("orcutt")
library(orcutt)
coch=cochrane.orcutt(fit) #fit here obtained by OLS
summary(coch)
#Estimate Std. Error t value  Pr(>|t|)    
#(Intercept) 24.970828   0.133624 186.873 < 2.2e-16 ***
#  logct.ts    -1.661048   0.017508 -94.873 < 2.2e-16 ***

## the parameters are significant

#Durbin-Watson statistic 
#(original):    2.83326 , p-value: 9.853e-01
#(transformed): 2.25450 , p-value: 6.833e-01


#p-value from DW test are not significant, suggest that not AR(1) model is a good choice

print(coch$rho) #-0.4468697
par(mfrow=c(2,1))
acf(resid(coch),lwd=2,cex.lab=1,cex.axis=1.5);
pacf(resid(coch),lwd=2,cex.lab=1,cex.axis=1.5)

#from ACF q<=2 and from PACF p<=3


install.packages("astsa")
library(astsa)
adjreg1 = sarima (logct.ts, 3,0,2, xreg=year)
adjreg1$ttable
adjreg2 = sarima (logct.ts, 2,0,2, xreg=year)
adjreg2$ttable
adjreg3 = sarima (logct.ts, 1,0,2, xreg=year)
adjreg3$ttable
adjreg4 = sarima (logct.ts, 3,0,1, xreg=year)
adjreg4$ttable
adjreg5 = sarima (logct.ts, 2,0,1, xreg=year)
adjreg5$ttable
adjreg6 = sarima (logct.ts, 1,0,1, xreg=year)
adjreg6$ttable
adjreg7 = sarima (logct.ts, 1,0,0, xreg=year)#accept
adjreg7$ttable
adjreg8 = sarima (logct.ts, 2,0,0, xreg=year)#accept
adjreg8$ttable
adjreg9 = sarima (logct.ts, 3,0,0, xreg=year)#accept
adjreg9$ttable
adjreg10 = sarima (logct.ts, 0,0,2, xreg=year)
adjreg10$ttable
adjreg11 = sarima (logct.ts, 0,0,1, xreg=year)#accept
adjreg11$ttable

#there are only 4 models accpected for the residual 
#they are AR(1) AR(2) AR(3) MA(1) 


par(mfrow=c(3,1))
plot.ts(resid(adjreg7$fit),lwd=2)
acf(resid(adjreg7$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg7$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
#not enough, ACF at lag 3, PACF at lag 2 are significant
par(mfrow=c(3,1))
plot.ts(resid(adjreg8$fit),lwd=2)
acf(resid(adjreg8$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg8$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
#OKAY

par(mfrow=c(3,1))
plot.ts(resid(adjreg9$fit),lwd=2)
acf(resid(adjreg9$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg9$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
#OKAY
par(mfrow=c(3,1))
plot.ts(resid(adjreg11$fit),lwd=2)
acf(resid(adjreg11$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg11$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
#OKAY

AIC(adjreg11$fit)
BIC(adjreg11$fit)
AIC(adjreg9$fit)
BIC(adjreg9$fit)
AIC(adjreg8$fit)
BIC(adjreg8$fit)

#by comparing AIC and BIC value, the smallest one is MA(1)
#therefore, I will use MA(1) model for the error estimatinon.

par(mfrow=c(3,1))
plot.ts(resid(adjreg11$fit),lwd=2)
acf(resid(adjreg11$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg11$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))

