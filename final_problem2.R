mydata = read.csv("SalesLead.csv")  # read csv file 
#St represent the annualy sales data
#Lt be the leading indicator
St.ts = ts(mydata[,1],frequency=1,start=1870,end=2019)
Lt.ts = ts(mydata[,2],frequency=1,start=1870,end=2019)

#(a)
plot.ts(St.ts)
logst = log(St.ts) #transform data to log
#Fit an ARIMA model to St
par(mfrow=c(3,1))
plot(logst)
acf(logst)
pacf(logst)
#ACF plot decay slow, suggest non-staionarity 
diff.logst=diff(logst,lag=1) #remove trend by differencing
install.packages("aTSA")
library(aTSA)
adf.test(diff.logst) 
# NONE OF THE THREE TYPE, P-value<0.05, claim that now it is stationary
par(mfrow=c(3,1))
plot(diff.logst)
acf(diff.logst)
pacf(diff.logst)
#intial guess based on ACF and PACF plots above q<=4 p<=2

#model fitting and residual diagnostics
#(p,1,q)
fit1=arima(diff.logst, order=c(1,1,2), seasonal=list(order=c(0,0,0), period=1))
coeftest(fit1)
#valid, ar1 ma1 and ma2 both significant
tsdiag(fit1,lwd=1.5)
print(AIC(fit1))
fit2=arima(diff.logst, order=c(1,1,1), seasonal=list(order=c(0,0,0), period=1))
coeftest(fit2)
print(AIC(fit2))
#invalid, ar1 ma1 and ma2 both significant
fit3=arima(diff.logst, order=c(1,1,3), seasonal=list(order=c(0,0,0), period=1))
coeftest(fit3)
print(AIC(fit3))
#invalid, ma1 and ma3 both significant


#AIC for fit1 is -1087.186, it is the smallest value among the three
#tsdiag() test for fit1 suggest that the residual is white noise, it is good.
#from coeftest() test, we know this model is valid, parameters are all significant
#thus ARMA(1,1,2) model can be used to model this time series


#(b)
d_1St.ts = diff(St.ts) #(1-B)St
d_1Lt.ts = diff(Lt.ts) #(1-B)Lt
ccf_3= ccf(d_1St.ts,d_1Lt.ts,lag=3)
ccf_3
#correlations between ∆St and ∆Lt:-0.003
#correlations between ∆St and ∆Lt-1:0.071
#correlations between ∆St and ∆Lt-2:-0.380
#correlations between ∆St and ∆Lt-3: 0.720, this is significant, suggest correlation between these two

crosscor = ccf(d_1St.ts,d_1Lt.ts, main = "corss-correlation of (1-B)St and (1-B)Lt") # estimated corss-correlation
#a regression of ∆St on ∆Lt−3 is reasonable 
#because correlation between ∆St and ∆Lt−3 is large
#meaning they have a strongly positive linear relationship

#(c)
dSt = d_1St.ts[1:146]
lag3Lt = d_1St.ts[4:149]
#fit a regression model 
regr1 <- lm(dSt~lag3Lt)
summary(regr1)
#Coefficients is significant
#Adjusted R-squared:  0.04535 
resid=regr1$residuals  
par(mfrow=c(3,1)) 
acf(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5)
pacf(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5)
plot(resid,main="Residuals",lwd=2,cex.lab=1,cex.axis=1.5)#cyclical pattern
#guess: q<=4 p<=4 (based on ACF and PACF plots)
install.packages("orcutt")
library(orcutt)
coch=cochrane.orcutt(regr1)
summary(coch)
#DW test p-value:7.214e-01, greater than 0.05, suggest AR(1) is not enough
par(mfrow=c(2,1))
acf(resid(coch),lwd=2,cex.lab=1,cex.axis=1.5);
pacf(resid(coch),lwd=2,cex.lab=1,cex.axis=1.5)
#guess: q<=4 p<=4 (based on ACF and PACF plots)
#Residual analysis from the the Cochrane-Orcutt procedure

install.packages("astsa")
library(astsa)
adjreg1_0 = sarima (dSt, 4,0,4, xreg=lag3Lt)
adjreg1_0$ttable
adjreg2_0 = sarima (dSt, 3,0,4, xreg=lag3Lt)
adjreg2_0$ttable
adjreg3_0 = sarima (dSt, 2,0,4, xreg=lag3Lt)
adjreg3_0$ttable
adjreg4_0 = sarima (dSt, 1,0,4, xreg=lag3Lt)
adjreg4_0$ttable
adjreg60 = sarima (dSt, 4,0,3, xreg=lag3Lt)
adjreg60$ttable
adjreg70 = sarima (dSt, 3,0,3, xreg=lag3Lt)
adjreg70$ttable
adjreg80 = sarima (dSt, 2,0,3, xreg=lag3Lt)
adjreg80$ttable
adjreg90 = sarima (dSt, 1,0,3, xreg=lag3Lt)
adjreg90$ttable
adjreg110 = sarima (dSt, 4,0,2, xreg=lag3Lt)
adjreg110$ttable
adjreg120 = sarima (dSt, 3,0,2, xreg=lag3Lt)
adjreg120$ttable
adjreg130 = sarima (dSt, 2,0,2, xreg=lag3Lt)
adjreg130$ttable
adjreg140 = sarima (dSt, 1,0,2, xreg=lag3Lt)
adjreg140$ttable
adjreg160 = sarima (dSt, 4,0,1, xreg=lag3Lt)
adjreg160$ttable
adjreg170 = sarima (dSt, 3,0,1, xreg=lag3Lt)
adjreg170$ttable
adjreg180 = sarima (dSt, 2,0,1, xreg=lag3Lt)
adjreg180$ttable
adjreg190 = sarima (dSt, 1,0,1, xreg=lag3Lt)
adjreg190$ttable

adjreg1 = sarima (dSt, 4,1,4, xreg=lag3Lt)
adjreg1$ttable
adjreg2 = sarima (dSt, 3,1,4, xreg=lag3Lt)
adjreg2$ttable
adjreg3 = sarima (dSt, 2,1,4, xreg=lag3Lt)
adjreg3$ttable
adjreg4 = sarima (dSt, 1,1,4, xreg=lag3Lt)
adjreg4$ttable
adjreg6 = sarima (dSt, 4,1,3, xreg=lag3Lt)
adjreg6$ttable
adjreg7 = sarima (dSt, 3,1,3, xreg=lag3Lt)
adjreg7$ttable
adjreg8 = sarima (dSt, 2,1,3, xreg=lag3Lt)#accept
adjreg8$ttable
adjreg9 = sarima (dSt, 1,1,3, xreg=lag3Lt)
adjreg9$ttable
adjreg11 = sarima (dSt, 4,1,2, xreg=lag3Lt)
adjreg11$ttable
adjreg12 = sarima (dSt, 3,1,2, xreg=lag3Lt)
adjreg12$ttable
adjreg13 = sarima (dSt, 2,1,2, xreg=lag3Lt)
adjreg13$ttable
adjreg14 = sarima (dSt, 1,1,2, xreg=lag3Lt)
adjreg14$ttable
adjreg16 = sarima (dSt, 4,1,1, xreg=lag3Lt)
adjreg16$ttable
adjreg17 = sarima (dSt, 3,1,1, xreg=lag3Lt)
adjreg17$ttable
adjreg18 = sarima (dSt, 2,1,1, xreg=lag3Lt)
adjreg18$ttable
adjreg19 = sarima (dSt, 1,1,1, xreg=lag3Lt)
adjreg19$ttable


#only one model accpected, because all other models have at least one parameter's P-value > 0.05
#it is ARIMA(2,1,3)
par(mfrow=c(3,1))
plot.ts(resid(adjreg8$fit),lwd=2)
acf(resid(adjreg8$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
pacf(resid(adjreg8$fit),lwd=2,cex.lab=1,cex.axis=1.5,ylim=c(-1,1))
#OKAY
AIC(adjreg8$fit)
BIC(adjreg8$fit)

