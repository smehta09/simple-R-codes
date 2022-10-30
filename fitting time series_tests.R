#time series class 6

#identifying type of time series
#fitting time series and estimating parameters

getwd()

setwd("D:/IFoA exams/CS2/R_working_files")
#Eg1

data<-ts(read.table("fittingmodelEg1.csv"))
head(data)
par(mfrow=c(3,1))

plot(data,main="what model to fit?",ylab="data eg1")
acf(data,main="Sample Acf")
pacf(data,main="sample PACF")

# ARIMA(0,0,3)


#data 2

data<-ts(read.table("fittingmodelEg3.csv"))
data1<-ts(read.table("fittingmodelEg1.csv"))
data2<-ts(read.table("fittingmodelEg2.csv"))

head(data)
par(mfrow=c(3,1))

plot(data,main="what model to fit?",ylab="data eg3")
acf(data,main="Sample Acf")
pacf(data,main="sample PACF")

#data 1 and 2 turned out to be AR and MA process by seeing ACF
#and PACF cutoff in their lag

#differencing data 3 as trend observed in TS graph
d<-diff(data,lag=1,differences=1)

plot(d,main="differenced TS",ylab="data eg3")
acf(d,main="diff Sample Acf")
pacf(d,main="diff sample PACF")

#data 3 is ARIMA(p,1,q), as differencing removed trend
#try to find p and q using hit and trial method


#Fitting time series model

#arima(x,order=c(1,2,1)),p,d,q

#eg1, MA 3 FIT

arima(data1,order=c(0,0,3))
#xt= mu + b1xt-1 +b2xt-2 + b3xt-3 +et
#aic less better fit in output

#eg2

arima(data2,order=c(2,0,0))

#Xt = mu +a1(Xt-1) + a2(Xt-2) +et

ar(data2)
#to estimate mu in ar
ar(data2)$x.mean
#ar function uses yule walker equations and MOM and gives ans
#arima function fit log likelihood on parameters


#eg3

#fitting arima with d=1
arima(data,order=c(1,1,1))

# fitting d where we have done differencing 
#so fitting arma(p,q), stat data

arima(d,order=c(1,0,1))


# Testing the fit

#calculating residuals
#eg1

ma3<-arima(data1,order=c(0,0,3))
ma3$residuals
e<-ma3$residuals
# e indicated error,(et) terms and then we saw its graph
par(mfrow=c(2,1))
plot(e,main="errors of MA 3 process",ylab="residuals")
acf(e,ylab="ACF of residuals")


#tsdiag function gives residual plot directly

tsdiag(ma3)

#Ljung and box test only on errors
#H0:residuals are independent
#degree of freedom chi df m-(p+q),fitdf is p+q
#lag=5 tells R to take 5 ACFs in its calculation. Must be atleast equal to fitdf.
#fitdf=p+q which is the number to be subtracted to calculate the dof. Thus, dof for 
#this test will be 5-3=2

Box.test(e,lag=5,fitdf=3,type="Ljung")

tsdiag(ma3)

#since p value is greater than 5%, we have insuff evidence to
#reject H0, ind errors

data2=ts(data2)
ar2=arima(data2,order=c(2,0,0))
tsdiag(ar2)
Box.test(ar2$residuals,type="Ljung",lag=5,fitdf=2)
Box.test(ar2$residuals,type="Ljung",lag=10,fitdf=2)
#If we include a lot of correlation coefficients in our Ljung-Box calculation, the test statistic 
#will be more likely to follow a Chi-square distribution. However, the power of the test will 
#be lower (because the number of degrees of freedom will be higher). The converse is also 
#true. If we specify a low lag in the Box.test function, the test will be more powerful but the 
#test statistic will be less likely to come from a Chi-square distribution White Knight V


#Turning points test
#Q10 R assignment scube

#Akaike's Info criteria(aic)
#aic less model goodfit


d
#ARIMA(2,1)
arima(d,order=c(2,0,1))

#ARIMA(1,2)
arima(d,order=c(1,0,2))

#2nd model better as aic is less

#auto arima function
ARIMAfit = auto.arima(Log_data , approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
#plotting residuals
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')















