#time series class 7

#time series forcasting

#box jenkins method
#exponential smoothing

set.seed(476)
abc<-sort(rbinom(3,1,0.6),decreasing=TRUE)*c(0.86,-0.4,0.15)
abc
data<-50+arima.sim(list(ar=abc),n=1000) 
data
fit<-ar(data)
fit
fit$x.mean 
p<-predict(fit,n.ahead=20)
p$pred[10] 
plot(data,xlim=c(950,1020))
lines(p$pred,col="red")

set.seed(476)
a<-runif(1,0.6,0.8);b<-runif(1,-0.3,1)
data<-50+cumsum(arima.sim(list(ma=c(a,b)),n=1460))
data<-ts(data,start=c(2015,1),frequency=365)

d<-diff(data)

fit2<-arima(d,order=c(0,0,2))
fit2
#can enter period=12 as argument in fit2 when fiting arima if data is monthly

p2<-predict(fit2,n.ahead=180)
p2$pred

#plotting data, its predicted values , upper and lower bounds CI 

U <- p2$pred + 2*p2$se 
L <- p2$pred - 2*p2$se 

ts.plot(fit2, p2$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2)) 
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), 
       col=c(1,2,4), lty=c(1,1,2)) 


data[1460]
tail(data,1)

#adding last value of data and cumm sum of predicted value of diff
#data to obtain original time series
pwithtrend<-tail(data,1)+cumsum(p2$pred)
plot(data)
end(data)
frequency(data)
pwithtrend<-ts(pwithtrend,start=c(2019,1),frequency=365)
plot(pwithtrend)

plot(data,xlim=c(2015,2019+0.75))
lines(pwithtrend,col="red")



#exponential smoothing method

xt<-read.table("forecasting.csv")
head(xt)
tail(xt)
Xt<-ts(xt,start=c(2017,20),frequency=365)
end(Xt)
#154th day of 2018 3rd June 2018

#using Holtwinters method
HW<-HoltWinters(Xt,alpha=0.7,beta=FALSE,gamma=FALSE)
HW
# predict for 1 day ahead ie 4th June 2018
predict(HW,n.ahead=1)
predict(HW,level=0.95,prediction.interval=TRUE)


#estimate value on 8th june 2018
predict(HW,n.ahead=5)


# calculate 95% CI for the value of time series on 4th June 2018
#using optimal smoothing parameter, R is calculating aplha and CI
#itself 

HW2<-HoltWinters(Xt,beta=FALSE,gamma=FALSE)
HW2
# CI calculated as upper and lower by R
predict(HW2,level=0.95,prediction.interval=TRUE)












