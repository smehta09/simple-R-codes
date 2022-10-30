# CS2B reinsurance assignment

#	. How to calculate MLE for the parameters like log or exponential, 
#always for original claims x, but see if data given for insurer or reinsurer,
#compare ques Q7 and Q3 of reinsurance scube assignment.
#. If reinsurance data is given , x+M is used as x-M+M = x(original claims)


#Q1  #simulating pareto claims
rpareto=function(n,a,l)
{rp <- l*((1-runif(n))^(-1/a)-1)
rp

}
set.seed(5)
x=rpareto(10000,2,800)
#ii) prob that claims involves reinsurer
z=pmax(0,x-1500)
length(z[z>0])/length(x)

#Q2  simulating exponential claims
set.seed(2357)
x=rexp(10000,0.001)
kx=1.05*x
y=pmax(kx,kx-100)
mean(y[y>0])

#Q3
set.seed(10249)
#yclaims is insurer claims
yclaims <-pmin(rexp(10000,rgamma(1,3,runif(1,580,620))),1000)
# i) MLE of lambda
M=1000
nz=length(yclaims[yclaims==1000]) #insurer claims equal to retention M
nz
length(yclaims)
yclaims.excl1000=yclaims[yclaims<1000]#insurer claims not equal to retention M
length(yclaims.excl1000)
#negative log likelihood function and nlm to maximize
flnL<-function(parameter){
 -nz*pexp(M,parameter,lower.tail=FALSE,log.p=TRUE)
  -sum(dexp(yclaims.excl1000,parameter,TRUE))

}
p=1
nlm(flnL,p)
#ii) plot of fitted distribution against emperical density
lambda= nlm(flnL,p)$estimate
lambda
density(yclaims)
plot(density(yclaims,from=100,to=1000),xlab="amounts paid by insurer",main="emperical density function vs exp(0.00272",col="blue")
x=seq(100,1000)
y=dexp(x,rate=lambda)
lines(x,y,col="red")

#Q4

#skewness of claims, gamma
pgamma(200,shape=5,rate=0.04,lower=FALSE)

#Q5
set.seed(1); x <- sort(rpois(365,rgamma(365,20,0.01)))
f=0.01*x
f2=1.04*f
nonf=(1-0.01)*x
r2=nonf+f2
x2=r2*(1-0.01*1.04)
skewx2=sum((x2-mean(x2))^3)/(length(x2)*sd(x2)^3)
skewx2
skewx3=sum((x2-mean(x2))^3)/length(x2)
skewx3^(1/3)

#Q6
set.seed(16550)
x=rexp(10000,rate=0.0005)
M=2500
#claims of insurer
y=pmin(x,M)
mean(y)
#claims of reinsurer
z=pmax(0,x-M)
mean(x-z)

#ii)
#retention level M that minimizes the variance of insurer claim net of rein
f=function(M){
z1= pmin(pmax(0,x-M),1000)
y1 = x-z1
var(y1) 
  
}

nlm(f,1000)

#Q7
#XOL

setwd("D:/IFoA exams/CS2/CS2 imp/CS2B NEW-20211224T162522Z-001/CS2B NEW")
data=read.table("Reinsurance payments.txt",header=FALSE)
head(data)
tail(data)
payments=data[,1]

M=45000
RI.claims=payments[payments>0]
head(RI.claims)
#the no of claims that fall below retention limit 45000
nm=length(payments)-length(RI.claims)
nm

#minimising log likelihood  of imported data
flogN=function(parameters){
-nm*plnorm(M,meanlog = parameters[1],sdlog=parameters[2],log.p=TRUE)
  -sum(dlnorm(RI.claims+M,meanlog=parameters[1],sdlog=parameters[2],log=TRUE))
  
}

mu=mean(log(RI.claims))
sigma=sd(log(RI.claims))
p=c(mu,sigma)
flogN(p)
nlm(flogN,p)

x=payments+M # as we are dealing with original claims and that is given for rein ie x-M, so x-M+M = x
max(x)
breakpoints <- c(20000,45000,seq(46000,86000,1000))
#histogram
hist(x,freq=FALSE,breaks=breakpoints,xlim=c(0,86000),ylim=c(0,0.00005),ylab="Density",main="Truncated data vs
     logN(10.86,0.121)",col="grey")
lines(seq(0,86000),dlnorm(seq(0:86000),meanlog=10.86,sdlog=0.121),col="blue")

#iv largest claim
max=max(RI.claims)
which(RI.claims%in%max)

x <- M+max
plnorm(x,10.475,0.265,lower.tail=FALSE)

#Q10
#pareto function and mean of insurer claim

rpareto <- function(n,a,l){
  rp <- l*((1-runif(n))^(-1/a)-1)
  rp
}
set.seed(225)
x<-rpareto(10000,3,400)
mean(x[x>100])
mean(x)


