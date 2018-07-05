#valuation of insurance products
#interset rate
i=0.05
# discount factor
v=i/(1+i)

discount_factors<- v^(0:7)
cashflows<-c(500,400,300,rep(200,5))
length(cashflows)
#discounting cashflows
cd=cashflows*discount_factors
cd
