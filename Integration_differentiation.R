# differentiation in R
#using expression

f=expression(3*x^3+5*x^2+8*x+1)
D(f,"x")

#2nd order differentiation
D(D(f,"x"),"x")

#3rd order derivative
D(D(D(f,"x"),"x"),"x")

f1<-expression(3*x^3+5*y^2+8*y+1)
#partial diff wrt to x or y
D(f1,"x")
D(f1,"y")

# diff wrt x then y
D(D(f1,"x"),"y")

# integration in R
?integrate

fx<-function(x){
  x^2
  
}
fx(2)
#integrate above function

integrate(fx,3,5)
integrate(fx,3,5)$value

#exponential dist integrate
#P(X>2) = ?
fexp<-function(x){
  exp(-x)
}
integrate(fexp,2,Inf)
# OR lamda = 1
pexp(2,1,lower=FALSE)
integrate(fexp,0,Inf)

#Application of integration function
#integrate functiion can only integrate for single argument
# pareto eg in sir video





