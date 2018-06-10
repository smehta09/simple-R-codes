#linear regression
print("data for speed")

x<-c(4,4,7,7,8,9,10,10,10,10,11)

print("data of distance")

y<-c(2,10,4,22,16,10,18,26,34,17)

print("relationship model") 
relation<-lm(dist~speed, data=cars) 
relation


print(relation)


m<-summary(relation)$coefficients



m[1,1]

m<-coefficients(relation) m<-relation$coefficients


str(m)

a<-data.frame(speed=55)
print(a)
result<-predict(relation,a)
result
