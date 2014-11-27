#verify the central limit theorem
par(mfrow=c(1,1))
M=matrix(runif(10000*30,2,4),10000,30)# M is 1000*30 matrix
x.bar=apply(M,1,mean)# x.bar=mean of every row
s1=apply(M,1,sd)# s1=sd of every row
# 
f1=function(x)
{
  x*dunif(x,2,4)
}

f2=function(x)
{
  x^2*dunif(x,2,4)
}

mu=integrate(f1,2,4)$value# E(X)
s2=integrate(f2,2,4)$value# E(X^2)

Z=(x.bar-mu)/((s2-mu^2)^0.5/sqrt(30))
hist(Z,breaks = 50,col="gray",probability = TRUE)
curve(dnorm(x,0,1),add=TRUE)
