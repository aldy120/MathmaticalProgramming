# Use accept-reject method
# thePDF=0.3*N(-2.5,1)+0.7*N(2.5,1)
thePDF=function(x)
{
  result=0.3*dnorm(x,-2.5,1)+0.7*dnorm(x,2.5,1)
  return(result)
}
f=function(x)
{
  return(-thePDF(x))
}
thePDF.max=-optimize(f,interval = c(-6,6))$obj
curve(thePDF,-6,6)
Nsim=100000
U=runif(Nsim,0,1)
Y=runif(Nsim,-6,6)
X=Y[U<thePDF(Y)/thePDF.max]
hist(X,breaks=50,col=rainbow(150),probability = TRUE)
curve(thePDF(x),-6,6,add=TRUE)
