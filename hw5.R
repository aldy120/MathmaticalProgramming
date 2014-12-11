# Students' height
# n=10 n=40 
# prior: N(160,1)
# observation: N(190,1)
par(mfrow = c(2,2))
logLikelihood=function(m)
{
  f0=function(m.i)# for each m.i
  {
    y.i=sum(dnorm(x = studentsHeight,mean = m.i,sd = 1,log=TRUE))
    return(y.i)# log(pdf(x.1)*pdf(x.2)*...) 
  }
  y=sapply(m,f0)
  return(y)
}
lpri=function(m)# log prior
{
  y=dnorm(m,160,1,log=TRUE)
  return(y)
}
lpos=function(m)# log posterior
{
  y=logLikelihood(m)+lpri(m) # log(ab)=log(a)+log(b)
  return(y)
}
n=10
studentsHeight=rnorm(n,190,1)
ans=optim(par = 0,fn = logLikelihood,method="BFGS",control=list("fnscale"=-1))
m.hat=ans$par# max m of logLikelihood(m)
ans=optim(0,lpos,method="BFGS",control=list("fnscale"=-1))
m.map=ans$par

curve(logLikelihood,180,200)
abline(v=m.hat,lty=2)
curve(lpos,180,200)
abline(v=c(m.hat,m.map),lty=c(2,1))

n=40
studentsHeight=rnorm(n,190,1)
ans=optim(par = 0,fn = logLikelihood,method="BFGS",control=list("fnscale"=-1))
m.hat=ans$par# max m of logLikelihood(m)
ans=optim(0,lpos,method="BFGS",control=list("fnscale"=-1))
m.map=ans$par

curve(logLikelihood,180,200)
abline(v=m.hat,lty=2)
curve(lpos,180,200)
abline(v=c(m.hat,m.map),lty=c(2,1))
