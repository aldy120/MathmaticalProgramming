#Using Old Faithful Geyser Data
attach(faithful)
Fit=lm(eruptions~waiting)
err=Fit$res
B=array(0,dim=c(1000,2))
for(i in 1:1000)
{
  eruptions1=eruptions+sample(err,replace=T)
  B[i,]=lm(eruptions1~waiting)$coe
}
par(mfrow=c(1,2))
hist(B[,1],breaks=50,xlab="intercept",col="red",main="")
hist(B[,2],breaks=50,xlab="intercept",col="red",main="")
mean(B[,1])
mean(B[,2])
sd(B[,1])
sd(B[,2])
quantile(B[,1],c(0.025,0.975))
quantile(B[,2],c(0.025,0.975))
