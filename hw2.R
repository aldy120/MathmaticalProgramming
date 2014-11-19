#theFunction(x)=1-exp(-x)
par(mfrow = c(1,2))

InverseOfTheFunction=function(x)# inverse funtion
{
  y=-log(1-x)
  return(y)
}

DerivativeOfTheFunction=function(x)
{
  y=exp(-x)# derivative
  return(y)
}

x1=runif(10000)# 0~1
y1=InverseOfTheFunction(x1)
hist(y1,breaks=50,col="steelblue")

x2=seq(0.05,10,0.01)
y2=DerivativeOfTheFunction(x2)
plot(x2,y2,"l")

