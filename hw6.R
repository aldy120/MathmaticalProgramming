# Integral on multivariate normal distribution
# h=0.01,0.005,0.001
# n=2,3
f=function(x)
{
  prod(dnorm(x,0,1))
}
# n=2
# for(h in c(0.01,0.005,0.001))
# {
#   
#   a=seq(0,1,h)
#   S=as.matrix(expand.grid(a,a))
#   t=system.time(v<-sum(h^dim(S)[2]*apply(S,1,f)))
#   print(t)
#   cat("h=",h,"\tv=",v,"\n\n")
# }
dim=2
cat("n= 2\n")
for(h in c(0.01,0.005,0.001))
{
  ptm=proc.time()
  x=seq(0,1,h)
  sum=0
  for(x1 in x)
  {
    for(x2 in x)
    {
      sum=sum+h^dim*dnorm(x1)*dnorm(x2)
    } 
  }
  cat("h=",h,"\n")
  cat("timeSpan=\n")
  print(proc.time()-ptm)
  cat("v=",sum,"\n\n")
}
# n=3
# for(h in c(0.01,0.005,0.001))
# {
#   
#   a=seq(0,1,h)
#   S=as.matrix(expand.grid(a,a,a))
#   t=system.time(v<-sum(h^dim(S)[2]*apply(S,1,f)))
#   print(t)
#   cat("h=",h,"\tv=",v,"\n")
# }
dim=3
cat("n= 3\n")
for(h in c(0.01,0.005,0.001))
{
  ptm=proc.time()
  x=seq(0,1,h)
  sum=0
  for(x1 in x)
  {
    for(x2 in x)
    {
      for(x3 in x)
      {
        sum=sum+h^dim*dnorm(x1)*dnorm(x2)*dnorm(x3)
      }
    } 
  }
  cat("h=",h,"\n")
  cat("timeSpan=\n")
  print(proc.time()-ptm)
  cat("v=",sum,"\n\n")
}
##################################
# the output:
# n=2
# h= 0.01 
# timeSpan=
#   user  system elapsed 
# 0.06    0.00    0.06 
# v= 0.1187128 
# 
# h= 0.005 
# timeSpan=
#   user  system elapsed 
# 0.25    0.00    0.25 
# v= 0.1176123 
# 
# h= 0.001 
# timeSpan=
#   user  system elapsed 
# 6.22    0.02    6.27 
# v= 0.1167351 
##################################
# n=3
# timespan=  
#   user  system elapsed 
# 8.60    0.00    8.66 
# h= 0.01  v= 0.04090219 
# timespan=  
#   user  system elapsed 
# 67.36    0.03   67.92 
# h= 0.005  v= 0.04033474 
# timespan=  
#   user  system elapsed 
# 8129.88    0.35 8138.57 
# h= 0.001  v= 0.03988432 