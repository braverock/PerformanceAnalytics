#To get some insight on the relationships between maximum drawdown per unit of volatility 
#and mean return divided by volatility, we have proceeded to Monte-Carlo simulations.
# We have simulated cash flows over a period of 36 monthly returns and measured maximum 
#drawdown for varied levels of annualised return divided by volatility varying from minus
# two to two by step of 0.1. The process has been repeated six thousand times.

mu=mean(Return.annualized(edhec))
monthly=(1+mu)^(1/12)-1
sig=StdDev.annualized(edhec[,1])[1];
T= 36
j=1
dt=1/T
nsim=10;
r=matrix(0,nsim,T+1)

r[,1]=monthly;
# Sigma 'monthly volatiltiy' will be the varying term
ratio= seq(-2, 2, by=.1);
len = length(ratio)
ddown=matrix(0,nsim,len)
Z <- array(0, c(len))
for(i in 1:len)
{
  monthly = sig*ratio[i];

  for(j in 1:nsim)
{
    dz=rnorm(T)
    
    
      r[j,2:37]=monthly+sig*dz
    
    ddown[j,i]= ES((r[j,]))
}
}
plot(ddown[1,])

