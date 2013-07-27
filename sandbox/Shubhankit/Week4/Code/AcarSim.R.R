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
thres=4;
r=matrix(0,nsim,T+1)
monthly = 0
r[,1]=monthly;
# Sigma 'monthly volatiltiy' will be the varying term
ratio= seq(-2, 2, by=.1);
len = length(ratio)
ddown=array(0, dim=c(nsim,len,thres))
Z <- array(0, c(len))
for(i in 1:len)
{
  monthly = sig*ratio[i];

  for(j in 1:nsim)
{
    dz=rnorm(T)
    
    
      r[j,2:37]=monthly+(sig*dz*sqrt(3*dt))
    
    ddown[j,i,1]= ES((r[j,]),.99)
    ddown[j,i,2]= ES((r[j,]),.95)
    ddown[j,i,3]= ES((r[j,]),.90)
    ddown[j,i,4]= ES((r[j,]),.85)
    assign("last.warning", NULL, envir = baseenv())
}
}
plot(ddown[1,,1]/sig)

