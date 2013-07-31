#To get some insight on the relationships between maximum drawdown per unit of volatility 
#and mean return divided by volatility, we have proceeded to Monte-Carlo simulations.
# We have simulated cash flows over a period of 36 monthly returns and measured maximum 
#drawdown for varied levels of annualised return divided by volatility varying from minus
# two to two by step of 0.1. The process has been repeated six thousand times.
AcarSim <-
  function()
  {
mu=mean(Return.annualized(edhec))
monthly=(1+mu)^(1/12)-1
sig=StdDev.annualized(edhec[,1])[1];
T= 36
j=1
dt=1/T
nsim=6000;
thres=4;
r=matrix(0,nsim,T+1)
monthly = 0
r[,1]=monthly;
# Sigma 'monthly volatiltiy' will be the varying term
ratio= seq(-2, 2, by=.1);
len = length(ratio)
ddown=array(0, dim=c(nsim,len,thres))
fddown=array(0, dim=c(len,thres))
Z <- array(0, c(len))
for(i in 1:len)
{
  monthly = sig*ratio[i];

  for(j in 1:nsim)
{
    dz=rnorm(T)
    
    
      r[j,2:37]=monthly+(sig*dz*sqrt(3*dt))
    
    ddown[j,i,1]= ES((r[j,]),.99)
    ddown[j,i,1][is.na(ddown[j,i,1])] <- 0
    fddown[i,1]=fddown[i,1]+ddown[j,i,1]
    ddown[j,i,2]= ES((r[j,]),.95)
    ddown[j,i,2][is.na(ddown[j,i,2])] <- 0
    fddown[i,2]=fddown[i,2]+ddown[j,i,2]
    ddown[j,i,3]= ES((r[j,]),.90)
    ddown[j,i,3][is.na(ddown[j,i,3])] <- 0
    fddown[i,3]=fddown[i,3]+ddown[j,i,3]
    ddown[j,i,4]= ES((r[j,]),.85)
    ddown[j,i,4][is.na(ddown[j,i,4])] <- 0
    fddown[i,4]=fddown[i,4]+ddown[j,i,4]
    assign("last.warning", NULL, envir = baseenv())
}
}
plot(((fddown[,1])/(sig*nsim)),xlab="Annualised Return/Volatility from [-2,2]",ylab="Maximum Drawdown/Volatility",type='o',col="blue")
lines(((fddown[,2])/(sig*nsim)),type='o',col="pink")
lines(((fddown[,3])/(sig*nsim)),type='o',col="green")
lines(((fddown[,4])/(sig*nsim)),type='o',col="red")
legend(32,-4, c("%99", "%95", "%90","%85"), col = c("blue","pink","green","red"), text.col= "black",
       lty = c(2, -1, 1), pch = c(-1, 3, 4), merge = TRUE, bg='gray90')

title("Maximum Drawdown/Volatility as a function of Return/Volatility 
36 monthly returns simulated 6,000 time") 
}