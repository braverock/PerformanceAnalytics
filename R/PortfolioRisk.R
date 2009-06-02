###############################################################################
# Functions to perform component risk calculations on portfolios of assets.
#
# Copyright (c) 2007-2009 Kris Boudt and Brian G. Peterson
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id: PortfolioRisk.R,v 1.3 2009-06-02 01:01:13 brian Exp $
###############################################################################


.setalphaprob = function (p)
{
    if ( p >= 0.51 ) {
        # looks like p was a percent like .99
        alpha = 1-p
    } else {
        alpha = p
    }

    return (p)
}

pvalJB = function(R)
{
   m2 = centeredmoment(R,2)
   m3 = centeredmoment(R,3)
   m4 = centeredmoment(R,4)
   skew = m3 / m2^(3/2);
   exkur = m4 / m2^(2) - 3;
   JB = ( length(R)/6 )*( skew^2 + (1/4)*(exkur^2) )
   out = 1-pchisq(JB,df=2)
}

VaR.Gaussian =  function(R,p)
{

   alpha = .setalphaprob(p)
   location = apply(R,2,mean);
   m2 = centeredmoment(R,2)
   out = - location - qnorm(alpha)*sqrt(m2)
   return(out)
}

ES.Gaussian =  function(R,p)
{
   alpha = .setalphaprob(p)
   location = apply(R,2,mean);
   m2 = centeredmoment(R,2)
   out = - location + dnorm(qnorm(alpha))*sqrt(m2)/alpha
   return(out)
}

VaR.CornishFisher.new =  function(R,p,r=2)
{
   alpha = .setalphaprob(p)
   z = qnorm(alpha)
   location = apply(R,2,mean);
   m2 = centeredmoment(R,2)
   m3 = centeredmoment(R,3)
   m4 = centeredmoment(R,4)
   skew = m3 / m2^(3/2);
   exkurt = m4 / m2^(2) - 3;

   h = z + (1/6)*(z^2 -1)*skew
   if(r==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2; }

   out = -location - h*sqrt(m2)
   return(out)
}

ES.CornishFisher =  function(R,p,r=2)
{
   alpha = .setalphaprob(p)
   p = alpha
   z = qnorm(alpha)
   location = apply(R,2,mean);
   m2 = centeredmoment(R,2)
   m3 = centeredmoment(R,3)
   m4 = centeredmoment(R,4)
   skew = m3 / m2^(3/2);
   exkurt = m4 / m2^(2) - 3;

   h = z + (1/6)*(z^2 -1)*skew
   if(r==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2};

   MES = dnorm(h)
   MES = MES + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   MES = MES +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   MES = MES + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   MES = - location + (sqrt(m2)/p)*MES
   return(MES)
}

operES.CornishFisher =  function(R,p,r=2)
{
   alpha = .setalphaprob(p)
   p = alpha
   z = qnorm(alpha)
   location = apply(R,2,mean);
   m2 = centeredmoment(R,2)
   m3 = centeredmoment(R,3)
   m4 = centeredmoment(R,4)
   skew = m3 / m2^(3/2);
   exkurt = m4 / m2^(2) - 3;

   h = z + (1/6)*(z^2 -1)*skew
   if(r==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2};

   MES = dnorm(h)
   MES = MES + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   MES = MES +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   MES = MES + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   MES = - location - (sqrt(m2))*min( -MES/alpha , h )
   return(MES)
}

# Definition of statistics needed to compute Gaussian and modified VaR and ES for the return R of portfolios
# and to compute the contributions to portfolio downside risk, made by the different positions in the portfolio.
#----------------------------------------------------------------------------------------------------------------



portm2 = function(w,sigma)
{
   return(w%*%sigma%*%w) #t(w) for first item?
}
derportm2 = function(w,sigma)
{
   return(2*sigma%*%w)
}
portm3 = function(w,M3)
{
   return(w%*%M3%*%(w%x%w))  #t(w) for first item?
}
derportm3 = function(w,M3)
{
   return(3*M3%*%(w%x%w))
}
portm4 = function(w,M4)
{
   return(t(w)%*%M4%*%(w%x%w%x%w))
}
derportm4 = function(w,M4)
{
   return(4*M4%*%(w%x%w%x%w))
}

precision = 4;

Portmean = function(w,mu)
{
   return( list( t(w)%*%mu , as.vector(w)*as.vector(mu) , as.vector(w)*as.vector(mu)/t(w)%*%mu) )
}

Portsd =  function(w,sigma)
{
   pm2 = portm2(w,sigma)
   dpm2 = derportm2(w,sigma)
   dersd = (0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = dersd*as.vector(w)
   # check
   if( abs( sum(contrib)-sqrt(pm2))>0.01*sqrt(pm2)) { print("error") } else {
   return(list(  sqrt(pm2) , contrib , contrib/sqrt(pm2) )) }
}

PortVaR.Gaussian =  function(p,w,mu,sigma)
{
   alpha = .setalphaprob(p)
   p=alpha
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = derportm2(w,sigma)
   VaR = - location - qnorm(alpha)*sqrt(pm2)
   derVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = derVaR*as.vector(w)
   if( abs( sum(contrib)-VaR)>0.01*abs(VaR)) { print("error") } else {
   return(list( VaR  ,  contrib  , contrib/VaR) ) }
}

kernel = function( x , h )
{
   return( apply( cbind( rep(0,length(x)) , 1-abs(x/h) ) , 1 , 'max' ) );
}

PortkernelVaR =  function( p, w , R)
{
   alpha = .setalphaprob(p)
   T = dim(R)[1]; N = dim(R)[2];
   portfolioreturn = c();
   for( t in 1:T ){ portfolioreturn = c( portfolioreturn , sum(w*R[t,]) ) }
   bandwith = 2.575*sd(portfolioreturn)/(T^(1/5)) ;
   CVaR = c();
   VaR = -quantile( portfolioreturn , probs = alpha );
   weights = kernel(x= (-VaR-portfolioreturn) , h=bandwith);
   correction  = VaR/sum(weights*portfolioreturn)
   for( i in 1:N ){ CVaR = c( CVaR , sum( weights*R[,i] ) ) }
   # check
   CVaR = w*CVaR
   #print( sum(CVaR) ) ; print( sum( weights*portfolioreturn)  )
   CVaR = CVaR/sum(CVaR)*VaR
   return(list( VaR  ,  CVaR  , CVaR/VaR  ))
}

PortES.Gaussian =  function(p,w,mu,sigma)
{
   alpha = .setalphaprob(p)
   p=alpha
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = derportm2(w,sigma)
   ES = - location + dnorm(qnorm(alpha))*sqrt(pm2)/alpha
   derES = - as.vector(mu) + (1/p)*dnorm(qnorm(alpha))*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = as.vector(w)*derES;
   if( abs( sum(contrib)-ES)>0.01*abs(ES)) { print("error") } else {
   return(list(  ES  ,  contrib ,  contrib/ES  )) }
}

PortVaR.CornishFisher =  function(p,w,mu,sigma,M3,M4)
{
   alpha = .setalphaprob(p)
   p=alpha
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = as.vector( derportm2(w,sigma) )
   pm3 = portm3(w,M3)
   dpm3 = as.vector( derportm3(w,M3) )
   pm4 = portm4(w,M4)
   dpm4 = as.vector( derportm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

   MVaR = - location - h*sqrt(pm2)

   derGausVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   derMVaR = derGausVaR + (0.5*dpm2/sqrt(pm2))*( -(1/6)*(z^2 -1)*skew  - (1/24)*(z^3 - 3*z)*exkurt + (1/36)*(2*z^3 - 5*z)*skew^2 )
   derMVaR = derMVaR + sqrt(pm2)*( -(1/6)*(z^2 -1)*derskew  - (1/24)*(z^3 - 3*z)*derexkurt + (1/36)*(2*z^3 - 5*z)*2*skew*derskew  )
   contrib = as.vector(w)*as.vector(derMVaR)
   if( abs( sum(contrib)-MVaR)>0.01*abs(MVaR)) { print("error") } else {
   return(list(   MVaR  ,  contrib, contrib/MVaR  ) ) }
}


derIpower = function(power,h)
{

   fullprod = 1;

   if( (power%%2)==0 ) #even number: number mod is zero
   {
      pstar = power/2;
      for(j in c(1:pstar))
      {
         fullprod = fullprod*(2*j)
      }
      I = -fullprod*h*dnorm(h);

      for(i in c(1:pstar) )
      {
         prod = 1;
         for(j in c(1:i) )
         {
            prod = prod*(2*j)
         }
         I = I + (fullprod/prod)*(h^(2*i-1))*(2*i-h^2)*dnorm(h)
      }
   }else{
      pstar = (power-1)/2
      for(j in c(0:pstar) )
      {
         fullprod = fullprod*( (2*j)+1 )
      }
      I = -fullprod*dnorm(h);

      for(i in c(0:pstar) )
      {
         prod = 1;
         for(j in c(0:i) )
         {
            prod = prod*( (2*j) + 1 )
         }
         I = I + (fullprod/prod)*(h^(2*i)*(2*i+1-h^2) )*dnorm(h)
      }
   }
   return(I)
}


PortES.CornishFisher =  function(p,w,mu,sigma,M3,M4)
{
   alpha = .setalphaprob(p)
   p=alpha
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = as.vector( derportm2(w,sigma) )
   pm3 = portm3(w,M3)
   dpm3 = as.vector( derportm3(w,M3) )
   pm4 = portm4(w,M4)
   dpm4 = as.vector( derportm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

   derh = (1/6)*(z^2 -1)*derskew + (1/24)*(z^3 - 3*z)*derexkurt - (1/18)*(2*z^3 - 5*z)*skew*derskew

   E = dnorm(h)
   E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   E = E/alpha
   MES = - location + sqrt(pm2)*E

   derMES = -mu + 0.5*(dpm2/sqrt(pm2))*E
   derE = (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*derexkurt
   derE = derE +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*derskew
   derE = derE + (1/36)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*skew*derskew
   X = -h*dnorm(h) + (1/24)*(  derIpower(4,h) - 6*derIpower(2,h) -3*h*dnorm(h)  )*exkurt
   X = X + (1/6)*( derIpower(3,h) - 3*derIpower(1,h) )*skew
   X = X + (1/72)*( derIpower(6,h) - 15*derIpower(4,h) + 45*derIpower(2,h) + 15*h*dnorm(h)  )*skew^2
   derE = derE+derh*X  # X is a scalar
   derE = derE/alpha
   derMES = derMES + sqrt(pm2)*derE
   contrib = as.vector(w)*as.vector(derMES)
   if( abs( sum(contrib)-MES)>0.01*abs(MES)) { print("error") } else {
   return(list(   MES , contrib , contrib/MES) ) }
}


operPortES.CornishFisher =  function(p,w,mu,sigma,M3,M4)
{
   alpha = .setalphaprob(p)
   p=alpha
   z = qnorm(alpha)
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = as.vector( derportm2(w,sigma) )
   pm3 = portm3(w,M3)
   dpm3 = as.vector( derportm3(w,M3) )
   pm4 = portm4(w,M4)
   dpm4 = as.vector( derportm4(w,M4) )

   skew = pm3 / pm2^(3/2);
   exkurt = pm4 / pm2^(2) - 3;

   derskew = ( 2*(pm2^(3/2))*dpm3 - 3*pm3*sqrt(pm2)*dpm2 )/(2*pm2^3)
   derexkurt = ( (pm2)*dpm4 - 2*pm4*dpm2    )/(pm2^3)

   h = z + (1/6)*(z^2 -1)*skew
   h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

   derh = (1/6)*(z^2 -1)*derskew + (1/24)*(z^3 - 3*z)*derexkurt - (1/18)*(2*z^3 - 5*z)*skew*derskew

   E = dnorm(h)
   E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
   E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
   E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
   E = E/alpha

   MES = - location - sqrt(pm2)*min(-E,h)

   if(-E<=h){
      derMES = -mu + 0.5*(dpm2/sqrt(pm2))*E
      derE = (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*derexkurt
      derE = derE +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*derskew
      derE = derE + (1/36)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*skew*derskew
      X = -h*dnorm(h) + (1/24)*(  derIpower(4,h) - 6*derIpower(2,h) -3*h*dnorm(h)  )*exkurt
      X = X + (1/6)*( derIpower(3,h) - 3*derIpower(1,h) )*skew
      X = X + (1/72)*( derIpower(6,h) - 15*derIpower(4,h) + 45*derIpower(2,h) + 15*h*dnorm(h)  )*skew^2
      derE = derE+derh*X  # X is a scalar
      derE = derE/alpha
      derMES = derMES + sqrt(pm2)*derE }else{
      derMES = -mu - 0.5*(dpm2/sqrt(pm2))*h - sqrt(pm2)*derh ;  }
   contrib = as.vector(w)*as.vector(derMES)
   if( abs( sum(contrib)-MES)>0.01*abs(MES)) { print("error") } else {
   return(list( MES ,  contrib  , contrib/MES ) ) }
}

realportES = function(R,w,VaR)
{
    T = dim(R)[1]
    N = dim(R)[2]
    c_exceed = 0;
    r_exceed = 0;
    realizedcontrib = rep(0,N);
    for(t in c(1:T) )
    {
       rt = as.vector(R[t,])
       rp = sum(w*rt)
       if(rp<=-VaR){
          c_exceed = c_exceed + 1;
          r_exceed = r_exceed + rp;
          for( i in c(1:N) ){
             realizedcontrib[i] =realizedcontrib[i] + w[i]*rt[i] }
       }
    }
    realizedcontrib=as.numeric(realizedcontrib)/r_exceed ;
    return( list(-r_exceed/c_exceed,c_exceed,realizedcontrib) )
}

realportVaR = function(R,w,p)
{
    portret = c();
    T = dim(R)[1]
    N = dim(R)[2]
    for( t in c(1:T) ){
       portret = c(portret,sum(w*as.numeric(R[t,])))
    }
    VaR = sort(portret)[floor(alpha*T)]
    return(-VaR)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson and Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: PortfolioRisk.R,v 1.3 2009-06-02 01:01:13 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.2  2009-04-17 15:23:47  brian
# - better standardize use of p for probability and conversion to alpha number
#
# Revision 1.1  2009-04-17 15:14:00  brian
# - Initial revision of VaR wrapper and portfolio risk functions
#
###############################################################################