###############################################################################
# Functions to perform multivariate matrix
# calculations on portfolios of assets.
#
# I've modified these to minimize the number of
# times the same calculation or statistic is run, and to minimize duplication
# of code from function to function.  This should make things more
# efficient when running against very large numbers of instruments or portfolios.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson for PerformanceAnalytics
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id$
###############################################################################


M3.MM = function(R,...){
   cAssets = ncol(R); T = nrow(R);
   if(!hasArg(mu)) mu = apply(R,2,'mean') else mu=mu=list(...)$mu
   M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
   for(t in c(1:T))
   {
       centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
       M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
   }
   return( 1/T*M3 );
}

M4.MM = function(R,...){
   cAssets = ncol(R); T = nrow(R);
   if(!hasArg(mu))   mu = apply(R,2,'mean')  else mu=list(...)$mu
   M4 = matrix(rep(0,cAssets^4),nrow=cAssets,ncol=cAssets^3);
   for(t in c(1:T))
   {
       centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
       M4 = M4 + ( centret%*%t(centret) )%x%t(centret)%x%t(centret)
   }
   return( 1/T*M4 );
}

multivariate_mean = function(w,mu){
   return( t(w)%*%mu )
}

StdDev.MM = function(w, sigma){
        return(   sqrt(t(w)%*%sigma%*%w)  )
}

skewness.MM=function(w,sigma,M3){
        return( ( t(w)%*%M3%*%(w%x%w) )/ (StdDev.MM(w,sigma))^3         )
}

kurtosis.MM=function(w,sigma,M4){
        return(  ( t(w)%*%M4%*%(w%x%w%x%w) )/ (StdDev.MM(w,sigma))^4    )
}

SR.StdDev.MM = function(w, mu , sigma ){
         return( multivariate_mean(w,mu) / StdDev.MM(w,sigma)   )
}

GVaR.MM = function(w, mu, sigma, p ){
         return ( -multivariate_mean(w,mu) - qnorm(1-p)*StdDev.MM(w,sigma)  )
}

SR.GVaR.MM = function(w, mu, sigma, p ){
          return(  multivariate_mean(w,mu) / GVaR.MM(w,mu,sigma,p)    )
}

mVaR.MM = function(w, mu, sigma, M3, M4, p ){
          skew = skewness.MM(w,sigma,M3);
          exkurt = kurtosis.MM(w,sigma,M4) - 3;
          z = qnorm(1-p);
          zc = z + (1/6)*(z^2 -1)*skew
          Zcf = zc + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
          return ( -multivariate_mean(w,mu) - Zcf*StdDev.MM(w,sigma)  )
}

SR.mVaR.MM = function(w, mu, sigma, M3, M4, p) {
          return( multivariate_mean(w,mu) / mVaR.MM(w, mu, sigma, M3, M4, p )     )
}

GES.MM = function(w, mu, sigma , p){
         return ( -multivariate_mean(w,mu) + dnorm(qnorm(1-p))*StdDev.MM(w,sigma)/(1-p) )
}

SR.GES.MM = function(w, mu , sigma , p){
                       return( multivariate_mean(w,mu) / GES.MM(w, mu, sigma , p)  )
}

Ipower = function(power,h){
          fullprod = 1;
          if( (power%%2)==0 ) #even number: number mod is zero
          {
                  pstar = power/2;
                  for(j in c(1:pstar)){
                        fullprod = fullprod*(2*j)  }
                   I = fullprod*dnorm(h);

                   for(i in c(1:pstar) )
                   {
                         prod = 1;
                         for(j in c(1:i) ){
                              prod = prod*(2*j)  }
                         I = I + (fullprod/prod)*(h^(2*i))*dnorm(h)
                    }
          }else{
                    pstar = (power-1)/2
                    for(j in c(0:pstar) ) {
                             fullprod = fullprod*( (2*j)+1 ) }
                    I = -fullprod*pnorm(h);
                    for(i in c(0:pstar) ){
                           prod = 1;
                           for(j in c(0:i) ){
                              prod = prod*( (2*j) + 1 )}
                           I = I + (fullprod/prod)*(h^(  (2*i) + 1))*dnorm(h) }
          }
          return(I)
}

mES.MM = function(w, mu, sigma, M3 , M4 , p){
          skew = skewness.MM(w,sigma,M3);
          exkurt = kurtosis.MM(w,sigma,M4) - 3;
          z = qnorm(1-p);
          h = z + (1/6)*(z^2 -1)*skew
          h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

          E = dnorm(h)
          E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
          E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
          E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
          E = E/(1-p)

          return (- multivariate_mean(w,mu) - StdDev.MM(w,sigma)*min(-E,h) )
}

SR.mES.MM = function(w, mu, sigma, M3 , M4 , p){
   return( multivariate_mean(w,mu) / mES.MM(w, mu, sigma, M3 , M4 , p)  )
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson and Kris Boudt
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################