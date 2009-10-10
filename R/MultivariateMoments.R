###############################################################################
# Functions to peRform multivariate matrix
# calculations on portfolios of assets.
#
# I've modified these to minimize the number of
# times the same calculation or statistic is run, and to minimize duplication
# of code from function to function.  This should make things more
# efficient when running against very large numbers of instruments or portfolios.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id: MultivariateMoments.R,v 1.6 2009-10-10 12:40:08 brian Exp $
###############################################################################


M3.MM = function(R){
   cAssets = ncol(R); T = nrow(R);
   mu = apply(R,2,'mean');
   M3 = matrix(rep(0,cAssets^3),nrow=cAssets,ncol=cAssets^2)
   for(t in c(1:T))
   {
       centret = as.numeric(matrix(R[t,]-mu,nrow=cAssets,ncol=1))
        M3 = M3 + ( centret%*%t(centret) )%x%t(centret)
   }
   return( 1/T*M3 );
}

M4.MM = function(R){
   cAssets = ncol(R); T = nrow(R);
   mu = apply(R,2,'mean');
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
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson and Kris Boudt
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: MultivariateMoments.R,v 1.6 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2009-10-03 18:23:55  brian
# - multiple Code-Doc mismatches cleaned up for R CMD check
# - further rationalized use of R,Ra,Rf
# - rationalized use of period/scale
#
# Revision 1.4  2008-08-12 22:52:22  brian
# - change mean.MM to multivariate_mean fn call to eliminate contention with R core mean fn
#
# Revision 1.3  2008-05-07 21:38:24  brian
# - update header, footer, and licencing to reflect moving into PerformanceAnalytics
#
# Revision 1.2  2008-01-20 12:07:24  kris
# Changed function definitions in optim_functions.R and updated the function calls in optimizer.R to these functions
#
# Revision 1.1  2008/01/20 06:30:08  brian
# - Initial Revision
#
###############################################################################