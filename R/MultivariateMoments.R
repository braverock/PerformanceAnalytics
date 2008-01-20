###############################################################################
# Functions for use in optimizer.R file to perform multivariate matrix
# calculations on portfolios of assets.  Replaces old univariate code.
#
# I've modified these from the file localsearch.R to minimize the number of
# times the same calculation or statistic is run, and to minimize duplication
# of code from function to function.  This should make things more
# efficient when running against very large numbers of portfolios.
#
# Copyright (c) 2008 Kris Boudt and Brian G. Peterson
# Kindly contact the authors for permission to use these functions
###############################################################################
# $Id: MultivariateMoments.R,v 1.1 2008-01-20 06:30:08 brian Exp $
###############################################################################


                   StdDevfun = function(w, sigma){
                       return(  StdDevR)
                   }

                   SRfun = function(w, sigma, meanR ){
                       return( meanR / StdDevfun(w,sigma)   )
                   }

                   GVaRfun = function(w, sigma, meanR, p ){
                      return ( meanR - qnorm(p)*StdDevR )
                   }

                   SR.GVaRfun = function(w, sigma, meanR, p ){
                       return(  meanR / GVaRfun(w,sigma,meanR,p)    )
                   }

                   mVaRfun = function(w, sigma, meanR, p, M3, M4 ){
                      pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                      skew = pm3 / pm2^(3/2);
                      exkurt = pm4 / pm2^(2) - 3;
                      z = qnorm(p);
                      h = z + (1/6)*(z^2 -1)*skew
                      h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
                      return ( meanR - h*sqrt( pm2  ) )
                   }

                   SR.mVaRfun = function(w, sigma, meanR, p, M3, M4) {
                      return( meanR / mVaRfun(w, sigma, meanR, p, M3, M4 )    )
                   }

                   GESfun = function(w, StdDevR, meanR, p){
                      return ( -meanR + dnorm(qnorm(p))*StdDevR/p )
                   }

                   SR.GESfun = function(w, StdDevR, meanR,  p){
                       return( meanR / GESfun(w, StdDevR, meanR, p)  )
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

                     mESfun = function(w, sigma, mu, p){
                       pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                       skew = pm3 / pm2^(3/2);
                       exkurt = pm4 / pm2^(2) - 3; z = qnorm(p);
                       h = z + (1/6)*(z^2 -1)*skew
                       h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

                       E = dnorm(h)
                       E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
                       E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
                       E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
                       E = E/p

                       return (- (t(w)%*%mu) - sqrt(pm2)*min(-E,h) )
                   }

                   NegSR.mESfun = function(w, sigma, mu, p){
                       pm4 = t(w)%*%M4%*%(w%x%w%x%w) ; pm3 = t(w)%*%M3%*%(w%x%w) ; pm2 =  t(w)%*%sigma%*%w ;
                       skew = pm3 / pm2^(3/2);
                       exkurt = pm4 / pm2^(2) - 3; z = qnorm(p);
                       h = z + (1/6)*(z^2 -1)*skew
                       h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;

                       E = dnorm(h)
                       E = E + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
                       E = E +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
                       E = E + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
                       E = E/p
                       mES = - (t(w)%*%mu) - sqrt(pm2)*min(-E,h)
                       return ( - (t(w)%*%mu) / mES )
                   }

###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################