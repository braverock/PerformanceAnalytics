###############################################################################
# Functions to peRform component risk calculations on portfolios of assets.
#
# Copyright (c) 2007-2009 Kris Boudt and Brian G. Peterson
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################
# $Id$
###############################################################################


.setalphaprob = function (p)
{
    if ( p >= 0.51 ) {
        # looks like p was a percent like .99
        alpha <- 1-p
    } else {
        alpha = p
    }

    return (alpha)
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

    columns = ncol(R)
    for(column in 1:columns) {
        r = as.vector(na.omit(R[,column]))
        if (!is.numeric(r)) stop("The selected column is not numeric") 
        # location = apply(R,2,mean);
        m2 = centeredmoment(r,2)
        VaR = - mean(r) - qnorm(alpha)*sqrt(m2)
        VaR=array(VaR)
        if (column==1) {
            #create data.frame
            result=data.frame(VaR=VaR)
        } else {
            VaR=data.frame(VaR=VaR)
            result=cbind(result,VaR)
        }
    }
    colnames(result)<-colnames(R)
    return(result)
}

ES.Gaussian =  function(R,p)
{
   alpha = .setalphaprob(p)
    columns = ncol(R)
    for(column in 1:columns) {
        r = as.vector(na.omit(R[,column]))
        if (!is.numeric(r)) stop("The selected column is not numeric") 
        # location = apply(R,2,mean);
	m2 = centeredmoment(r,2)
	GES = - mean(r) + dnorm(qnorm(alpha))*sqrt(m2)/alpha
        GES=array(GES)
        if (column==1) {
            #create data.frame
            result=data.frame(GES=GES)
        } else {
            GES=data.frame(GES=GES)
            result=cbind(result,GES)
        }
    }
    colnames(result)<-colnames(R)
    return(result)
}

VaR.CornishFisher =  function(R,p)
{
   alpha = .setalphaprob(p)
   z = qnorm(alpha)
   columns = ncol(R)
   for(column in 1:columns) {
        r = as.vector(na.omit(R[,column]))
        if (!is.numeric(r)) stop("The selected column is not numeric") 
           
        # location = apply(r,2,mean);
        m2 = centeredmoment(r,2)
        m3 = centeredmoment(r,3)
        m4 = centeredmoment(r,4)
        skew = m3 / m2^(3/2);
        exkurt = m4 / m2^(2) - 3;
        # skew=skewness(r)
        # exkurt=kurtosis(r)

        h = z + (1/6)*(z^2 -1)*skew + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2
        

        VaR = - mean(r) - h*sqrt(m2)
        VaR=array(VaR)
        if (column==1) {
            #create data.frame
            result=data.frame(VaR=VaR)
        } else {
            VaR=data.frame(VaR=VaR)
            result=cbind(result,VaR)
        }
   }
   colnames(result)<-colnames(R)
   return(result)
}

ES.CornishFisher =  function(R,p,c=2)
{
   alpha = .setalphaprob(p)
   p = alpha
   z = qnorm(alpha)

   columns = ncol(R)
   for(column in 1:columns) {
        r = as.vector(na.omit(R[,column]))
        if (!is.numeric(r)) stop("The selected column is not numeric") 
	# location = apply(R,2,mean);
	m2 = centeredmoment(r,2)
	m3 = centeredmoment(r,3)
	m4 = centeredmoment(r,4)
	skew = m3 / m2^(3/2);
	exkurt = m4 / m2^(2) - 3;

	h = z + (1/6)*(z^2 -1)*skew
	if(c==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2};

	MES = dnorm(h)
	MES = MES + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
	MES = MES +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
	MES = MES + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
	MES = - mean(r) + (sqrt(m2)/p)*MES
        MES=array(MES)
        if (column==1) {
            #create data.frame
            result=data.frame(MES=MES)
        } else {
            MES=data.frame(MES=MES)
            result=cbind(result,MES)
        }
    }
    colnames(result)<-colnames(R)
    return(result)
}

operES.CornishFisher =  function(R,p,c=2)
{
   alpha = .setalphaprob(p)
   p = alpha
   z = qnorm(alpha)
   columns = ncol(R)
   for(column in 1:columns) {
        r = as.vector(na.omit(R[,column]))
        if (!is.numeric(r)) stop("The selected column is not numeric") 
	#location = apply(R,2,mean);
	m2 = centeredmoment(r,2)
	m3 = centeredmoment(r,3)
	m4 = centeredmoment(r,4)
	skew = m3 / m2^(3/2);
	exkurt = m4 / m2^(2) - 3;

	h = z + (1/6)*(z^2 -1)*skew
	if(c==2){ h = h + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2};

	MES = dnorm(h)
	MES = MES + (1/24)*(   Ipower(4,h) - 6*Ipower(2,h) + 3*dnorm(h)   )*exkurt
	MES = MES +  (1/6)*(   Ipower(3,h) - 3*Ipower(1,h)   )*skew;
	MES = MES + (1/72)*(  Ipower(6,h) -15*Ipower(4,h)+ 45*Ipower(2,h) - 15*dnorm(h) )*(skew^2)
	MES = - mean(r) - (sqrt(m2))*min( -MES/alpha , h )
        MES=array(MES)
        if (column==1) {
            #create data.frame
            result=data.frame(MES=MES)
        } else {
            MES=data.frame(MES=MES)
            result=cbind(result,MES)
        }
    }	
    colnames(result)<-colnames(R)
    return(result)
}

# Definition of statistics needed to compute Gaussian and modified VaR and ES for the return R of portfolios
# and to compute the contributions to portfolio downside risk, made by the different positions in the portfolio.
#----------------------------------------------------------------------------------------------------------------



portm2 = function(w,sigma)
{
   return(t(w)%*%sigma%*%w) #t(w) for first item?
}
derportm2 = function(w,sigma)
{
   return(2*sigma%*%w)
}
portm3 = function(w,M3)
{
   return(t(w)%*%M3%*%(w%x%w))  #t(w) for first item?
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
   if( abs( sum(contrib)-sqrt(pm2))>0.01*sqrt(pm2)) { print("error") 
   } else {
       ret<-list(  sqrt(pm2) , contrib , contrib/sqrt(pm2) )
       names(ret) <- c("StdDev","contribution","pct_contrib_StdDev")
   }
   return(ret)
}

VaR.Gaussian.portfolio =  function(p,w,mu,sigma)
{
   alpha = .setalphaprob(p)
   p=alpha
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = derportm2(w,sigma)
   VaR = - location - qnorm(alpha)*sqrt(pm2)
   derVaR = - as.vector(mu)- qnorm(alpha)*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = derVaR*as.vector(w)
   names(contrib) = names(w)
   pct_contrib = contrib/VaR
   names(pct_contrib) = names(w)
   if( abs( sum(contrib)-VaR)>0.01*abs(VaR)) { stop("contribution does not add up") } else {
      ret<-list( VaR  ,  contrib  , pct_contrib ) 
      names(ret) = c("VaR","contribution","pct_contrib_VaR")
   }
   return(ret)

}

kernel = function( x , h )
{
   return( apply( cbind( rep(0,length(x)) , 1-abs(x/h) ) , 1 , 'max' ) );
}

VaR.kernel.portfolio =  function( R, p, w )
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
   pct_contrib = CVaR/VaR
   colnames(CVaR)<-colnames(R)
   colnames(pct_contrib)<-colnames(R)
   ret= list( VaR  ,  CVaR  , pct_contrib  )
   names(ret) = c("VaR","contribution","pct_contrib_VaR")
   return(ret)
}

ES.kernel.portfolio= function( R, p, w )
{#WARNING incomplete
    VAR<-VaR.kernel.portfolio( R, p, w )
    
    #I'm sure that using Return.portfolio probably makes more sense here...
    T = dim(R)[1]; N = dim(R)[2];
    portfolioreturn = c();
    for( t in 1:T ){ portfolioreturn = c( portfolioreturn , sum(w*R[t,]) ) }
    
    PES<-mean(portfolioreturn>VAR$VaR)
    
    
}

ES.Gaussian.portfolio =  function(p,w,mu,sigma)
{
   alpha = .setalphaprob(p)
   p=alpha
   location = t(w)%*%mu
   pm2 = portm2(w,sigma)
   dpm2 = derportm2(w,sigma)
   ES = - location + dnorm(qnorm(alpha))*sqrt(pm2)/alpha
   derES = - as.vector(mu) + (1/p)*dnorm(qnorm(alpha))*(0.5*as.vector(dpm2))/sqrt(pm2);
   contrib = as.vector(w)*derES;
   names(contrib) = names(w)
   pct_contrib = contrib/ES
   names(pct_contrib) = names(w)
   if( abs( sum(contrib)-ES)>0.01*abs(ES)) { stop("contribution does not add up") } 
   else {
       ret = list(  ES  ,  contrib ,  pct_contrib  )
       names(ret) = c("ES","contribution","pct_contrib_ES")
       return(ret)
   }
}

VaR.CornishFisher.portfolio =  function(p,w,mu,sigma,M3,M4)
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
   pct_contrib = contrib/MVaR
   names(contrib) <- names(w)
   names(pct_contrib) <- names(w)
   if( abs( sum(contrib)-MVaR)>0.01*abs(MVaR)) { stop("contribution does not add up") } 
   else {
       ret=(list(   MVaR  ,  contrib, pct_contrib  ) )
       names(ret) = c("MVaR","contribution","pct_contrib_MVaR")
       return(ret)
   }
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


ES.CornishFisher.portfolio =  function(p,w,mu,sigma,M3,M4)
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
   names(contrib) = names(w)
   pct_contrib = contrib/MES
   names(pct_contrib) = names(w)
   if( abs( sum(contrib)-MES)>0.01*abs(MES)) { stop("contribution does not add up") } 
   else {
   ret= list(   MES , contrib , pct_contrib) 
   names(ret) = c("MES","contribution","pct_contrib_MES")
   return(ret)
   }
}


operES.CornishFisher.portfolio =  function(p,w,mu,sigma,M3,M4)
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
   names(contrib) = names(w)
   pct_contrib = contrib/MES
   names(pct_contrib) = names(w)
   if( abs( sum(contrib)-MES)>0.01*abs(MES)) { stop("contribution does not add up") } 
   else {
       ret= list(   MES , contrib , pct_contrib) 
       names(ret) = c("MES","contribution","pct_contrib_MES")
       return(ret)
   }
}

ES.historical = function(R,p) {
    alpha = .setalphaprob(p)
    for(column in 1:ncol(R)) {
      r = na.omit(as.vector(R[,column]))
      q = quantile(r,probs=alpha)
      exceedr = r[r<q]
      hES = (-mean(exceedr))
      if(is.nan(hES)){
        warning(paste(colnames(R[,column]),"No values less than VaR observed.  Setting ES equal to VaR."))
        hES=-q
      }
      
      hES=array(hES)
      if (column==1) {
        #create data.frame
        result=data.frame(hES=hES)
      } else {
        hES=data.frame(hES=hES)
        result=cbind(result,hES)
      }
    }	
    colnames(result)<-colnames(R)
    return(result)
}

ES.historical.portfolio = function(R,p,w)
{
    VaR = VaR.historical.portfolio(R,p,w)
    T = dim(R)[1]
    N = dim(R)[2]
    c_exceed = 0;
    r_exceed = 0;
    realizedcontrib = rep(0,N);
    for(t in c(1:T) )
    {
       rt = as.vector(R[t,])
       rp = sum(w*rt)
       if(rp<= -VaR){
          c_exceed = c_exceed + 1;
          r_exceed = r_exceed + rp;
          for( i in c(1:N) ){
             realizedcontrib[i] =realizedcontrib[i] + w[i]*rt[i] }
       }
    }
    realizedcontrib=as.numeric(realizedcontrib)/r_exceed ;
    names(realizedcontrib)<-names(w)
    ret <- list(-r_exceed/c_exceed,c_exceed,realizedcontrib) 
    names(ret) <- c("-r_exceed/c_exceed","c_exceed","realizedcontrib")
    return(ret)
}

VaR.historical = function(R,p)
{
    alpha = .setalphaprob(p)
    for(column in 1:ncol(R)) {
        r = na.omit(as.vector(R[,column]))
        rq = -quantile(r,probs=alpha)
        if (column==1) {
            result=data.frame(rq=rq)
        } else {
            rq=data.frame(rq=rq)
            result=cbind(result,rq)
        }
    }
    colnames(result)<-colnames(R)
    return(result)
}    
VaR.historical.portfolio = function(R,p,w)
{
    alpha = .setalphaprob(p)
    portret = c();
    T = dim(R)[1]
    N = dim(R)[2]
    for( t in c(1:T) ){
       portret = c(portret,sum(w*as.numeric(R[t,])))
    }
    hVaR = -1* sort(portret)[floor(alpha*T)]
    return(hVaR)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson and Kris Boudt
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################