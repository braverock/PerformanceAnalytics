

portm2 = function(w,sigma)
{
   return(t(w)%*%sigma%*%w)
}
PerformanceAnalytics:::portm2

portm3 = function(w,M3)
{
   return(t(w)%*%M3%*%(w%x%w))
}
PerformanceAnalytics:::portm3

table.VaR.CornishFisher.portfolio = 
function (p, w, mu, sigma, M3, M4 , names) 
{
    w = matrix( w , ncol = 1 )
    alpha = PerformanceAnalytics:::.setalphaprob(p)
    p = alpha
    z = qnorm(alpha)
    location = t(w) %*% mu
    pm2 = portm2(w, sigma)
    dpm2 = as.vector(PerformanceAnalytics:::derportm2(w, sigma))
    pm3 = portm3(w, M3)
    dpm3 = as.vector(PerformanceAnalytics:::derportm3(w, M3))
    pm4 = PerformanceAnalytics:::portm4(w, M4)
    dpm4 = as.vector(PerformanceAnalytics:::derportm4(w, M4))
    skew = pm3/pm2^(3/2)
    exkurt = pm4/pm2^(2) - 3
    derskew = (2 * (pm2^(3/2)) * dpm3 - 3 * pm3 * sqrt(pm2) * 
        dpm2)/(2 * pm2^3)
    derexkurt = ((pm2) * dpm4 - 2 * pm4 * dpm2)/(pm2^3)
    h = z + (1/6) * (z^2 - 1) * skew
    h = h + (1/24) * (z^3 - 3 * z) * exkurt - (1/36) * (2 * z^3 - 
        5 * z) * skew^2
    MVaR = -location - h * sqrt(pm2)
    derGausVaR = -as.vector(mu) - qnorm(alpha) * (0.5 * as.vector(dpm2))/sqrt(pm2)
    derMVaR_skew =  (0.5 * dpm2/sqrt(pm2)) * (-(1/6) *
              (z^2 - 1) * skew + (1/36) * (2 * z^3 - 5 * z) * skew^2)
    derMVaR_skew = derMVaR_skew + sqrt(pm2) * (
           -(1/6) * (z^2 - 1) * derskew + (1/36) * (2 * z^3 - 
        5 * z) * 2 * skew * derskew)
    derMVaR_kurt = (0.5 * dpm2/sqrt(pm2)) * ( - (1/24) * (z^3 - 3 * z) * exkurt  )
    derMVaR_kurt = derMVaR_kurt +  sqrt(pm2) * (  - (1/24) * (z^3 - 3 * z) * derexkurt) 


    # derMVaR = derGausVaR + (0.5 * dpm2/sqrt(pm2)) * (-(1/6) * 
    #    (z^2 - 1) * skew - (1/24) * (z^3 - 3 * z) * exkurt + 
    #    (1/36) * (2 * z^3 - 5 * z) * skew^2)
    # derMVaR = derMVaR + sqrt(pm2) * (-(1/6) * (z^2 - 1) * derskew - 
    #    (1/24) * (z^3 - 3 * z) * derexkurt + (1/36) * (2 * z^3 - 
    #    5 * z) * 2 * skew * derskew)

    derMVaR = derGausVaR + derMVaR_skew + derMVaR_kurt 

    contrib = as.vector(w) * as.vector(derMVaR)
    contrib_gaus = as.vector(w) * as.vector(derGausVaR)
    contrib_skew = as.vector(w) * as.vector(derMVaR_skew)
    contrib_kurt = as.vector(w) * as.vector(derMVaR_kurt) 
    pct_contrib = contrib/MVaR
    pct_contrib_gaus = contrib_gaus/MVaR
    pct_contrib_skew = contrib_skew/MVaR
    pct_contrib_kurt = contrib_kurt/MVaR

    names(contrib_gaus) = names(contrib_skew) = names(contrib_kurt) = names(contrib) <- names(w); 
    names(pct_contrib_gaus) = names(pct_contrib_skew) = names(pct_contrib_kurt) = names(pct_contrib) <- names(w)

    out = cbind( contrib, contrib_gaus , contrib_skew , contrib_kurt , 
                pct_contrib , pct_contrib_gaus , pct_contrib_skew , pct_contrib_kurt )
    out = rbind( out , apply( out , 2 , 'sum' ) )
    rownames(out) = c(names,"sum")
    colnames(out)= c( "total comp" , "Gaussian comp" , "skew comp" , "kurt comp",
              "Perc total" ,  "Perc. Gaussian comp" , "Perc. skew comp" , "Perc. kurt comp")
    print( round(out, 3 ) )
    ret = (list(MVaR, contrib, contrib_gaus , contrib_skew , contrib_kurt , 
                pct_contrib , pct_contrib_gaus , pct_contrib_skew , pct_contrib_kurt ))
    names(ret) = c("MVaR", "contribution", "contribution_gaus" , "contribution_skew" , "contribution_kurt" , 
                     "pct_contrib","pct_contrib_gaus","pct_contrib_skew","pct_contrib_kurt" )
    return(ret)
    
}

#data(edhec)
#N = 4
#edhec = edhec[,1:N]
#mu = apply( edhec , 2 , 'mean' )
#sigma = cov(edhec)
#m3 = PerformanceAnalytics:::M3.MM( edhec )
#m4 = PerformanceAnalytics:::M4.MM( edhec )
#out = Table.VaR.CornishFisher.portfolio ( p = 0.95 , w = rep(1/N,N) , mu = mu , sigma = sigma , M3 = m3 , M4 = m4 , names = colnames(edhec) ) 




###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
