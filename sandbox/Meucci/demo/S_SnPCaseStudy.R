####################################################################
# Example from Meucci's MATLAB script:  S_SnPCaseStudy.M
# See MATLAB package "Meucci_RobustBayesian" for original MATLAB
# source on www.symmys.com
####################################################################

load("SectorsSnP500")

p_m = .1 # aversion to estimation risk for mu
p_s = .1 # aversion to estimation risk for sigma

Ps <- P[seq(from=1, to=nrow(P),by=5),]
R <- data.frame((Ps[2:nrow(Ps),]/Ps[1:nrow(Ps)-1,]) - 1)
Dates_P <- DP[seq(from=1, to=nrow(DP), by=5),]
Dates_R <- DP[2:nrow(DP),]
Ttot = nrow(R)
N = ncol(R)

W = 52
NumPortf = 10
Ret_hat = NULL
Ret_rB = NULL
Dates = NULL

for(t in (W+1):(Ttot-1)) {
  Rets <- R[(t-W):t,]
  m_hat = colMeans(Rets)
  S_hat = cov(Rets)
  sampleFrontier = efficientFrontier( NumPortf , S_hat , m_hat , TRUE )
  S0 = diag(diag(S_hat))
  m0 = 0.5*S0%*%matrix(c(rep(1,N)))/N
  
  posterior = PartialConfidencePosterior(mean_sample = m_hat, cov_sample = S_hat, mean_prior = m0, cov_prior  = S0,
                                         relativeConfidenceInMeanPrior = 2 , relativeConfidenceInCovPrior = 2 , sampleSize = nrow( Rets ) )
  cov_post = posterior$cov_post ; mean_post = posterior$mean_post ; time_post = posterior$time_post ; nu_post = posterior$nu_post ; rm( posterior )
  
  bayesianFrontier = efficientFrontier(NumPortf, cov_post, mean_post)
  
  q_m2 = qchisq(p_m, N)
  g_m = sqrt(q_m2/time_post*nu_post/(nu_post-2))
  q_s2 = qchisq(p_s,N*(N+1)/2)
  pickVol = round(0.8*NumPortf)
  v = ( sampleFrontier[[ "volatility" ]][ pickVol ] ) ^ 2
  if ( is.na(v) == TRUE ) { stop( "The chosen volatility is too high" ) }
  g_s = v/ ( nu_post / ( nu_post + N + 1 ) + sqrt( 2 * nu_post * nu_post * q_s2 / ( ( nu_post + N + 1 ) ^ 3 ) ) ) 
  
  target = NULL
  
  wu = sampleFrontier[[ "weights"]][ pickVol, ]
  Ret_hat = c(Ret_hat, as.matrix(R[t+1,])%*%wu)
  
  for(k in 1:NumPortf-1) {
    if ( wu %*% cov_post %*% wu <= g_s ) # constraint for formula 19
    { target = c( target , wu %*% mean_post - g_m * sqrt( wu %*% cov_post %*% wu )) } # formula 19
    else { target = c( target , -999999999 ) }
  }
  
  maxTarget = max( target )    
  maxIndex = which( target == maxTarget , arr.ind = TRUE ) # identify most robust Bayesian portfolio
  if ( length( maxIndex ) > 1 ) { k = 1 }
  else { k = maxIndex  }
  
  wu = bayesianFrontier[[ "weights" ]][ k , ]
  Ret_rB = c(Ret_rB, as.matrix(R[t+1,])%*%wu)
  Dates = c(Dates, Dates_R[t+1])
}

NAV_hat=cumprod(1+Ret_hat);
NAV_rB=cumprod(1+Ret_rB);

nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE), respect=TRUE)
layout.show(nf)

plot(Dates, Ret_rB)
lines(Dates, Ret_rB)
plot(Dates, Ret_hat)
lines(Dates, Ret_hat)
plot(Dates, NAV_hat)
lines(Dates, NAV_hat)
plot(Dates, NAV_rB)
lines(Dates, NAV_rB)