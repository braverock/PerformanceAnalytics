# This script fits the swap rates dynamics to a multivariate Ornstein-Uhlenbeck process 
# and computes and plots the estimated future distribution
# see A. Meucci (2009) 
# "Review of Statistical Arbitrage, Cointegration, and Multivariate Ornstein-Uhlenbeck"
# available at ssrn.com

# Code by A. Meucci, April 2009
# Most recent version available at www.symmys.com > Teaching > MATLAB

# inputs
TimeStep = 5 # select time interval (days)
Taus = c( 1/252,5/252,1/12,.5,1,2,10 ) # select horizon projection (years)
Pick = c( 2,3 )

# estimation
StepRates = Rates[ seq( from = 1, to = nrow( Rates ), by = TimeStep ) , ]
OUResult = FitOU( as.matrix( StepRates ) , TimeStep / 252 )

for( s in 1:length(Taus))
{
  RGB = .6 * as.matrix( c( runif(1),runif(1),runif(1) ) )
  tau = Taus[ s ]

  # projection
  # x_T = Mu
  x_T = Rates[ nrow( Rates ) , ]
  OUstepResult = OUstep( as.matrix( x_T ) , tau , OUResult$Mu , OUResult$Th , OUResult$Sig )

  # plot
  # historical observations
  plot( Rates[ , Pick[1] ] , Rates[ , Pick[2] ] )

  # current observation
  plot( x_T[ Pick[1] ] , x_T[ Pick[2] ] )

  # horizon location
  plot( OUstepResult$Mu_t[ Pick[1] ] , OUstepResult$Mu_t[ Pick[2] ] )
}