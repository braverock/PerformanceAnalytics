# TODO: plot efficient frontier in R


#' This script performs the butterfly-trading case study for the 
#' Entropy-Pooling approach by Attilio Meucci, as it appears in 
#' "A. Meucci - Fully Flexible Views: Theory and Practice -
#' The Risk Magazine, October 2008, p 100-106"
#' available at www.symmys.com > Research > Working Papers
#' Adapted from Code by A. Meucci, September 2008
#' Last version available at www.symmys.com > Teaching > MATLAB


###################################################################
#' Load panel X of joint factors realizations and vector p of respective probabilities
#' In real life, these are provided by the estimation process
###################################################################
load("butterflyTradingX.rda")

FactorNames = c('MSFT_close' , 'MSFT_vol_30' , 'MSFT_vol_91' , 'MSFT_vol_182' , 
                'YHOO_close' , 'YHOO_vol_30' , 'YHOO_vol_91' , 'YHOO_vol_182' ,
                'GOOG_close' , 'GOOG_vol_30' , 'GOOG_vol_91' , 'GOOG_vol_182' , 
                'USD SWAP 2Y rate' , 'USD SWAP 10Y rate' )

p = matrix( rep( 1 / 100224 , 100224 ) , ncol = 1 ) # creates a Jx1 matrix of equal probabilities for each scenario

ans = 1 / 100224

library( R.matlab )
library( matlab )

emptyMatrix = matrix( nrow = 0 , ncol = 0 )

###################################################################
# Load current prices, deltas and other analytics of the securities 
# In real life, these are provided by data provider
###################################################################

load( "FactorDistributions.rda" )

# create Butterflies as a list of named arrays
Butterflies = as.matrix( Butterflies[[1]] , nrow = 8 , ncol = 9 )
Butterflies = matrix(Butterflies, ncol = 9 , nrow = 8 )
rownames( Butterflies ) = c( "Name" , "P_0" , "Y_0" , "K" , "T" , "sig_0" , "Delta" , "Vega" )
colnames( Butterflies ) = c( "MSFT_vol_30" , "MSFT_vol_91" , "MSFT_vol_182" , 
                             "YHOO_vol_30" , "YHOO_vol_91" , "YHOO_vol_182" ,	
                             "GOOG_vol_30" , "GOOG_vol_91" , "GOOG_vol_182" )

colnames( X ) = FactorNames
Butterflies = lapply( seq_len( ncol( Butterflies ) ), function( i ) Butterflies[ , i ] )

###################################################################
# Map factors scenarios into p&l scenarios at the investment horizon
# In real life with complex products, the pricing can be VERY costly 
###################################################################
PnL = HorizonPricing( Butterflies , X )

###################################################################
# compute mean-risk efficient frontier based on prior market distribution
###################################################################
Options = list()
Options$Quant = .95  # quantile level for CVaR
Options$NumPortf = 30  # number of portfolios in efficient frontier
Options$FrontierSpan = array( c(.1 , .8) ) # range of normalized exp.vals. spanned by efficient frontier
Options$DeltaNeutral = 1 # 1 = portfolio is delta neutral, 0 = no constraint
Options$Budget = 0 # initial capital
Options$Limit = 10000 # limit to absolute value of each investment

# The following commands loads the PnL object as calculated by the MATLAB option-pricing model
    # This is for testing purposes so we can replicate results with the Meucci program
load("C:\\Users\\Ram Ahluwalia\\Documents\\Applications\\R\\r-user\\MeucciButterflyPnL.rda")

optimalPortfolios = LongShortMeanCVaRFrontier( PnL , p , Butterflies , Options )

View( optimalPortfolios ) # Note that composition is measured in dollars. Here we are short GOOG_vol_91 and long GOOG_vol_182

# plot efficient frontier ( Exp , CVaR , w )

###################################################################
# process views (this is the core of the Entropy Pooling approach
###################################################################
p_1 = ViewImpliedVol( X , p )
    # View 1 (inequality view): bearish on on 2m-6pm implied volaility spread for Google
    # Note that the mean-CVaR efficient frontier is long based on the reference model
    # After processing the view, the G6m-G2m spread is now short in the mean-CVaR long-short efficient frontier

p_2 = ViewRealizedVol( X , p )
    # view 2 (relative inequality view on median): bullish on realized volatility of MSFT (i.e. absolute log-change in the underlying). 
    # This is the variable such that, if larger than a threshold, a long position in the butterfly turns into a profit (e.g. Rachev 2003)
    # we issue a relative statement on the media comparing it with the third quintile implied by the reference market model

p_3 = ViewCurveSlope( X , p )
    # view 3 (equality view - expectations and binding constraints): slope of the yield curve will increase by 5 bp
 
# assign confidence to the views and pool opinions
    # sum of weights should be equal 1
    # .35 is the weight on the reference model
    # .2 is the confidence on View 1; .25 is the confidence on View 2; .2 is the confidence on View 3
    c = matrix( c( .35 , .2 , .25 , .2 ) , nrow = 1 ) 

p_= cbind( p , p_1 , p_2 , p_3 ) %*% t(c) # compute the uncertainty weighted posterior probabilities

###################################################################
# compute mean-risk efficient frontier based on posterior market distribution
###################################################################
optimalPortfoliosPosterior = LongShortMeanCVaRFrontier( PnL , p_ , Butterflies , Options )

View( optimalPortfoliosPosterior ) # Note that composition is measured in dollars. Now we are long GOOG_vol_91, and short Goog_vol_182

# plot efficient frontier ( Exp_ , CVaR_ , w_ )
PlotFrontier(Exp_,SDev_,w_)

###################################################################
# tests
###################################################################
p_3b = ViewCurveSlope( X , p )
p_4 = ViewCurveSlopeTest( X , p )

ViewCurveSlopeTest = function( X , p )
  { 
  J = nrow( X ) ; K = ncol( X )
  
  # constrain probabilities to sum to one...
  Aeq = ones( 1 , J )
  beq = matrix( 1 , nrow = 1 , ncol = 1 )
  browser()
  # ...constrain the expectation...
  V = matrix( , nrow = nrow( X ) , ncol = 0 )  
  # Add 3 equality views
  V = cbind( V , X[ , 14 ] - X[ , 13 ] ) # View 1: spread on treasuries
  V = cbind( V , X[ , 14 ] - X[ , 13 ] ) # View 2: identical view (spread on treasuries)
  V = cbind( V , X[ , 6 ] - X[ , 5 ] )   # View 3: difference in YHOO Vol
  v = matrix( c( .0005 , 0 ) , nrow = ncol( V ) , ncol = 1 )
  
  Aeq = rbind( Aeq , t(V) )
      
  beq = rbind( beq , v )
  
  # add an inequality view
    # ...constrain the median...  
  V = abs( X[ , 1 ] ) # absolute value of the log of changes in MSFT close prices (definition of realized volatility)    
  V_Sort = sort( V , decreasing = FALSE ) # sorting of the abs value of log changes in prices from smallest to largest
  I_Sort = order( V ) 
  
  F = cumsum( p[ I_Sort ] ) # represents the cumulative sum of probabilities from ~0 to 1
  
  I_Reference = max( matlab:::find( F <= 3/5 ) ) # finds the (max) index corresponding to element with value <= 3/5 along the empirical cumulative density function for the abs log-changes in price
  V_Reference = V_Sort[ I_Reference ] # returns the corresponding abs log of change in price at the 3/5 of the cumulative density function

  I_Select = find( V <= V_Reference ) # finds all indices with value of abs log-change in price less than the reference value  
  a = zeros( 1 , J )
  a[ I_Select ] = 1 # select those cases where the abs log-change in price is less than the 3/5 of the empirical cumulative density...
  
  A = a
  b = .5 # ... and assign the probability of these cases occuring as 50%. This moves the media of the distribution
  
  # ...compute posterior probabilities
  p_ = EntropyProg( p , A , b , Aeq ,beq )
  return( p_ )
  }
