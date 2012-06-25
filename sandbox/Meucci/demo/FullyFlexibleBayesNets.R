# This case study uses Entropy Pooling to compute Fully Flexible Bayesian networks for risk management, see 
# A. Meucci (2010) "Fully Flexible Bayesian Networks", working paper
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/152

##################################################################################
# upload scenarios table and prior distribution for changes in
# SWAP2YR SWAP10YR CDXIG S&P500 DollarIndex Crude Gold VIX 10YRInflationSwapRate
library( matlab )
data( "MeucciFreaqEst" ) 
Names = unlist( Names )
colnames( X ) = colnames( DY ) = colnames( Data ) = Names
rownames( Data ) = Dates

J = nrow( X ) ; N = ncol( X )
e = .01
p = ( 1 - e ) * p + e * ones( J , 1 ) / J # assigns a minimum probability to each scenario of e * number of scenarios
moments = ComputeMoments( X , p )
m = moments$means ; s = moments$sd ; C = moments$correlationMatrix ; rm( moments )

##################################################################################
# input views
# statement: View(k).Who (e.g. [1 3]) = View(k).Equal (e.g. {[2 3] [1 3 5]})
# optional conditional statement: View(k).Cond_Who (e.g. [2]) = View(k).Cond_Equal (e.g. {[1]})
# amount of stress is quantified as Prob(statement) <= View(k).v if View(k).sgn = 1
#                                   Prob(statement) >= View(k).v if View(k).sgn = -1
# confidence in stress is quantified in View(k).c in (0,1)

# prepopulate list
#namedVector  = vector( mode = "numeric" , length = 7 )
#names( namedVector ) = c( "Who" , "Equal" , "Cond_Who" , "Cond_Equal" , "v" , "sgn" , "c " )
#View = list( namedVector ) 
#View = rep( list( namedVector ) , length = 2*N )

emptyMatrix = matrix( , nrow = 0 , ncol = 0 )
View = list()
View$Who = vector( mode="numeric" )
View$Equal = list( matrix( 0 , nrow = 0 , ncol = 0 ) )
View$Cond_Who = emptyMatrix
View$Cond_Equal = list( matrix( 0 , nrow = 0 , ncol = 0 ) )
View$v = numeric(0)
View$sgn = numeric(0)
View$c = numeric(0)
View = rep( list( View ) , length = 2*N + 1)


for ( n in 1:N ) # for each asset...
{
  k = 2 * n - 1 
  View[[k]]$Who = matrix( n , nrow = 1 , ncol = 1 )
  View[[k]]$Equal = list( matrix( -1 , nrow = 1 , ncol = 1 ) )
  View[[k]]$Cond_Who = emptyMatrix 
  View[[k]]$Cond_Equal = list( emptyMatrix )
  View[[k]]$v = .4
  View[[k]]$sgn = -1
  View[[k]]$c = .5

  k = 2 * n
  View[[k]]$Who = matrix( n , nrow = 1 , ncol = 1 )
  View[[k]]$Equal = list( matrix( -1 , nrow = 1 , ncol = 1 ) )
  View[[k]]$Cond_Who = emptyMatrix
  View[[k]]$Cond_Equal = list( emptyMatrix )
  View[[k]]$v = .4
  View[[k]]$sgn = -1
  View[[k]]$c =.5
}

k = 2 * N + 1 
View[[k]]$Who = c( 1 , 2 , 8 ) 
View[[k]]$Equal = list( matrix( c( -1 , 0 ) , nrow = 1 ) , matrix(c(-1,0) , nrow = 1 ) , matrix( 1 , nrow = 1 ) )
View[[k]]$Cond_Who = matrix( 4 , nrow = 1 )
View[[k]]$Cond_Equal = list( matrix( -1 , nrow = 1) )
View[[k]]$v = .9
View[[k]]$sgn = -1
View[[k]]$c = .1

# create linear constraint representation of views on probabilities
constraints = CondProbViews( View , X )
A = constraints$A ; b = constraints$b ; g = constraints$g ; rm( constraints )

# add constraint for view on correlation
C_12_ = .6
New_A = t( X[ , 1 ] * X[ , 2 ] )
New_b = s[1] * s[2] * C_12_ + m[1] * m[2]
New_g = -log( 1 - .1 )

A = rbind( A , New_A )
b = rbind( b , New_b )
g = rbind( g , New_g )
    
# enforce consistency
db = Tweak( A , b , g )
b = b + db

##################################################################################
# compute posterior
Aeq = ones( 1 , J )  # constrain probabilities to sum to one
beq = as.matrix(1)
p_ = EntropyProg( p , A , b , Aeq , beq )
##################################################################################

barplot(p)
barplot(p_)