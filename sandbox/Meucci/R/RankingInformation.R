# TODO: translate PlotResults function
# TODO: update plot of efficient frontier to show maximum return case
# TODO: fix StackedBarChart function
# TODO: add max weights constraint to EfficientFrontier()
# TODO: add computeCVaR to EfficientFrontier()

# TODO: confirm QuadProg does not have a bug (i.e. it can optimize expected returns without use dvec by adding an equality constraint)

#' @param      a matrix of weights where rows are efficient portfolios summing to one, and columns are assets
#' @param      a string indicating the title of the chart
#' TODO FIXME check against function in Performanceanalytics, we probably want to use that one  unless there's a reaso to use this
StackedBarChart = function( weightsMatrix )
{
  data = as.data.frame( weightsMatrix )
  data$aspect = 1:nrow(data)
  data2 = reshape2:::melt( data , id.vars = "aspect" )
  p <- ggplot(data2, aes(x=factor(aspect), y = value, fill=factor(variable))) + geom_bar() #+ opts( title = expression( "Efficient Frontier Weights" ))
  return( p )
}

#' @param  Lower    a vector of indexes indicating which column is lower than the corresponding column number in Upper
#' @param  Upper    a vector of indexes indicating which column is lower than the corresponding column number in Upper
# @example ViewRanking( X , p , Lower = c(3,4) , Upper = c(4,5) ) # two inequality views: asset 3 < asset 4 returns, and asset 4 < asset 5 returns
ViewRanking = function( X , p , Lower , Upper )
{
  library( matlab )
  J = nrow( X )
  N = ncol( X )
    
  K = length( Lower )
    
  # constrain probabilities to sum to one across all scenarios...
  Aeq = ones( 1 , J )
  beq = 1
    
  # ...constrain the expectations... A*x <= 0
  # X[,Lower] refers to the column of returns for Asset-lower
  # X[,Upper] refers to the column of returns for Asset-lower
  # X[ , Lower ] - X[ , Upper ] is vector returns of the "lower"" asset less the returns of the "higher" asset
  V = X[ , Lower ] - X[ , Upper ] # Jx1 vector. Expectation is assigned to each scenario
    
  A = t( V )
  b = 0 # The expectation is that (Lower - Upper)x <= 0. (i.e. The returns of upper are greater than zero for each scenario)
    
  # ...compute posterior probabilities
  p_ = EntropyProg( p , A , as.matrix(b) , Aeq , as.matrix(beq) )
    
  return( p_ )
}

#' @param  X             a matrix with the joint-scenario probabilities by asset (rows are joint-scenarios, columns are assets)
#' @param  p             a vector of probabilities associated with each scenario in matrix X
#' @param  Options       a list of options....TBD
#' @return A list with NumPortf efficient portfolios whos returns are equally spaced along the whole range of the efficient frontier
#'          Exps          the NumPortf x 1 vector of expected returns for each asset
#'          Covs          the NumPortf x N vector of security volatilities along the efficient frontier
#'             w          the NumPortf x N matrix of compositions (security weights) for each portfolio along the efficient frontier
#'             e          the NumPortf x 1 matrix of expected returns for each portfolio along the efficient frontier
#'             s          the NumPortf x 1 matrix of standard deviation of returns for each portfolio along the efficient frontier
EfficientFrontier = function( X , p , Options)
{    
  library( matlab )
    
  J = nrow( X ) # number of scenarios
  N = ncol( X ) # number of assets
    
  Exps = t(X) %*% p # probability-weighted expected return of each asset
    
  Scnd_Mom = t(X) %*% (X * ( p %*% ones( 1 , N ) ) )
  Scnd_Mom = ( Scnd_Mom + t(Scnd_Mom) ) / 2 # an N*N matrix
  Covs = Scnd_Mom - Exps %*% t( Exps )
    
  Constr = list()
    
  # constrain the sum of weights to 1
  Constr$Aeq = ones( 1 , N )
  Constr$beq = 1
    
  # constrain the weight of any security to between 0 and 1
  Constr$Aleq = rbind( eye(N) , -eye(N) ) # linear coefficients matrix A in the inequality constraint A*x <= b
  Constr$bleq = rbind( ones(N,1) , 0*ones(N,1) ) # constraint vector b in the inequality constraint A*x <= b
    
  Amat = rbind( Constr$Aeq , Constr$Aleq ) # stack the equality constraints on top of the inequality constraints
  bvec = rbind( Constr$beq , Constr$bleq ) # stack the equality constraints on top of the inequality constraints
    
  ############################################################################################
  # determine return of minimum-risk portfolio
  FirstDegree = zeros( N , 1 ) # TODO: assumes that securities have zero expected returns when computing efficient frontier?
  SecondDegree = Covs
  library( quadprog )    
  # Why is FirstDegree "expected returns" set to 0? 
  # We capture the equality view in the equality constraints matrix
  # In other words, we have a constraint that the Expected Returns by Asset %*% Weights = Target Return
  MinVol_Weights = solve.QP( Dmat = SecondDegree , dvec = -1*FirstDegree , Amat = -1*t(Amat) , bvec = -1*bvec , meq = length( Constr$beq ) )
  MinSDev_Exp = t( MinVol_Weights$solution ) %*% Exps
    
  ############################################################################################
  # determine return of maximum-return portfolio
  FirstDegree = -Exps
  library( limSolve )
  MaxRet_Weights = linp( E = Constr$Aeq , F = Constr$beq , G = -1*Constr$Aleq , H = -1*Constr$bleq , Cost = FirstDegree , ispos = FALSE )$X
  MaxExp_Exp = t( MaxRet_Weights) %*% Exps
    
  ############################################################################################
  # slice efficient frontier in NumPortf equally thick horizontal sections
  Grid = matrix( , ncol = 0 , nrow = 0 )
  Grid = t( seq( from = Options$FrontierSpan[1] , to = Options$FrontierSpan[2] , length.out = Options$NumPortf ) )
    
  # the portfolio return varies from a minimum of MinSDev_Exp up to a maximum of MaxExp_Exp
  # We establish equally-spaced portfolio return targets and use this find efficient portfolios 
  # in the next step
  Targets = as.numeric( MinSDev_Exp ) + Grid * as.numeric( ( MaxExp_Exp - MinSDev_Exp ) ) 
    
  ############################################################################################
  # compute the NumPortf compositions and risk-return coordinates        
  FirstDegree = zeros( N , 1 )
    
  w = matrix( , ncol = N , nrow = 0 )
  e = matrix( , ncol = 1 , nrow = 0 )
  s = matrix( , ncol = 1 , nrow = 0 )       
    
  for ( i in 1:Options$NumPortf )
  {
    # determine least risky portfolio for given expected return
    # Ax = b ; Exps %*% weights = Target Return
    AEq = rbind( Constr$Aeq , t( Exps ) )     # equality constraint: set expected return for each asset...
    bEq = rbind( Constr$beq , Targets[ i ] )  # ...and target portfolio return for i'th efficient portfolio
        
    Amat = rbind( AEq , Constr$Aleq ) # stack the equality constraints on top of the inequality constraints
    bvec = rbind( bEq , Constr$bleq )
        
    Weights = solve.QP( Dmat = SecondDegree , dvec = -1*FirstDegree , Amat = -1*t(Amat) , bvec = -1*bvec , meq = length( bEq ) )
        
    w = rbind( w , Weights$solution )
    s = rbind( s , sqrt( t(Weights$solution) %*% Covs %*% Weights$solution ) )
    e = rbind( e , Weights$solution %*% Exps )
  }
    
  return( list( e = e , Sdev = s , Composition = w , Exps = Exps , Covs = Covs ) )    
}
