# Input views
# statement: View(k).Who (e.g. [1 3])= View(k).Equal (e.g. {[2 3] [1 3 5]})
# optional conditional statement: View(k).Cond_Who (e.g. [2])= View(k).Cond_Equal (e.g. {[1]})
# amount of stress is quantified as Prob(statement) <= View(k).v if View(k).sgn = 1;
#                                   Prob(statement) >= View(k).v if View(k).sgn = -1;
# confidence in stress is quantified in View(k).c in (0,1)
#' @param View              TBD
#' @param X                 TBD
#'
#' @return A                TBD
#' @return b                TBD
#' @return g                TBD
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
CondProbViews = function( View , X ) 
{    
    # initialize parameters    
    A = matrix( , nrow = 0 , ncol = nrow( X ) )
    b = g = matrix( , nrow = 0 , ncol = 1 )    
    
    # for each view...    
    for ( k in 1:length( View ) ) {
    
            I_mrg = ( X[ , 1] < Inf ) 
            
            for ( s in 1:length( View[[k]]$Who ) ) 
                {
                Who = View[[k]]$Who[s]
                Or_Targets = View[[k]]$Equal[[s]]
                I_mrg_or = ( X[ , Who] > Inf )
                for ( i in 1:length( Or_Targets ) ) { I_mrg_or = I_mrg_or | ( X[ , Who ] == Or_Targets[i] ) } # element-wise logical OR
                I_mrg = I_mrg & I_mrg_or # element-wise logical AND
                }
    
        I_cnd = ( X[ , 1 ] < Inf )        

        if ( length( View[[k]]$Cond_Who ) != 0) # If length of Cond_Who is zero, skip
        { 
        for ( s in 1:length( View[[k]]$Cond_Who ) ) 
            {
            Who = View[[k]]$Cond_Who[s]
            Or_Targets = View[[k]]$Cond_Equal[[s]]
            I_cnd_or = ( X[ , Who ] > Inf )
            for ( i in 1:length( Or_Targets ) ) { I_cnd_or = I_cnd_or | X[ ,Who ] == Or_Targets[i] }
            I_cnd = I_cnd & I_cnd_or
            }
        }
        
        I_jnt=I_mrg & I_cnd
        
        if ( !isempty( View[[k]]$Cond_Who ) ) 
            {
            New_A = View[[k]]$sgn %*% t( (I_jnt - View[[k]]$v * I_cnd) )
            New_b = 0
            }
        else 
            {
            New_A = View[[k]]$sgn %*% t( I_mrg ) 
            New_b = View[[k]]$sgn %*% View[[k]]$v
            }
        
        A = rbind( A , New_A ) # constraint for the conditional expectations...
        b = rbind( b , New_b ) 
        g = rbind( g , -log( 1 - View[[k]]$c ) )        
    }
    return( list( A = A , b = b , g = g ) )
}

#' @param   A     matrix A consisting of inequality constraints ( Ax <= b )
#' @param   b     matrix b consisting of inequality constraint vector b ( Ax <= b )
#' @param   g     TODO: TBD
#'
#' @result db
#' @export
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
Tweak = function( A , b , g )
{
    library( matlab )
    library( limSolve )
    
    K = nrow( A )
    J = ncol( A )
    
    browser()
    
    g_ = rbind( g , zeros( J , 1 ) )
    
    Aeq_ = cbind( zeros( 1 , K ) , ones( 1 , J ) )
    beq_ = 1
    
    lb_ = rbind( zeros( K , 1 ) , zeros( J , 1 ) )
    ub_ = rbind( Inf * ones( K , 1 ) , ones( J , 1 ) )
    
    A_ = cbind( -eye( K ) , A )
    b_ = b
    
    # add lower-bound and upper-bound constraints
    A_ = rbind( A_ , -eye(ncol(A_)) )
    b_ = rbind( b_ , rep( 0 , ncol(A_) )
    
    x0 = rep( 1/ncol( Aeq_ ) , ncol( Aeq_ ) )
    # db_ = linprog( g_ , A_ , b_ , Aeq_ ,beq_ , lb_ , ub_ ) # MATLAB version
    optimResult = linp( E = Aeq_ ,     # matrix containing coefficients of equality constraints Ex=F
                        F = beq_ ,     # vector containing the right-hand side of equality constraints
                        G = -1*A_ ,    # matrix containint coefficients of the inequality constraints GX >= H
                        H = -1*b_ ,    # vector containing the right-hand side of the inequality constraints
                        Cost = -1*g_ , # vector containing the coefficients of the cost function
                        ispos = FALSE )
                
    costFunction = function( x ) { matrix( x , nrow = 1 ) %*% matrix( -1*g_ , ncol = 1) }
    optimResult = optim( par = x0 ,
                         fn = costFunction , # CHECK
                         gr = -1*g_ ,
                         method = "L-BFGS-B",
                         lower = lb_ ,
                         upper = ub_ ,
                         hessian = FALSE )
    
    
    library( linprog )
    optimResult2 = solveLP( E = Aeq_ ,   # numeric matrix containing coefficients of equality constraints Ex=F
                        F = beq_ ,   # numeric vector containing the right-hand side of equality constraints
                        G = -1*A_ ,  # numeric matrix containint coefficients of the inequality constraints GX >= H
                        H = -1*b_ ,  # numeric vector containing the right-hand side of the inequality constraints
                        Cost = -g_ , # numeric vector containing the coefficients of the cost function
                        ispos = FALSE )
    
    
    db_ = optimResult$X
    
    db = db_[ 1:K ]
    
    return( db )
}


#' Takes a matrix of joint-scenario probability distributions and generates expectations, standard devation, and correlation matrix for the assets
#'
#' Takes a matrix of joint-scenario probability distributions and generates expectations, standard devation, and correlation matrix for the assets
#'
#' @param X     a matrix of joint-probability scenarios (rows are scenarios, columns are assets)
#' @param p     a numeric vector containing the probabilities for each of the scenarios in the matrix X
#'
#' @return means                 a numeric vector of the expectations (probability weighted) for each asset
#' @return sd                    a numeric vector of standard deviations corresponding to the assets in the covariance matrix
#' @return correlationMatrix     the correlation matrix resulting from converting the covariance matrix to a correlation matrix
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
ComputeMoments = function( X , p )
{
    library( matlab )
    J = nrow( X ) ; N = ncol( X )
    m = t(X) %*% p
    Sm = t(X) %*% (X * repmat( p , 1 , N ) ) # repmat : repeats/tiles a matrix
    S = Sm - m %*% t(m)
    C = cov2cor( S ) # the correlation matrix
    s = sqrt( diag( S ) ) # the vector of standard deviations    
    return( list( means = m , sd = s , correlationMatrix = C ) )
}


# This case study uses Entropy Pooling to compute Fully Flexible Bayesian networks for risk management, see 
# A. Meucci (2010) "Fully Flexible Bayesian Networks", working paper
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/152

##################################################################################
# upload scenarios table and prior distribution for changes in
# SWAP2YR SWAP10YR CDXIG S&P500 DollarIndex Crude Gold VIX 10YRInflationSwapRate
library( matlab )
base:::load( "MeucciFreaqEst.rda" ) ; Names = unlist( Names )
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
New_g = -log( 1 - .1 ) # TODO?

A = rbind( A , New_A )
b = rbind( b , New_b )
g = rbind( g , New_g )
    
# enforce consistency
db = Tweak( A , b , g ) # TODO
b = b + db

##################################################################################
# compute posterior
Aeq = ones( 1 , J )  # constrain probabilities to sum to one
beq = 1
p_ = EntropyProg( p , A , b , Aeq , beq )
##################################################################################

# plots TODO

#figure
#subplot(2,1,1)
#bar(p)
#YLim=get(gca,'ylim');
#subplot(2,1,2)
#bar(p_)
#set(gca,'ylim',YLim);