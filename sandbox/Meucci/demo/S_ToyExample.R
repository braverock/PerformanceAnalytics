# This toy example illustrates the use of Entropy Pooling to compute Fully Flexible Bayesian networks, see 
# A. Meucci (2010) "Fully Flexible Bayesian Networks", working paper
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/152

#  set up scenarios table and prior distribution

x_1 = cbind(1, 2, 3)
x_2 = cbind(1, 2)
x_3 = cbind(1, 2)
emptyMatrix = matrix( , nrow = 0 , ncol = 0 )

X = matrix( , nrow = 1, ncol =3 )
for ( i in 1:length(x_1) )
{
  for ( k in 1:length(x_2) )
  {
    for ( l in 1:length(x_3) )
    {
      X = rbind(X, cbind( x_1[i], x_2[k], x_3[l]));
    }
  }
}

X = X[-1,]
J = nrow( X ) ; N = ncol( X )
p = ones( J , 1 ) / J

# input views
# statement: View(k).Who (e.g. [1 3])= View(k).Equal (e.g. {[2 3] [1 3 5]})
# optional conditional statement: View(k).Cond_Who (e.g. [2])= View(k).Cond_Equal (e.g. {[1]})
# amount of stress is quantified as Prob(statement) <= View(k).v if View(k).sgn = 1;
#                                   Prob(statement) >= View(k).v if View(k).sgn = -1;
# confidence in stress is quantified in View(k).c in (0,1)

View = list()
View$Who = vector( mode="numeric" )
View$Equal = list( matrix( 0 , nrow = 0 , ncol = 0 ) )
View$Cond_Who = emptyMatrix
View$Cond_Equal = list( matrix( 0 , nrow = 0 , ncol = 0 ) )
View$v = numeric(0)
View$sgn = numeric(0)
View$c = numeric(0)
View = rep( list( View ) , length = 2)

View[[1]]$Who = matrix( 1 , nrow = 1 , ncol = 1 )
View[[1]]$Equal = list( matrix( cbind(2, 3) , nrow = 1 , ncol = 2 ) )
View[[1]]$Cond_Who = matrix( 2 , nrow = 1 , ncol = 1 )
View[[1]]$Cond_Equal = list( matrix( 1 , nrow = 1 , ncol = 1 ) )
View[[1]]$v = .7
View[[1]]$sgn = -1
View[[1]]$c = .5

View[[2]]$Who = matrix( 2 , nrow = 1 , ncol = 1 )
View[[2]]$Equal = list( matrix( 1 , nrow = 1 , ncol = 2 ) )
View[[2]]$Cond_Who = emptyMatrix
View[[2]]$Cond_Equal = list( emptyMatrix )
View[[2]]$v = .3
View[[2]]$sgn = -1
View[[2]]$c = .5

# create linear constraint representation of views
constraints = CondProbViews( View , X )
A = constraints$A ; b = constraints$b ; g = constraints$g ; rm( constraints )

# enforce consistence
db = Tweak(A, b, g)
b = b + db

# compute posterior
Aeq = ones(1,J)  # constrain probabilities to sum to one
beq = 1

# compute posterior probabilities
p_ = EntropyProg(p,A,b,Aeq ,beq)

barplot(p_)