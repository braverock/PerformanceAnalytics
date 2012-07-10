#' Entropy Pooling Example - Ranking Information script
#'
#' This script performs ranking allocation using the 
#' Entropy-Pooling approach by Attilio Meucci, as it appears in 
#' "A. Meucci - Fully Flexible Views: Theory and Practice -
#' The Risk Magazine, October 2008, p 100-106"
#' available at www.symmys.com > Research > Working Papers

#' Code by A. Meucci, September 2008
#' Last version available at www.symmys.com > Teaching > MATLAB

#############################################################################
# Load panel X of joint returns realizations and vector p of respective probabilities
# In real life, these are provided by the estimation process
#############################################################################
data("ReturnsDistribution")

#############################################################################
# compute and plot efficient frontier based on prior market distribution
#############################################################################
Options = list()
Options$NumPortf = 20 # number of portfolios in efficient frontier
Options$FrontierSpan = c( .3 , .9 ) # range of normalized exp.vals. spanned by efficient frontier

frontierPrior = RIEfficientFrontier( X , P , Options ) # Frontier Plot Data contains [e,s,w,M,S]

# PlotResults( frontierPrior$e , frontierPrior$Sdev , frontierPrior$Composition , frontierPrior$Exps )
plot( x = (frontierPrior$Sdev)^2 , y = frontierPrior$e , xlab = "Variance" , ylab = "Expected Return" , main = "Prior" , type = "l" , ylim = c( .03 , .1 ) )
# create stacked bar chart. each bar is a row (20 rows). each row sums to one. add legend.
options( warn = 0 )
library( ggplot2 )
plotStackedBar <- StackedBarChart( frontierPrior$Composition )
plotStackedBar
options( warn = 2 )
#############################################################################
# process ordering information (this is the core of the Entropy Pooling approach
#############################################################################

# print expected returns of assets 3 and 4
frontierPrior$Exps[3]
frontierPrior$Exps[4] # note that asset 4 has a higher expected return assuming the prior distribution

# the expected return of each entry of Lower is supposed to be smaller than respective entry in Upper
Lower = as.numeric( c( 4 ) )
Upper = as.numeric( c( 3 ) )
P_ = ViewRanking( X , P , Lower , Upper )$p_

# confidence
c = .5 
blendedProbability = (1-c) * P + c * P_

#############################################################################
# compute and plot efficient frontier based on posterior market distribution
#############################################################################

frontierFullConfidencePosterior = RIEfficientFrontier( X , P_ , Options )
# print expected returns of assets 3 and 4
frontierFullConfidencePosterior$Exps[3]
frontierFullConfidencePosterior$Exps[4] # note that asset 3 and asset 4 have equal expected returns

# bar chart of portfolios on frontier -- note asset 3 has substantially more weight vs. asset 4
options( warn = 0 )
library( ggplot2 )
plotStackedBar <- StackedBarChart( frontierFullConfidencePosterior$Composition )
plotStackedBar
options( warn = 2 )

frontierPosterior = RIEfficientFrontier( X , blendedProbability , Options )
# print expected returns of assets 3 and 4
frontierPosterior$Exps[3]
frontierPosterior$Exps[4] # note that asset 4 still has a higher expected return, but less so

plot( x = (frontierPosterior$Sdev)^2 , y = frontierPosterior$e , xlab = "Variance" , ylab = "Expected Return" , main = "Posterior" , type = "l" , ylim = c( .03 , .1 ) )
# PlotResults( frontierPosterior$e , frontierPosterior$Sdev , frontierPosterior$Composition , frontierPosterior$Exps , Lower , Upper )

# bar chart of portfolios on frontier
options( warn = 0 )
library( ggplot2 )
plotStackedBar <- StackedBarChart( frontierPosterior$Composition )
plotStackedBar
options( warn = 2 )

# Tests
# Test1 - views that are already in the prior return no revision
result  = ViewRanking( X , P , c(3,3) , c(4,4) ) # none of the probabilities are revised from 1e-05. Why? Because the expectation that asset 3 is lower than expected return of asset 4 is already satisfied in prior
result2 = ViewRanking( X , P , c(3) , c(4) )     # none of the probabilities are revised from 1e-05

# Test2 - indentical (repeated) views return the same probabilities
result3 = ViewRanking( X , P , c(4) , c(3) )     # returns revised probability distribution
result4 = ViewRanking( X , P , c(4,4) , c(3,3) ) # returns identical probability distribution as in result3

# Test3 - indentical (repeated) views return the same probabilities
result3 = ViewRanking( X , P , c(4) , c(3) )     # returns revised probability distribution
result4 = ViewRanking( X , P , c(4,4) , c(3,3) ) # returns identical probability distribution as in result3

# Test4 - indentical (repeated) views return the same probabilities
result5 = ViewRanking( X , P , c(4) , c(3) )     # returns revised probability distribution
result6 = ViewRanking( X , P , c(4,1) , c(3,2) ) # the second view is non-binding since it is already reflected in prior, so p_ matches result 5

# Test5
result7 = ViewRanking( X , P , c(4,2) , c(3,1) ) # the second view is non-binding since it is already reflected in prior, so p_ matches result 5