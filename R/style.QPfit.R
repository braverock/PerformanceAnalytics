`style.QPfit` <-
function(R.fund, R.style, model=FALSE, leverage = FALSE, ...) 
{
# INPUTS
# R.fund   Vector of a fund return time series
# R.style   Matrix of a style return time series
# 
# 
# OUTPUTS
# weights   Vector of optimal style index weights
# @ todo: TE  Tracking error between the calc'd and actual fund
# @ todo: Fp  Vector of calculated fund time series
# @ todo: R^2  Coefficient of determination
#
# 
# w_i   Style weights
# V   Variance-covariance matrix of style index matrix
# C   Vector of covariances between the style index and the fund
# e   Vector of 1's
# n   Number of style indexes
# 
# To implement style analysis as described in:
# http://www.stanford.edu/~wfsharpe/art/sa/sa.htm
# here's what we're trying to do:
# min VAR(R.f - SUM[w_i * R.s_i]) = min VAR(F - w*S)
#   s.t. SUM[w_i] = 1; w_i > 0
# 
# Remembering that VAR(aX + bY) = a^2 VAR(X) + b^2 VAR(Y) + 2ab COV(X,Y), 
# we can refactor our problem as:
# 
# = VAR(R.f) + w'*V*w - 2*w'*COV(R.f,R.s)
# 
# drop VAR[R.f] as it isn't a function of weights, multiply by 1/2:
# 
# = min (1/2) w'*V*w - C'w
#   s.t. w'*e = 1, w_i > 0
# 
# Now, map that to the inputs of solve.QP, which is specified as:
# min(-d' b + 1/2 b' D b) with the constraints A' b >= b_0
# 
# so:
# b is the weight vector,
# D is the variance-covariance matrix of the styles
# d is the covariance vector between the fund and the styles
#

    # Check to see if the required libraries are loaded
    if(!require("quadprog", quietly=TRUE))
        stop("package", sQuote("quadprog"), "is needed.  Stopping")

    R.fund = checkData(R.fund[,1,drop=FALSE], method = "zoo")
    R.style = checkData(R.style, method = "zoo")

    # @todo: Missing data is not allowed, use = "pairwise.complete.obs" ?
    style.rows = dim(R.style)[1]
    style.cols = dim(R.style)[2]

    # Calculate D and d
    Dmat = cov(R.style, use = "pairwise.complete.obs")
    dvec = cov(R.fund, R.style, use = "pairwise.complete.obs")

    # To specify A' b >= b_0, we create an nxn matrix Amat and the constraints 
    # vector b0.  First we tackle Amat.  

    # If we do not allow leverage, the first constraint listed above is 
    # SUM[w_i] = 1.  The left side of the constraint is expressed as a vector 
    # of 1's:
    if(!leverage)
        a1 = rep(1, style.cols)

    # In b0, which represents the right side of the equation, this vector is 
    # paired with the value '1'.

    # The second constraint sets the lower bound of the weights to zero.  First
    # we set up an identity matrix.  
    a2 = matrix(0, style.cols, style.cols)
    diag(a2) = 1

    # It's paired in b0 with a vector of lower bounds set to zeros:
    w.min = rep(0, style.cols)

    # Construct A from the two components a1 and a2
    # Construct b_0
    if(!leverage){
        Amat = t(rbind(a1, a2))
        b0 = c(1, w.min)
    }
    else {
        Amat = t(a2)
        b0 = w.min
    }

    # This is where 'meq' comes in.  The ?solve.QP page says:
    #     meq: the first 'meq' constraints are treated as equality
    #     constraints, all further as inequality constraints (defaults
    #     to 0).
    # I think that the way to interpret this is: if it is set to a value 'q' <= n,
    # the ordered constraints numbered less than or equal to 'q' are treated as an 
    # equality constraint.  In this case, we only want to first constraint to be
    # treated as an equality, so that the weights would sum to exactly 1.  So we
    # set meq = 1.

    # With 'meq' set to 1, the second constraint (a2) is treated as an inequality,
    # so each weight is constrainted to be greater than or equal to zero.
    optimal <- solve.QP(Dmat, dvec, Amat, bvec=b0, meq=1)

    weights = as.data.frame(optimal$solution)
    rownames(weights) = colnames(R.style)
    colnames(weights) = colnames(R.fund)[1]

    # Calculate metrics for the quality of the fit
    R.fitted = rowSums(R.style*matrix(rep(t(weights),style.rows),byrow=TRUE,ncol=style.cols))
    R.nonfactor = R.fund - R.fitted

    R.squared = as.data.frame(1 - (var(R.nonfactor, na.rm=TRUE)/var(R.fund, na.rm=TRUE)))
#     adj.R.squared = as.data.frame(1 - (1 - R.squared)*(style.rows - 1)/(style.rows - style.cols - 1))

    rownames(R.squared) = "R-squared"
#     rownames(adj.R.squared) = "Adj R-squared"

    if(model) 
        result = optimal
    else 
        result = list(weights = weights, R.squared = R.squared) #, adj.R.squared = adj.R.squared )

    # @todo: retain the labels on the weights
    # @todo: add other values to output, e.g., 
    #    result <- list(weights = optimal$solution, R.squared = , tracking.error = )

    return(result)


    # EXAMPLE:
    # > head(R.fund)
    #          SP500.TR
    # Jan 1996   0.0340
    # Feb 1996   0.0093
    # Mar 1996   0.0096
    # Apr 1996   0.0147
    # May 1996   0.0258
    # Jun 1996   0.0038
    # > head(R.style)
    #          Russell.1000.Growth Russell.1000.Value
    # Jan 1996              0.0335             0.0312
    # Feb 1996              0.0183             0.0076
    # Mar 1996              0.0013             0.0170
    # Apr 1996              0.0263             0.0038
    # May 1996              0.0349             0.0125
    # Jun 1996              0.0014             0.0008
    # > style.QPfit(R.fund, R.style)
    # [1] 0.5047724 0.4952276
    # > style.QPfit(R.fund, R.style, all=T)
    # $solution
    # [1] 0.5047724 0.4952276
    # 
    # $value
    # [1] -0.0008888153
    # 
    # $unconstrainted.solution
    # [1] 0.5040111 0.4815228
    # 
    # $iterations
    # [1] 2 0
    # 
    # $iact
    # [1] 1
    # 

}