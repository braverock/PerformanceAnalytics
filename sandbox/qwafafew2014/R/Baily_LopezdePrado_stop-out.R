# --------------------------------------------------------------------
# Max Quartile Loss at Confidence level MLdP
# --------------------------------------------------------------------

MaxQL<-function(R, confidence=0.95, type=c("ac","normal"), ...) {
  # @TODO Handle multi-column output
  if(!is.vector(R))
    x = checkData(R[,1])
  else
    x = checkData(R)
  x = na.omit(x)
  if(type[1]=="ac"){  
    mu = mean(x, na.rm = TRUE)
    sigma_infinity = StdDev(x)
    phi = cov(x[-1],x[-length(x)])/(cov(x[-length(x)]))
    phi=.5
    sigma = sigma_infinity*((1-phi^2)^0.5)
    dPi0 = 0
    minQ = minQ(phi, mu, sigma, dPi0, confidence)
  }
  if(type[1]=="normal"){
    minQ = minQ_norm(x, confidence)
  }
  MaxQL=min(0,minQ[[1]])
#   rownames(MaxQL) = paste0("MaxQL (", confidence*100, "%)") #,"t*")
  return(MaxQL)  
}

# --------------------------------------------------------------------
# Max Quartile Loss at Confidence level - First-order AC 
# --------------------------------------------------------------------
getQ <- function(bets, phi, mu, sigma, dPi0, confidence) {
  # Compute analytical solution to quantile
  #1) Mean (eq 15)
  mean=(phi^(bets+1)-phi)/(1-phi)*(dPi0-mu)+mu*bets  # wrong?
  #2) Variance (eq 15)
  var=sigma^2/(phi-1)^2
  var=var*((phi^(2*(bets+1))-1)/(phi^2-1)-2*(phi^(bets+1)-1)/(phi-1)+bets+1)
  #3) Quantile
  q=mean+qnorm(1-confidence)*(var^0.5)
  #print(sprintf("bets %g, mean %g, var %g, var1 %g, var2 %g, var3 %g, q %g", bets, mean, var, var1, var2, var3, q))
  q
}

goldenSection<-function(a, b, FUN, minimum = TRUE, ...) {
  FUN = match.fun(FUN)
  tol = 10^-9
  sign = 1
  
  if(minimum) sign = -1
  N = round(ceiling(-2.078087*log(tol/abs(b-a))))
  r = 0.618033989
  c = 1.0 - r
  x1 = r*a + c*b
  x2 = c*a + r*b
  f1 = sign * FUN(x1,...=...)
  f2 = sign * FUN(x2,...=...)
  #print(f1); print(f2)
  for(i in 1:N){
    if(f1>f2){
      a = x1
      x1 = x2
      f1 = f2
      x2 = c*a+r*b
      f2 = sign*FUN(x2,...=...)
    } else {
      b = x2
      x2 = x1
      f2 = f1
      x1 = r*a + c*b
      f1 = sign*FUN(x1,...=...)
    }
  }
  if(f1 < f2){
    return(list(minQ=sign*f1, t=x1))
  } else {
    return(list(minQ=sign*f2, t=x2))
  }
}

minQ <- function(phi, mu, sigma, dPi0, confidence) {
  q = 0
  bets = 0
  while (q <= 0) {
    bets = bets + 1
    q = getQ(bets, phi, mu, sigma, dPi0, confidence)
  }
  #print(sprintf("bets %g, q %g", bets, q))
  goldenSection(0,bets,getQ,FALSE,phi=phi,mu=mu,sigma=sigma,dPi0=dPi0,confidence=confidence)
}

# minQ(0.5, 1, 2, 1, 0.95)
# MinQ = -9.15585580378
# Time at MinQ = 12.4832517718

# --------------------------------------------------------------------
# Max Quartile Loss at Confidence level - Assuming IID (eq. 5)
# --------------------------------------------------------------------

minQ_norm<-function(x, confidence){
# Calculate the maximum drawdown for a normal distribution, assuming iid returns
  x = na.omit(x)
  sd = StdDev(x)
  mu = mean(x, na.rm = TRUE)
  minQ = -((qnorm(1-confidence)*sd)^2)/(4*mu)
  t = ((qnorm(1-confidence)*sd)/(2*mu))^2
  return(list(minQ=minQ,t=t))
}
