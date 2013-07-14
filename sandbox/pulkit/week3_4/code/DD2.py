#!/usr/bin/env python
# On 20121230
# Get maximum drawdown
# by MLdP <lopezdeprado@lbl.gov>
from scipy.stats import norm
#---------------------------------------------------------------
def main():
	#1) Parameters
	phi=.5 # AR(1) coefficient
	mu=1 # unconditional mean
	sigma=2 # Standard deviation of the random shock
	dPi0=1 # Bet at origin (initialization of AR(1))
	confidence=.95 # Confidence level for quantile
	#2) Compute MinQ
	t,minQ=getMinQ(phi,mu,sigma,dPi0,confidence)
	print 'MinQ = '+str(minQ)
	print 'Time at MinQ = '+str(t)
	print 'MaxDD = '+str(max(0,-minQ))
	return
#---------------------------------------------------------------
def getMinQ(phi,mu,sigma,dPi0,confidence):
	# Compute MinQ
	q,bets=0,0
	#1) Determine extremes of search
	while not q>0:
		bets+=1
		q=getQ(bets,phi,mu,sigma,dPi0,confidence)
	#2) Compute min of q
	kargs={'args':(phi,mu,sigma,dPi0,confidence)}
	t,minQ=goldenSection(getQ,0,bets,**kargs)
	return t,minQ
#---------------------------------------------------------------
def getQ(bets,phi,mu,sigma,dPi0,confidence):
	# Compute analytical solution to quantile
	#1) Mean
	mean=(phi**(bets+1)-phi)/(1-phi)*(dPi0-mu)+mu*bets
	#2) Variance
	var=sigma**2/(phi-1)**2
	var*=(phi**(2*(bets+1))-1)/(phi**2-1)-2*(phi**(bets+1)-1)/(phi-1)+bets+1
	#3) Quantile
	q=mean+norm.ppf(1-confidence,0,1)*var**.5
	return q
#---------------------------------------------------------------
def goldenSection(obj,a,b,**kargs):
	# Golden section method. Maximum if kargs['minimum']==False is passed 
	from math import log,ceil
	tol,sign,args=1.0e-9,1,None
	if 'minimum' in kargs and kargs['minimum']==False:sign=-1
	if 'args' in kargs:args=kargs['args']
	numIter=int(ceil(-2.078087*log(tol/abs(b-a))))
	r=0.618033989
	c=1.0-r
	# Initialize
	x1=r*a+c*b;x2=c*a+r*b
	f1=sign*obj(x1,*args);f2=sign*obj(x2,*args)
	# Loop
	for i in range(numIter):
		if f1>f2:
			a=x1
			x1=x2;f1=f2
			x2=c*a+r*b;f2=sign*obj(x2,*args)
		else:
			b=x2
			x2=x1;f2=f1
			x1=r*a+c*b;f1=sign*obj(x1,*args)
	if f1<f2:return x1,sign*f1
	else:return x2,sign*f2
#---------------------------------------------------------------
# Boilerplate
if __name__=='__main__':main()
