#!/usr/bin/env python
# PSR class for computing the Probabilistic Sharpe Ratio
# On 20120502 by MLdP <lopezdeprado@lbl.gov>

from scipy.stats import norm
#-------------------------------------------
# PSR class
class PSR:
    def __init__(self,stats,sr_ref,obs,prob):
        self.PSR=0
        self.minTRL=0
        self.stats=stats
        self.sr_ref=sr_ref
        self.obs=obs
        self.prob=prob
#-------------------------------------------
    def set_PSR(self,moments):
        stats=[0,0,0,3]
        stats[:moments]=self.stats[:moments]
        sr=self.stats[0]/self.stats[1]
        self.PSR=norm.cdf((sr-self.sr_ref)*(self.obs-1)**0.5/(1-sr*stats[2]+sr**2*(stats[3]-1)/4.)**0.5)
#-------------------------------------------
    def set_TRL(self,moments):
        stats=[0,0,0,3]
        stats[:moments]=self.stats[:moments]
        sr=self.stats[0]/self.stats[1]
        self.minTRL=1+(1-stats[2]*sr+(stats[3]-1)/4.*sr**2)*(norm.ppf(self.prob)/(sr-self.sr_ref))**2
#-------------------------------------------
    def get_PSR(self,moments):
        self.set_PSR(moments)
        return self.PSR
#-------------------------------------------
    def get_TRL(self,moments):
        self.set_TRL(moments)
        return self.minTRL
#-------------------------------------------
#-------------------------------------------
# Main function
def main():
    #1) Inputs (stats on excess returns)
    stats=[1.5,12**0.5,-0.72,5.78]
    #non-annualized stats
    sr_ref=1/(12**0.5)
    #reference Sharpe ratio (non-annualized)
    obs=59.895
    prob=0.95
    #2) Create class
    psr=PSR(stats,sr_ref,obs,prob)
    #3) Compute and report values
    print 'PSR(2m,3m,4m):',[psr.get_PSR(i) for i in range(2,5,1)]
    print 'minTRL(2m,3m,4m):',[psr.get_TRL(i) for i in range(2,5,1)]
#-------------------------------------------
# Boilerplate
if __name__=='__main__': main()

