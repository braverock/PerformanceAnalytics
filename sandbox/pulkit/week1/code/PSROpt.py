#!/usr/bin/env python
# PSR class for Portfolio Optimization
# On 20121128 by MLdP <lopezdeprado@lbl.gov>

import numpy as np
import time
#-------------------------------------------
#-------------------------------------------
class PSR_Opt:
    def __init__(self,series,seed,delta,maxIter,bounds=None):
        # Construct the object
        self.series,self.w,self.delta=series,seed,delta
        self.z,self.d1Z=None,[None for i in range(series.shape[1])]
        self.maxIter,self.iter,self.obs=maxIter,0,series.shape[0]
        if len(bounds)==None or seed.shape[0]!=len(bounds):
            self.bounds=[(0,1) for i in seed]
        else:
            self.bounds=bounds
#-------------------------------------------
    def optimize(self):
        # Optimize weights
        mean=[self.get_Moments(self.series[:,i],1) for i in range(self.series.shape[1])]
        w=np.array(self.w)
        # Compute derivatives
        while True:
            if self.iter==self.maxIter:break
            # Compute gradient
            d1Z,z=self.get_d1Zs(mean,w)
            # Evaluate result
            if z>self.z and self.checkBounds(w)==True:
                # Store new local optimum
                self.z,self.d1Z=z,d1Z
                self.w=np.array(w)
            # Find direction and normalize
            self.iter+=1
            w=self.stepSize(w,d1Z)
            if w==None:return
        return
#-------------------------------------------
    def checkBounds(self,w):
        # Check that boundary conditions are satisfied
        flag=True
        for i in range(w.shape[0]):
            if w[i,0]<self.bounds[i][0]:flag=False
            if w[i,0]>self.bounds[i][1]:flag=False
        return flag
#-------------------------------------------
    def stepSize(self,w,d1Z):
        # Determine step size for next iteration
        x={}
        for i in range(len(d1Z)):
            if d1Z[i]!=0:x[abs(d1Z[i])]=i
        if len(x)==0:return
        index=x[max(x)]
        w[index,0]+=self.delta/d1Z[index]
        w/=sum(w)
        return w
#-------------------------------------------
    def get_d1Zs(self,mean,w):
        # First order derivatives of Z
        d1Z=[0 for i in range(self.series.shape[1])]
        m=[0 for i in range(4)]
        series=np.dot(self.series,w)[:,0]
        m[0]=self.get_Moments(series,1)
        for i in range(1,4):m[i]=self.get_Moments(series,i+1,m[0])
        stats=self.get_Stats(m)
        meanSR,sigmaSR=self.get_SR(stats,self.obs)
        for i in range(self.series.shape[1]):
            d1Z[i]=self.get_d1Z(stats,m,meanSR,sigmaSR,mean,w,i)
        return d1Z,meanSR/sigmaSR
#-------------------------------------------
    def get_d1Z(self,stats,m,meanSR,sigmaSR,mean,w,index):
        # First order derivatives of Z with respect to index
        d1Mu=self.get_d1Mu(mean,index)
        d1Sigma=self.get_d1Sigma(stats[1],mean,w,index)
        d1Skew=self.get_d1Skew(d1Sigma,stats[1],mean,w,index,m[2])
        d1Kurt=self.get_d1Kurt(d1Sigma,stats[1],mean,w,index,m[3])
        d1meanSR=(d1Mu*stats[1]-d1Sigma*stats[0])/stats[1]**2
        d1sigmaSR=(d1Kurt*meanSR**2+2*meanSR*d1meanSR*(stats[3]-1))/4
        d1sigmaSR-=d1Skew*meanSR+d1meanSR*stats[2]
        d1sigmaSR/=2*sigmaSR*(self.obs-1)
        d1Z=(d1meanSR*sigmaSR-d1sigmaSR*meanSR)/sigmaSR**2
        return d1Z
#-------------------------------------------
    def get_d1Mu(self,mean,index):
        # First order derivative of Mu
        return mean[index]
#-------------------------------------------
    def get_d1Sigma(self,sigma,mean,w,index):
        # First order derivative of Sigma
        return self.get_dnMoments(mean,w,2,1,index)/(2*sigma)
#-------------------------------------------    
    def get_d1Skew(self,d1Sigma,sigma,mean,w,index,m3):
        # First order derivative of Skewness
        d1Skew=self.get_dnMoments(mean,w,3,1,index)*sigma**3
        d1Skew-=3*sigma**2*d1Sigma*m3
        d1Skew/=sigma**6
        return d1Skew
#-------------------------------------------    
    def get_d1Kurt(self,d1Sigma,sigma,mean,w,index,m4):
        # First order derivative of Kurtosis
        d1Kurt=self.get_dnMoments(mean,w,4,1,index)*sigma**4
        d1Kurt-=4*sigma**3*d1Sigma*m4
        d1Kurt/=sigma**8
        return d1Kurt
#-------------------------------------------    
    def get_dnMoments(self,mean,w,mOrder,dOrder,index):
        # Get dOrder derivative on mOrder mean-centered moment with respect to w index
        x0,sum=1.,0
        for i in range(dOrder):x0*=(mOrder-i)
        for i in self.series:
            x1,x2=0,(i[index]-mean[index])**dOrder
            for j in range(len(i)):x1+=w[j,0]*(i[j]-mean[j])
            sum+=x2*x1**(mOrder-dOrder)
        return x0*sum/self.obs
#-------------------------------------------    
    def get_SR(self,stats,n):
        # Set Z*
        meanSR=stats[0]/stats[1]
        sigmaSR=((1-meanSR*stats[2]+meanSR**2*(stats[3]-1)/4.)/(n-1))**.5
        return meanSR,sigmaSR
#-------------------------------------------
    def get_Stats(self,m):
        # Compute stats
        return [m[0],m[1]**.5,m[2]/m[1]**(3/2.),m[3]/m[1]**2]
#-------------------------------------------
    def get_Moments(self,series,order,mean=0):
        # Compute a moment
        sum=0
        for i in series:sum+=(i-mean)**order
        return sum/float(self.obs)
#-------------------------------------------
#-------------------------------------------
def main():
    #1) Inputs (path to csv file with returns series)
    path='data.csv'
    maxIter=1000 # Maximum number of iterations
    delta=.005 # Delta Z (attempted gain per interation)
    
    #2) Load data, set seed
    series=np.genfromtxt(path,delimiter=',') # load as numpy array
    seed=np.ones((series.shape[1],1)) # initialize seed
    bounds=[(0,1) for i in seed] # min and max boundary per weight
    
    #3) Create class and solve
    psrOpt=PSR_Opt(series,seed,delta,maxIter,bounds)
    start = time.time()
    psrOpt.optimize()
    end = time.time()
    print(end-start)
    
    #4) Optimize and report optimal portfolio
    print 'Maximized Z-value: '+str(psrOpt.z)
    print '# of iterations: '+str(psrOpt.iter)
    print 'PSR optimal portfolio:'
    print str(psrOpt.w)
#-------------------------------------------
# Boilerplate
if __name__=='__main__': main()
