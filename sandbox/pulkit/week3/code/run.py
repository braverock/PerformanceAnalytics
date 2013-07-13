#!/usr/bin/env python
# On 20121230
# MaxDD, t* and MaxTuW for HFR series
# by MLdP <lopezdeprado@lbl.gov>
import DD2,DD3 # These are the two modules in Appendices 9 and 10
#---------------------------------------------------------------
def isNumber(input):
    # Determines whether input is a number
    try:
        float(input)
        return True
    except:
        return False
#---------------------------------------------------------------
def main():
    #1) Parameters
    path=''
    inFileName='data1.csv'
    outFileName='Results1.csv'
    fields=['Code','Mean','Phi','Sigma']
    confidence=.95
    dPi0=0
    #2) Read file
    inFile=open(path+inFileName,'r')
    outFile=open(path+outFileName,'w')
    headers=inFile.readline().split(',')
    indices=[headers.index(i) for i in fields]
    for line in inFile:
        #3) Get Input
        params={}
        line=line[:-1].split(',')
        for i in indices:
            if isNumber(line[i])==True:
                params[headers[i]]=float(line[i])
            else:
                params[headers[i]]=line[i]
        #4) Compute MaxDD,MaxTuW
        if params['Mean']>0 and params['Phi']>=0:
            t,minQ=DD2.getMinQ(params['Phi'],params['Mean'],params['Sigma'],dPi0,confidence)
            maxDD=max(0,-minQ)
            maxTuW=DD3.getTuW(params['Phi'],params['Mean'],params['Sigma'],dPi0,confidence)
        else:
            maxDD,t,maxTuW='--','--','--'
        #5) Store result
        msg=params['Code']+','+str(maxDD)+','+str(t)+','+str(maxTuW)
        outFile.writelines(msg+'\n')
        print msg
    return
#---------------------------------------------------------------
# Boilerplate
if __name__=='__main__':main()
