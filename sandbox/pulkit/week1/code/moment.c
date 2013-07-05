#include<R.h>
#include<Rinternals.h>
#include<Rmath.h>

SEXP sums(SEXP mat,SEXP index,SEXP Rmean,SEXP dOrder,SEXP Rweights,SEXP mOrder,SEXP sum){
    double x1,x2,diff;
    int a,b,row,column;
    SEXP Rdim = getAttrib(mat,R_DimSymbol);
    row = INTEGER(Rdim)[0];
    column = INTEGER(Rdim)[1];
    mat = coerceVector(mat,REALSXP);
    dOrder = coerceVector(dOrder,INTSXP);
    mOrder = coerceVector(mOrder,INTSXP);
    index = coerceVector(index,INTSXP);
    int ind = INTEGER(index)[0];
    int d = INTEGER(dOrder)[0];
    int m = INTEGER(mOrder)[0];
    double *mean = REAL(Rmean);
    double *weights = REAL(Rweights);
    double s = 0;
    for(a = 0;a<row;a++){
        x1 = 0;
        diff = REAL(mat)[a+row*(ind-1)]-mean[ind-1];
        x2 = pow(diff,d);
        for(b = 0;b<column;b++){
            x1 = x1 + weights[b]*(REAL(mat)[a + row*b]-mean[b]);
        }
        s = s +x2*pow(x1,m-d);
    }
    REAL(sum)[0] = s;
    return sum;
}

        
