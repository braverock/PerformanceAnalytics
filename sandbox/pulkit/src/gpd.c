#include<R.h>
#include<Rinternals.h>
#include<Rmath.h>


void gpdlik(double *data, int *n, double *loc, double *scale,
	    double *shape, double *dns)
{
  int i;
  double *dvec;
  
  dvec = (double *)R_alloc(*n, sizeof(double));

  if(*scale <= 0) {
     *dns = -1e6;
     return;
  }

  for(i=0;i<*n;i++)  {
    data[i] = (data[i] - loc[i]) / *scale;
    if (data[i] <= 0) {
      *dns = -1e6;
      return;
    }
    if(fabs(*shape) <= 1e-6){
      *shape = 0;
      dvec[i] = -log(*scale) - data[i];
    }
    else {
      data[i] = 1 + *shape * data[i];
      if(data[i] <= 0) {
	*dns = -1e6;
	return;
      }
      dvec[i] = -log(*scale) - (1 / *shape + 1) * log(data[i]);
    }
  }
  
  for(i=0;i<*n;i++) 
    *dns = *dns + dvec[i];
}
