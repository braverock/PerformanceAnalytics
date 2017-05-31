
#include <R.h>
#include <Rinternals.h>

// // //
// // Sample estimators
// // //

SEXP  M3sample(SEXP XX, SEXP NN, SEXP PP, SEXP CC){
  /*
   arguments
   XX        : vector of length NN * PP with observations
   NN        : integer, number of observations
   PP        : integer, number of assets
   CC        : numeric, normalising constant

   Written by Dries Cornilly
   */

  // // declare pointers for the vector
  double *X;

  // use REAL() to access the C array inside the numeric vector passed in from R
  X = REAL(XX);
  int N = asInteger(NN);
  int P = asInteger(PP);
  double c = asReal(CC);

  // allocate and compute the coskewness matrix
  SEXP M3S = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) / 6));
  double *rM3S = REAL(M3S);

  int iter = 0;
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * N;
    for (int jj = ii; jj < P; jj++) {
      int jjN = jj * N;
      for (int kk = jj; kk < P; kk++) {
        int kkN = kk * N;
        // compute coskewness element
        double elem = 0.0;
        for (int tt = 0; tt < N; tt++) {
          elem += X[iiN + tt] * X[jjN + tt] * X[kkN + tt];
        }

        // fill in the element and update the iterator
        rM3S[iter] = c * elem;
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii

  UNPROTECT(1);
  return M3S;
}

SEXP  M4sample(SEXP XX, SEXP NN, SEXP PP){
  /*
   arguments
   XX        : vector of length NN * PP with observations
   NN        : integer, number of observations
   PP        : integer, number of assets

   Written by Dries Cornilly
   */

  // // declare pointers for the vectors
  double *X;

  // use REAL() to access the C array inside the numeric vector passed in from R
  X = REAL(XX);
  int N = asInteger(NN);
  int P = asInteger(PP);
  double n = asReal(NN);

  // allocate and compute the cokurtosis matrix
  SEXP M4S = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) * (P + 3) / 24));
  double *rM4S = REAL(M4S);

  int iter = 0;
  // compute the unique elements
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * N;
    for (int jj = ii; jj < P; jj++) {
      int jjN = jj * N;
      for (int kk = jj; kk < P; kk++) {
        int kkN = kk * N;
        for (int ll = kk; ll < P; ll++) {
          int llN = ll * N;
          // compute cokurtosis element
          double elem = 0.0;
          for (int tt = 0; tt < N; tt++) {
            elem += X[iiN + tt] * X[jjN + tt] * X[kkN + tt] * X[llN + tt];
          }

          // fill in the element and update the iterator
          rM4S[iter] = elem / n;
          iter++;
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii

  UNPROTECT(1);
  return M4S;
}
