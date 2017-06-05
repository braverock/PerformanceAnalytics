
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


// // //
// // Shrinkage and structured estimators for covariance
// // //

SEXP  VM2(SEXP mm11, SEXP mm22, SEXP NN, SEXP PP){
  /*
   arguments
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *m11, *m22;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  m11 = REAL(mm11);
  m22 = REAL(mm22);
  int P = asInteger(PP);
  double N = asReal(NN);
  
  // allocate and compute the sample variance and covariances
  SEXP VM2vec = PROTECT(allocVector(REALSXP, 3));
  double *rVM2vec = REAL(VM2vec);
  for (int ii = 0; ii < 3; ii++) {
    rVM2vec[ii] = 0.0;
  }
  
  // loop over the unique indices i <= j
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      if (ii == jj) {
        // element s_ii
        double temp = m22[iiP + ii] - m11[iiP + ii] * m11[iiP + ii];
        rVM2vec[0] += temp / N;
        rVM2vec[2] += temp / N;
      } else {
        // element s_ij
        rVM2vec[0] += 2.0 * (m22[jjP + ii] - m11[jjP + ii] * m11[jjP + ii]) / N;
      }
    } // loop jj
  } // loop ii
  
  // compute cov(T1, Sigma)
  rVM2vec[1] = rVM2vec[2];
  
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      rVM2vec[1] += 2.0 * (m22[jjP + ii] - m11[iiP + ii] * m11[jjP + jj]) / N;
    } // loop over jj
  } // loop over ii
  rVM2vec[1] /= P;
  
  UNPROTECT(1);
  return VM2vec;
}

SEXP  CM2_1F(SEXP XXc, SEXP ffcobs, SEXP ffvar, SEXP mm11, SEXP mm22, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   ffcobs    : numeric vector with centered factor observations
   ffvar     : double with factor variance
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *m11, *m22, *fcobs;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  m11 = REAL(mm11);
  m22 = REAL(mm22);
  fcobs = REAL(ffcobs);
  double fvar = asReal(ffvar);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // compute some helper variables for later on
  double fvar2 = fvar * fvar;
  double covXf[P];
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * n;
    covXf[ii] = 0.0;
    for (int tt = 0; tt < n; tt++) {
      covXf[ii] += Xc[iiN + tt] * fcobs[tt];
    }
    covXf[ii] /= N;
  }
  
  // allocate and compute the sample variance and covariances
  SEXP CM2vec = PROTECT(allocVector(REALSXP, 1));
  double *rCM2vec = REAL(CM2vec);
  rCM2vec[0] = 0.0;
  
  // loop over the unique indices i <= j
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      if (ii == jj) {
        // element s_ii
        rCM2vec[0] += (m22[iiP + ii] - m11[iiP + ii] * m11[iiP + ii]) / N;
      } else {
        // element s_ij
        double S211 = 0.0;
        double S121 = 0.0;
        double S112 = 0.0;
        for (int tt = 0; tt < n; tt++) {
          S211 += Xc[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
          S121 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[jjN + tt] * fcobs[tt];
          S112 += Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt] * fcobs[tt];
        }
        
        double temp_i0 = S211 / N - m11[jjP + ii] * covXf[ii];
        double temp_j0 = S121 / N - m11[jjP + ii] * covXf[jj];
        double temp_var = S112 / N - m11[jjP + ii] * fvar;
        
        rCM2vec[0] += 2.0 * (covXf[jj] * temp_i0 / fvar + covXf[ii] * temp_j0 / fvar -
          covXf[ii] * covXf[jj] * temp_var / fvar2) / N;
      }
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return CM2vec;
}

SEXP  CM2_CC(SEXP XXc, SEXP rrcoef, SEXP mm11, SEXP mm22, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   rrcoef     : double with factor variance
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *m11, *m22;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  m11 = REAL(mm11);
  m22 = REAL(mm22);
  double rcoef = asReal(rrcoef);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // allocate and compute the sample variance and covariances
  SEXP CM2vec = PROTECT(allocVector(REALSXP, 1));
  double *rCM2vec = REAL(CM2vec);
  rCM2vec[0] = 0.0;
  
  // loop over the unique indices i <= j
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      if (ii == jj) {
        // element s_ii
        rCM2vec[0] += (m22[iiP + ii] - m11[iiP + ii] * m11[iiP + ii]) / N;
      } else {
        // element s_ij
        double S31 = 0.0;
        double S13 = 0.0;
        for (int tt = 0; tt < n; tt++) {
          S31 += Xc[iiN + tt] * Xc[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
          S13 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[jjN + tt] * Xc[jjN + tt];
        }
        
        double temp_ii = S31 / N - m11[jjP + ii] * m11[iiP + ii];
        double temp_jj = S13 / N - m11[jjP + ii] * m11[jjP + jj];
        
        rCM2vec[0] += rcoef * (sqrt(m11[jjP + jj] / m11[iiP + ii]) * temp_ii + 
          sqrt(m11[iiP + ii] / m11[jjP + jj]) * temp_jj) / N;
      }
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return CM2vec;
}


// // //
// // Shrinkage and structured estimators for coskewness
// // //

SEXP  M3_T23(SEXP mmargskews, SEXP PP){
  /*
   arguments
   mmargskews : vector of length PP with marginal skewness values
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margskews;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margskews = REAL(mmargskews);
  int P = asInteger(PP);
  
  // allocate and compute the coskewness matrix
  SEXP M3 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) / 6));
  double *rM3 = REAL(M3);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        // compute coskewness element
        double elem = 0.0;
        if ((ii == jj) & (jj == kk)) elem = margskews[ii];
        
        // fill in the element and update the iterator
        rM3[iter] = elem;
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M3;
}

SEXP  M3_Simaan(SEXP mmargskewsroot, SEXP PP){
  /*
   arguments
   mmargskewsroot : vector of length PP with marginal skewness values (phi^(1 / 3))
   PP        : integer, number of assets
   
   Written by Dries Cornilly, coskewness matrix based on Simaan (1993)
   */
  
  // // declare pointers for the vector
  double *margskewsroot;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margskewsroot = REAL(mmargskewsroot);
  int P = asInteger(PP);
  
  // allocate and compute the coskewness matrix
  SEXP M3 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) / 6));
  double *rM3 = REAL(M3);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        // compute and fill coskewness element
        rM3[iter] = margskewsroot[ii] * margskewsroot[jj] * margskewsroot[kk];
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M3;
}

SEXP  M3_1F(SEXP mmargskews, SEXP bbeta, SEXP ffskew, SEXP PP){
  /*
   arguments
   mmargskews : vector of length PP with marginal skewness values
   bbeta     : vector of length PP with factor loadings
   ffskew    : factor skewness
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margskews, *beta;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margskews = REAL(mmargskews);
  beta = REAL(bbeta);
  double fskew = asReal(ffskew);
  int P = asInteger(PP);
  
  // allocate and compute the coskewness matrix
  SEXP M3 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) / 6));
  double *rM3 = REAL(M3);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        // compute and fill coskewness element
        if ((ii == jj) & (jj == kk)) {
          rM3[iter] = margskews[ii];
        } else {
          rM3[iter] = beta[ii] * beta[jj] * beta[kk] * fskew;
        }
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M3;
}

SEXP  M3_CCoefficients(SEXP mmargvars, SEXP mmargkurts,
                       SEXP mm21, SEXP mm22, SEXP XXc, SEXP NN, SEXP PP){
  /*
   arguments
   mmargvars : vector of length PP with marginal variances
   mmargkurts : vector of length PP with marginal kurtosis values
   mm21      : t(Xc^2) %*% Xc
   mm22      : t(Xc^2) %*% Xc^2
   XXc       : numeric vector with centered observations
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margvars, *margkurts, *m21, *m22, *Xc;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margvars = REAL(mmargvars);
  margkurts = REAL(mmargkurts);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  Xc = REAL(XXc);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  double p = asReal(PP);
  
  // allocate and compute the extended correlation coefficients
  SEXP CCoef = PROTECT(allocVector(REALSXP, 3));
  double *rCCoef = REAL(CCoef);
  
  // compute the generalized correlation coefficients
  rCCoef[0] = 0.0;
  rCCoef[2] = 0.0;
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      rCCoef[0] += m21[jjP + ii] / sqrt(margkurts[ii] * margvars[jj]);
      rCCoef[2] += m22[jjP + ii] / sqrt(margkurts[ii] * margkurts[jj]);
    }
  }
  rCCoef[0] *= 2.0 / (p * (p - 1.0));
  rCCoef[2] *= 2.0 / (p * (p - 1.0));
  rCCoef[1] = 0.0;
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * n;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjN = jj * n;
      for (int kk = jj + 1; kk < P; kk++) {
        int kkN = kk * n;
        double m111 = 0.0;
        for (int tt = 0; tt < n; tt++) {
          m111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
        }
        m111 /= N;
        double nc = sqrt(margvars[ii] * rCCoef[2] * sqrt(margkurts[jj] * margkurts[kk])) +
          sqrt(margvars[jj] * rCCoef[2] * sqrt(margkurts[ii] * margkurts[kk])) +
          sqrt(margvars[kk] * rCCoef[2] * sqrt(margkurts[ii] * margkurts[jj]));
        rCCoef[1] += m111 / nc;
      }
    }
  }
  rCCoef[1] *= 6.0 / (p * (p - 1.0) * (p - 2.0));
  
  UNPROTECT(1);
  return CCoef;
}

SEXP  M3_CC(SEXP mmargvars, SEXP mmargskews, SEXP mmargkurts,
            SEXP rr2, SEXP rr4, SEXP rr5, SEXP PP){
  /*
   arguments
   mmargvars : vector of length PP with marginal variances
   mmargskews : vector of length PP with marginal skewness values
   mmargkurts : vector of length PP with marginal kurtosis values
   rr2       : generalized correlation of order 2
   rr4       : generalized correlation of order 4
   rr5       : generalized correlation of order 5
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margvars, *margskews, *margkurts;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margvars = REAL(mmargvars);
  margskews = REAL(mmargskews);
  margkurts = REAL(mmargkurts);
  double r2 = asReal(rr2);
  double r4 = asReal(rr4);
  double r5 = asReal(rr5);
  int P = asInteger(PP);
  
  // allocate and compute the coskewness matrix
  SEXP M3 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) / 6));
  double *rM3 = REAL(M3);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        // compute and fill coskewness element
        if (ii == jj) {
          if (jj == kk) {
            // phi_iii
            rM3[iter] = margskews[ii];
          } else {
            // phi_iik
            rM3[iter] = r2 * sqrt(margkurts[kk] * margvars[ii]);
          }
        } else {
          if (jj == kk) {
            // phi_ijj
            rM3[iter] = r2 * sqrt(margvars[ii] * margkurts[jj]);
          } else {
            // phi_ijk
            rM3[iter] = r4 * sqrt(r5) * (sqrt(margvars[kk] * sqrt(margkurts[ii] * margkurts[jj])) +
              sqrt(margvars[jj] * sqrt(margkurts[ii] * margkurts[kk])) +
              sqrt(margvars[ii] * sqrt(margkurts[jj] * margkurts[kk]))) / 3.0;
          }
        }
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M3;
}

SEXP  VM3kstat(SEXP XXc, SEXP XXc2,
               SEXP SS11, SEXP SS21, SEXP SS22, SEXP SS31,
               SEXP SS42, SEXP SS33, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   SS11      : numeric vector of t(Xc) %*% Xc
   SS21      : numeric vector of t(Xc^2) %*% Xc
   SS22      : numeric vector of t(Xc^2) %*% Xc^2
   SS31      : numeric vector of t(Xc^3) %*% Xc
   SS42      : numeric vector of t(Xc^4) %*% Xc^2
   SS33      : numeric vector of t(Xc^3) %*% Xc^3
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *S11, *S21, *S22, *S31, *S42, *S33;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  S11 = REAL(SS11);
  S21 = REAL(SS21);
  S22 = REAL(SS22);
  S31 = REAL(SS31);
  S42 = REAL(SS42);
  S33 = REAL(SS33);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // initialize useful constants
  double N2 = N * N;
  double N3 = N2 * N;
  double N4 = N3 * N;
  double N5 = N4 * N;
  double N122 = (N - 1.0) * (N - 1.0) * (N - 2.0) * (N - 2.0);
  
  double alpha = N * N122 * (N - 3.0) * (N - 4.0) * (N - 5.0);
  double ciii_S2_3 = (9.0 * N4 - 72.0 * N3 + 213.0 * N2 - 270.0 * N + 120.0) / alpha;
  double ciii_S4S2 = (-6.0 * N5 + 33.0 * N4 - 42.0 * N3 - 75.0 * N2 + 210.0 * N - 120.0) / alpha;
  double ciii_S3_2 = (-N5 - 4.0 * N4 + 41.0 * N3 - 40.0 * N2 - 100.0 * N + 80.0) / alpha;
  double ciii_S6 = (N5 - 5.0 * N4 + 13.0 * N3 - 23.0 * N2 + 22.0 * N - 8.0) /
    (N122 * (N - 3.0) * (N - 4.0) * (N - 5.0));
  
  double ciij_S02S20_2 = (N4 - 8.0 * N3 + 29.0 * N2 - 46.0 * N + 24.0) / alpha;
  double ciij_S20S11_2 = (8.0 * N4 - 64.0 * N3 + 184.0 * N2 - 224.0 * N + 96.0) / alpha;
  double ciij_S20S22 = (-2.0 * N5 + 10.0 * N4 - 10.0 * N3 - 34.0 * N2 + 84.0 * N - 48.0) / alpha;
  double ciij_S31S11 = (-4.0 * N5 + 24.0 * N4 - 36.0 * N3 - 32.0 * N2 + 112.0 * N - 64.0) / alpha;
  double ciij_S40S02 = (-N4 + 4.0 * N3 - 9.0 * N2 + 14.0 * N - 8.0) / alpha;
  double ciij_S21_2 = (-N5 + 25.0 * N3 - 36.0 * N2 - 60.0 * N + 48.0) / alpha;
  double ciij_S12S30 = (-4.0 * N4 + 16.0 * N3 - 4.0 * N2 - 40.0 * N + 32.0) / alpha;
  
  double cijk_S002S110_2 = (N4 - 8.0 * N3 + 25.0 * N2 - 34.0 * N + 16.0) / alpha;
  double cijk_S011S101S110 = (6.0 * N4 - 48.0 * N3 + 134.0 * N2 - 156.0 * N + 64.0) / alpha;
  double cijk_S112S110 = (-2.0 * N5 + 12.0 * N4 - 18.0 * N3 - 16.0 * N2 + 56.0 * N - 32.0) / alpha;
  double cijk_S002S020S200 = (4.0 * N2 - 12.0 * N + 8.0) / alpha;
  double cijk_S022S200 = (-N4 + 4.0 * N3 - 9.0 * N2 + 14.0 * N - 8.0) / alpha;
  double cijk_S111_2 = (-N5 + 2.0 * N4 + 17.0 * N3 - 34.0 * N2 - 40.0 * N + 32.0) / alpha;
  double cijk_S102S120 = (-2.0 * N4 + 8.0 * N3 - 2.0 * N2 - 20.0 * N + 16.0) / alpha;
  
  // allocate and compute the sample variance and covariances
  SEXP VM3vec = PROTECT(allocVector(REALSXP, 3));
  double *rVM3vec = REAL(VM3vec);
  for (int ii = 0; ii < 3; ii++) {
    rVM3vec[ii] = 0.0;
  }
  
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        if (ii == jj) {
          if (jj == kk) {
            // element phi_iii
            double temp = ciii_S2_3 * S11[iiP + ii] * S11[iiP + ii] * S11[iiP + ii] +
              ciii_S4S2 * S22[iiP + ii]* S11[iiP + ii] +
              ciii_S3_2 * S21[iiP + ii] * S21[iiP + ii] + ciii_S6 * S42[iiP + ii];
            rVM3vec[0] += temp;
            rVM3vec[2] += temp;
          } else {
            // element phi_iik
            rVM3vec[0] += 3.0 * (ciij_S02S20_2 * S11[kkP + kk] * S11[iiP + ii] * S11[iiP + ii] +
              ciij_S20S11_2 * S11[iiP + ii] * S11[kkP + ii] * S11[kkP + ii] +
              ciij_S20S22 * S11[iiP + ii] * S22[kkP + ii] + ciij_S31S11 * S31[kkP + ii] * S11[kkP + ii] +
              ciij_S40S02 * S22[iiP + ii] * S11[kkP + kk] + ciij_S21_2 * S21[kkP + ii] * S21[kkP + ii] +
              ciij_S12S30 * S21[iiP + kk] * S21[iiP + ii] + ciii_S6 * S42[kkP + ii]);
          }
        } else {
          if (jj == kk) {
            // element phi_ijj
            rVM3vec[0] += 3.0 * (ciij_S02S20_2 * S11[iiP + ii] * S11[jjP + jj] * S11[jjP + jj] +
              ciij_S20S11_2 * S11[jjP + jj] * S11[iiP + jj] * S11[iiP + jj] +
              ciij_S20S22 * S11[jjP + jj] * S22[iiP + jj] + ciij_S31S11 * S31[iiP + jj] * S11[iiP + jj] +
              ciij_S40S02 * S22[jjP + jj] * S11[iiP + ii] + ciij_S21_2 * S21[iiP + jj] * S21[iiP + jj] +
              ciij_S12S30 * S21[jjP + ii] * S21[jjP + jj] + ciii_S6 * S42[iiP + jj]);
          } else {
            // element phi_ijk
            double S211i = 0.0;
            double S211j = 0.0;
            double S211k = 0.0;
            double S111 = 0.0;
            double S222 = 0.0;
            
            for (int tt = 0; tt < n; tt++) {
              S211i += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S211j += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt];
              S211k += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
              S111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S222 += Xc2[iiN + tt] * Xc2[jjN + tt] * Xc2[kkN + tt];
            }
            
            rVM3vec[0] += 6.0 * (cijk_S002S110_2 * S11[kkP + kk] * S11[jjP + ii] * S11[jjP + ii] +
              (cijk_S011S101S110 * S11[kkP + jj] * S11[kkP + ii] + cijk_S112S110 * S211k) * S11[jjP + ii] +
              cijk_S002S110_2 * S11[jjP + jj] * S11[kkP + ii] * S11[kkP + ii] +
              cijk_S112S110 * S211j * S11[kkP + ii] +
              cijk_S002S110_2 * S11[iiP + ii] * S11[kkP + jj] * S11[kkP + jj] +
              cijk_S112S110 * S211i * S11[kkP + jj] +
              (cijk_S002S020S200 * S11[jjP + jj] * S11[kkP + kk] + cijk_S022S200 * S22[kkP + jj]) * S11[iiP + ii] +
              cijk_S022S200 * S22[kkP + ii] * S11[jjP + jj] + cijk_S022S200 * S22[jjP + ii] * S11[kkP + kk] +
              cijk_S111_2 * S111 * S111 +
              cijk_S102S120 * S21[iiP + kk] * S21[iiP + jj] + cijk_S102S120 * S21[jjP + kk] * S21[jjP + ii] +
              cijk_S102S120 * S21[kkP + jj] * S21[kkP + ii] + ciii_S6 * S222);
          }
        }
      } // loop kk
    } // loop jj
  } // loop ii
  
  // compute cov(T1, Phi)
  rVM3vec[1] = rVM3vec[2];
  
  double c_S11_3 = (24.0 * N2 - 72.0 * N + 48.0) / alpha;
  double c_S02S20S11 = (9.0 * N4 - 72.0 * N3 + 189.0 * N2 - 198.0 * N + 72.0) / alpha;
  double c_S22S11 = (-9.0 * N4 + 36.0 * N3 - 81.0 * N2 + 126.0 * N - 72.0) / alpha;
  double c_S13S20 = (-3.0 * N5 + 21.0 * N4 - 39.0 * N3 + 3.0 * N2 + 42.0 * N - 24.0) / alpha;
  double c_S21S12 = (-9.0 * N4 + 36.0 * N3 - 9.0 * N2 - 90.0 * N + 72.0) / alpha;
  double c_S03S30 = (-N5 + 5.0 * N4 + 5.0 * N3 - 31.0 * N2 - 10.0 * N + 8.0) / alpha;
  
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      rVM3vec[1] += 2 * (c_S11_3 * S11[jjP + ii] * S11[jjP + ii] * S11[jjP + ii] +
        (c_S02S20S11 * S11[iiP + ii] * S11[jjP + jj] + c_S22S11 * S22[jjP + ii]) * S11[jjP + ii] +
        c_S13S20 * S31[iiP + jj] * S11[iiP + ii] + c_S13S20 * S31[jjP + ii] * S11[jjP + jj] +
        c_S21S12 * S21[jjP + ii] * S21[iiP + jj] + c_S03S30 * S21[iiP + ii] * S21[jjP + jj] +
        ciii_S6 * S33[jjP + ii]);
    } // loop over jj
  } // loop over ii
  rVM3vec[1] /= P;
  
  UNPROTECT(1);
  return VM3vec;
}

SEXP  VM3(SEXP XXc, SEXP XXc2, SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm31,
          SEXP mm42, SEXP mm33, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   mm42      : numeric vector of t(Xc^4) %*% Xc^2 / NN
   mm33      : numeric vector of t(Xc^3) %*% Xc^3 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m22, *m31, *m42, *m33;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  m42 = REAL(mm42);
  m33 = REAL(mm33);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // allocate and compute the sample variance and covariances
  SEXP VM3vec = PROTECT(allocVector(REALSXP, 3));
  double *rVM3vec = REAL(VM3vec);
  for (int ii = 0; ii < 3; ii++) {
    rVM3vec[ii] = 0.0;
  }
  
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        if (ii == jj) {
          if (jj == kk) {
            // element phi_iii
            double temp = (m42[iiP + ii] - m21[iiP + ii] * m21[iiP + ii] - 6.0 * m22[iiP + ii] * m11[iiP + ii] +
                           9.0 * m11[iiP + ii] * m11[iiP + ii] * m11[iiP + ii]) / N;
            rVM3vec[0] += temp;
            rVM3vec[2] += temp;
          } else {
            // element phi_iik
            rVM3vec[0] += 3.0 * (m42[kkP + ii] - m21[kkP + ii] * m21[kkP + ii] - 4.0 * m31[kkP + ii] * m11[kkP + ii] -
              2.0 * m22[kkP + ii] * m11[iiP + ii] + 8.0 * m11[iiP + ii] * m11[kkP + ii] * m11[kkP + ii] +
              m11[kkP + kk] * m11[iiP + ii] * m11[iiP + ii]) / N;
          }
        } else {
          if (jj == kk) {
            // element phi_ijj
            rVM3vec[0] += 3.0 * (m42[iiP + jj] - m21[iiP + jj] * m21[iiP + jj] - 4.0 * m31[iiP + jj] * m11[iiP + jj] -
              2.0 * m22[iiP + jj] * m11[jjP + jj] + 8.0 * m11[jjP + jj] * m11[iiP + jj] * m11[iiP + jj] +
              m11[iiP + ii] * m11[jjP + jj] * m11[jjP + jj]) / N;
          } else {
            // element phi_ijk
            double S211 = 0.0;
            double S121 = 0.0;
            double S112 = 0.0;
            double S111 = 0.0;
            double S222 = 0.0;
            
            for (int tt = 0; tt < n; tt++) {
              S211 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S211 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt];
              S211 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
              S111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S222 += Xc2[iiN + tt] * Xc2[jjN + tt] * Xc2[kkN + tt];
            }
            
            rVM3vec[0] += 6.0 * (S222 / N - S111 * S111 / (N * N) - 2.0 * S211 / N * m11[kkP + jj] -
              2.0 * S121 / N * m11[kkP + ii] - 2.0 * S112 / N * m11[jjP + ii] +
              6.0 * m11[kkP + ii] * m11[kkP + jj] * m11[jjP + ii] +
              m11[iiP + ii] * m11[kkP + jj] * m11[kkP + jj] + m11[jjP + jj] * m11[kkP + ii] * m11[kkP + ii] +
              m11[kkP + kk] * m11[jjP + ii] * m11[jjP + ii]) / N;
          }
        }
      } // loop kk
    } // loop jj
  } // loop ii
  
  // compute cov(T1, Phi)
  rVM3vec[1] = rVM3vec[2];
  
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      rVM3vec[1] += 2 * (m33[jjP + ii] - m21[iiP + ii] * m21[jjP + jj] - 3.0 * m31[jjP + ii] * m11[jjP + jj] -
        3.0 * m31[iiP + jj] * m11[iiP + ii] + 9.0 * m11[iiP + ii] * m11[jjP + jj] * m11[jjP + ii]) / N;
    } // loop over jj
  } // loop over ii
  rVM3vec[1] /= P;
  
  UNPROTECT(1);
  return VM3vec;
}

SEXP  CM3_Simaan(SEXP XXc, SEXP XXc2, SEXP mmargskewsroot,
                 SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm31,
                 SEXP mm42, SEXP mm51, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   mmargskewsroot : phi^(-2 / 3) with phi the marginal skewness values
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   mm42      : numeric vector of t(Xc^4) %*% Xc^2 / NN
   mm51      : numeric vector of t(Xc^5) %*% Xc / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m22, *m31, *m42, *m51, *margskewsroot;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  m42 = REAL(mm42);
  m51 = REAL(mm51);
  margskewsroot = REAL(mmargskewsroot);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // allocate and compute the sample variance and covariances
  SEXP CM3vec = PROTECT(allocVector(REALSXP, 1));
  double *rCM3vec = REAL(CM3vec);
  rCM3vec[0] = 0.0;
  
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        if (ii == jj) {
          if (jj == kk) {
            // element phi_iii
            rCM3vec[0] += (m42[iiP + ii] - m21[iiP + ii] * m21[iiP + ii] - 6.0 * m22[iiP + ii] * m11[iiP + ii] +
              9.0 * m11[iiP + ii] * m11[iiP + ii] * m11[iiP + ii]);
          } else {
            // element phi_iik
            double temp_ii = m51[kkP + ii] - m21[kkP + ii] * m21[iiP + ii] - 4.0 * m31[kkP + ii] * m11[iiP + ii] -
              2.0 * m22[iiP + ii] * m11[kkP + ii] + 9.0 * m11[iiP + ii] * m11[iiP + ii] * m11[kkP + ii];
            double temp_kk = m42[iiP + kk] - m21[kkP + ii] * m21[kkP + kk] - 3.0 * m22[kkP + ii] * m11[kkP + kk] -
              m22[kkP + kk] * m11[iiP + ii] - 2.0 * m31[iiP + kk] * m11[kkP + ii] +
              6.0 * m11[kkP + kk] * m11[kkP + ii] * m11[kkP + ii] + 3.0 * m11[kkP + kk] * m11[kkP + kk] * m11[iiP + ii];
            
            rCM3vec[0] += margskewsroot[ii] * margskewsroot[ii] * margskewsroot[kk] *
              (2.0 * m21[jjP + jj] * m21[kkP + kk] * temp_ii + m21[iiP + ii] * m21[iiP + ii] * temp_kk);
          }
        } else {
          if (jj == kk) {
            // element phi_ijj
            double temp_ii = m42[jjP + ii] - m21[iiP + jj] * m21[iiP + ii] - 3.0 * m22[iiP + jj] * m11[iiP + ii] -
              m22[iiP + ii] * m11[jjP + jj] - 2.0 * m31[jjP + ii] * m11[iiP + jj] +
              6.0 * m11[iiP + ii] * m11[iiP + jj] * m11[iiP + jj] + 3.0 * m11[iiP + ii] * m11[iiP + ii] * m11[jjP + jj];
            double temp_jj = m51[iiP + jj] - m21[iiP + jj] * m21[jjP + jj] - 4.0 * m31[iiP + jj] * m11[jjP + jj] -
              2.0 * m22[jjP + jj] * m11[iiP + jj] + 9.0 * m11[jjP + jj] * m11[jjP + jj] * m11[iiP + jj];
            
            rCM3vec[0] += margskewsroot[ii] * margskewsroot[jj] * margskewsroot[jj] *
              (m21[jjP + jj] * m21[jjP + jj] * temp_ii + 2.0 * m21[iiP + ii] * m21[kkP + kk] * temp_jj);
          } else {
            // element phi_ijk
            double S411 = 0.0;
            double S141 = 0.0;
            double S114 = 0.0;
            double S111 = 0.0;
            double S211 = 0.0;
            double S121 = 0.0;
            double S112 = 0.0;
            for (int tt = 0; tt < n; tt++) {
              S411 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S141 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[kkN + tt];
              S114 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt];
              S111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S211 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S121 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt];
              S112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
            }
            
            double temp_ii = S411 / N - S111 / N * m21[iiP + ii] - 3.0 * S211 / N * m11[iiP + ii] -
              m22[iiP + ii] * m11[kkP + jj] - m31[jjP + ii] * m11[kkP + ii] - m31[kkP + ii] * m11[jjP + ii] +
              6.0 * m11[iiP + ii] * m11[jjP + ii] * m11[kkP + ii] + 3.0 * m11[iiP + ii] * m11[iiP + ii] * m11[kkP + jj];
            double temp_jj = S141 / N - S111 / N * m21[jjP + jj] - 3.0 * S121 / N * m11[jjP + jj] -
              m22[jjP + jj] * m11[kkP + ii] - m31[iiP + jj] * m11[kkP + jj] - m31[kkP + jj] * m11[iiP + jj] +
              6.0 * m11[jjP + jj] * m11[iiP + jj] * m11[kkP + jj] + 3.0 * m11[jjP + jj] * m11[jjP + jj] * m11[kkP + ii];
            double temp_kk = S114 / N - S111 / N * m21[kkP + kk] - 3.0 * S112 / N * m11[kkP + kk] -
              m22[kkP + kk] * m11[jjP + ii] - m31[jjP + kk] * m11[iiP + kk] - m31[iiP + kk] * m11[jjP + kk] +
              6.0 * m11[kkP + kk] * m11[jjP + kk] * m11[iiP + kk] + 3.0 * m11[kkP + kk] * m11[kkP + kk] * m11[jjP + ii];
            
            rCM3vec[0] += 2.0 * margskewsroot[ii] * margskewsroot[jj] * margskewsroot[kk] *
              (m21[jjP + jj] * m21[kkP + kk] * temp_ii + m21[iiP + ii] * m21[kkP + kk] * temp_jj +
              m21[iiP + ii] * m21[jjP + jj] * temp_kk);
          }
        }
      } // loop kk
    } // loop jj
  } // loop ii
  
  rCM3vec[0] /= N;
  
  UNPROTECT(1);
  return CM3vec;
}

SEXP  CM3_1F(SEXP XXc, SEXP XXc2, SEXP ffcobs, SEXP ffvar, SEXP ffskew,
             SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm42, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   ffcobs    : numeric vector with centered factor observations
   ffvar     : double with the factor variance
   ffskew    : double with the factor skewness
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm42      : numeric vector of t(Xc^4) %*% Xc^2 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m22, *m42, *fcobs;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m42 = REAL(mm42);
  fcobs = REAL(ffcobs);
  double fvar = asReal(ffvar);
  double fskew = asReal(ffskew);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // compute some helper variables for later on
  double fvar3 = fvar * fvar * fvar;
  double covXf[P];
  double X1f2[P];
  double X1f3[P];
  double X11f1[P * P];
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * n;
    covXf[ii] = 0.0;
    X1f2[ii] = 0.0;
    X1f3[ii] = 0.0;
    for (int tt = 0; tt < n; tt++) {
      covXf[ii] += Xc[iiN + tt] * fcobs[tt];
      X1f2[ii] += Xc[iiN + tt] * fcobs[tt] * fcobs[tt];
      X1f3[ii] += Xc[iiN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
    }
    covXf[ii] /= N;
    X1f2[ii] /= N;
    X1f3[ii] /= N;
    for (int jj = ii; jj < P; jj++) {
      int jjN = jj * n;
      double elem = 0.0;
      for (int tt = 0; tt < n; tt++) {
        elem += Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
      }
      elem /= N;
      X11f1[ii * P + jj] = elem;
      X11f1[jj * P + ii] = elem;
    }
  }
  
  // allocate and compute the sample variance and covariances
  SEXP CM3vec = PROTECT(allocVector(REALSXP, 1));
  double *rCM3vec = REAL(CM3vec);
  rCM3vec[0] = 0.0;
  
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        if (ii == jj) {
          if (jj == kk) {
            // element phi_iii
            rCM3vec[0] += m42[iiP + ii] - m21[iiP + ii] * m21[iiP + ii] - 6.0 * m22[iiP + ii] * m11[iiP + ii] +
              9.0 * m11[iiP + ii] * m11[iiP + ii] * m11[iiP + ii];
          } else {
            // element phi_iik
            double S311 = 0.0;
            double S221 = 0.0;
            double S213 = 0.0;
            double S211 = 0.0;
            double S212 = 0.0;
            for (int tt = 0; tt < n; tt++) {
              S311 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[kkN + tt] * fcobs[tt];
              S221 += Xc2[iiN + tt] * Xc2[kkN + tt] * fcobs[tt];
              S213 += Xc2[iiN + tt] * Xc[kkN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
              S211 += Xc2[iiN + tt] * Xc[kkN + tt] * fcobs[tt];
              S212 += Xc2[iiN + tt] * Xc[kkN + tt] * fcobs[tt] * fcobs[tt];
            }
            
            double temp_ii = S311 / N - m21[kkP + ii] * covXf[ii] - 2.0 * m11[kkP + ii] * X11f1[iiP + ii] -
              m11[iiP + ii] * X11f1[kkP + ii];
            double temp_kk = S221 / N - m21[kkP + ii] * covXf[kk] - m11[iiP + ii] * X11f1[kkP + kk] -
              2.0 * m11[kkP + ii] * X11f1[kkP + ii];
            double temp_fskew = S213 / N - m21[kkP + ii] * fskew - 3.0 * S211 / N * fvar -
              2.0 * X1f3[ii] * m11[kkP + ii] - X1f3[kk] * m11[iiP + ii] +
              3.0 * m11[iiP + ii] * covXf[kk] * fvar + 6.0 * m11[kkP + ii] * covXf[ii] * fvar;
            double temp_fvar = S212 / N - m21[kkP + ii] * fvar - 2.0 * X1f2[ii] * m11[kkP + ii] -
              X1f2[kk] * m11[iiP + ii];
            
            rCM3vec[0] += 3.0 * ((2.0 * covXf[ii] * covXf[kk] * temp_ii +
              covXf[ii] * covXf[ii] * temp_kk) * fskew + covXf[ii] * covXf[ii] * covXf[kk] * temp_fskew -
              3.0 * covXf[ii] * covXf[ii] * covXf[kk] * fskew * temp_fvar / fvar) / fvar3;
          }
        } else {
          if (jj == kk) {
            // element phi_ijj
            double S221 = 0.0;
            double S131 = 0.0;
            double S123 = 0.0;
            double S121 = 0.0;
            double S122 = 0.0;
            for (int tt = 0; tt < n; tt++) {
              S221 += Xc2[iiN + tt] * Xc2[jjN + tt] * fcobs[tt];
              S131 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * fcobs[tt];
              S123 += Xc[iiN + tt] * Xc2[jjN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
              S121 += Xc[iiN + tt] * Xc2[jjN + tt] * fcobs[tt];
              S122 += Xc[iiN + tt] * Xc2[jjN + tt] * fcobs[tt] * fcobs[tt];
            }
            double temp_ii = S221 / N - m21[iiP + jj] * covXf[ii] - m11[jjP + jj] * X11f1[iiP + ii] -
              2.0 * m11[jjP + ii] * X11f1[jjP + ii];
            double temp_jj = S131 / N - m21[iiP + jj] * covXf[jj] - 2.0 * m11[jjP + ii] * X11f1[jjP + jj] -
              m11[jjP + jj] * X11f1[jjP + ii];
            double temp_fskew = S123 / N - m21[iiP + jj] * fskew - 3.0 * S121 / N * fvar -
              X1f3[ii] * m11[jjP + jj] - 2.0 * X1f3[jj] * m11[jjP + ii] +
              6.0 * m11[jjP + ii] * covXf[jj] * fvar + 3.0 * m11[jjP + jj] * covXf[ii] * fvar;
            double temp_fvar = S122 / N - m21[iiP + jj] * fvar - X1f2[ii] * m11[jjP + jj] -
              2.0 * X1f2[jj] * m11[jjP + ii];
            
            rCM3vec[0] += 3.0 * ((covXf[jj] * covXf[jj] * temp_ii + 2.0 * covXf[ii] * covXf[jj] * temp_jj) * fskew +
              covXf[ii] * covXf[jj] * covXf[jj] * temp_fskew -
              3.0 * covXf[ii] * covXf[jj] * covXf[jj] * fskew * temp_fvar / fvar) / fvar3;
          } else {
            // element phi_ijk
            double S2111 = 0.0;
            double S1211 = 0.0;
            double S1121 = 0.0;
            double S1112 = 0.0;
            double S1111 = 0.0;
            double S1113 = 0.0;
            double S1110 = 0.0;
            for (int tt = 0; tt < n; tt++) {
              S2111 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * fcobs[tt];
              S1211 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt] * fcobs[tt];
              S1121 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * fcobs[tt];
              S1112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * fcobs[tt] * fcobs[tt];
              S1113 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
              S1111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * fcobs[tt];
              S1110 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
            }
            
            double temp_ii = S2111 / N - S1110 / N * covXf[ii] - m11[kkP + jj] * X11f1[iiP + ii] -
              m11[kkP + ii] * X11f1[jjP + ii] - m11[jjP + ii] * X11f1[kkP + ii];
            double temp_jj = S1211 / N - S1110 / N * covXf[jj] - m11[kkP + ii] * X11f1[jjP + jj] -
              m11[kkP + jj] * X11f1[iiP + jj] - m11[iiP + jj] * X11f1[kkP + jj];
            double temp_kk = S1121 / N - S1110 / N * covXf[kk] - m11[jjP + ii] * X11f1[kkP + kk] -
              m11[jjP + kk] * X11f1[iiP + kk] - m11[iiP + kk] * X11f1[jjP + kk];
            double temp_fskew = S1113 / N - S1110 / N * fskew - 3.0 * S1111 / N * fvar -
              X1f3[ii] * m11[kkP + jj] - X1f3[jj] * m11[kkP + ii] - X1f3[kk] * m11[jjP + ii] +
              3.0 * m11[jjP + ii] * covXf[kk] * fvar + 3.0 * m11[kkP + ii] * covXf[jj] * fvar +
              3.0 * m11[kkP + jj] * covXf[ii] * fvar;
            double temp_fvar = S1112 / N - S1110 / N * fvar - X1f2[ii] * m11[kkP + jj] -
              X1f2[jj] * m11[kkP + ii] - X1f2[kk] * m11[jjP + ii];
            
            rCM3vec[0] += 6.0 * ((covXf[jj] * covXf[kk] * temp_ii + covXf[ii] * covXf[kk] * temp_jj +
              covXf[ii] * covXf[jj] * temp_kk) * fskew + covXf[ii] * covXf[jj] * covXf[kk] * temp_fskew -
              3.0 * covXf[ii] * covXf[jj] * covXf[kk] * fskew * temp_fvar / fvar) / fvar3;
          }
        }
      } // loop kk
    } // loop jj
  } // loop ii
  
  rCM3vec[0] /= N;
  
  UNPROTECT(1);
  return CM3vec;
}

SEXP  CM3_CC(SEXP XXc, SEXP XXc2, SEXP mmargvars, SEXP mmargskews,
             SEXP mmargkurts, SEXP mmarg5s, SEXP mmarg6s,
             SEXP mm11, SEXP mm21, SEXP mm31, SEXP mm32,
             SEXP mm41, SEXP mm61, SEXP rr2, SEXP rr4, SEXP rr5,
             SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   mmargvars : numeric vector with marginal variances
   mmargskews : numeric vector with marginal skewness values
   mmargkurts : numeric vector with marginal kurtosis values
   mmarg5s   : numeric vector with marginal 5th order moments
   mmarg6s   : numeric vector with marginal 6th order moments
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   mm32      : numeric vector of t(Xc^3) %*% Xc^2 / NN
   mm41      : numeric vector of t(Xc^4) %*% Xc / NN
   mm61      : numeric vector of t(Xc^6) %*% Xc / NN
   rr2       : generalized correlation coefficient of order 2
   rr4       : generalized correlation coefficient of order 4
   rr5       : generalized correlation coefficient of order 5
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m31, *m32, *m41, *m61, *margvars, *margskews, *margkurts, *marg5s, *marg6s;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m31 = REAL(mm31);
  m32 = REAL(mm32);
  m41 = REAL(mm41);
  m61 = REAL(mm61);
  margvars = REAL(mmargvars);
  margskews = REAL(mmargskews);
  margkurts = REAL(mmargkurts);
  marg5s = REAL(mmarg5s);
  marg6s = REAL(mmarg6s);
  double r2 = asReal(rr2);
  double r4 = asReal(rr4);
  double r5 = asReal(rr5);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  
  // allocate and compute the sample variance and covariances
  SEXP CM3vec = PROTECT(allocVector(REALSXP, 1));
  double *rCM3vec = REAL(CM3vec);
  rCM3vec[0] = 0.0;
  
  // loop over the unique indices i <= j <= k
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        if (ii == jj) {
          if (jj == kk) {
            // element phi_iii
            rCM3vec[0] += (marg6s[ii] - margskews[ii] * margskews[ii] - 6.0 * margkurts[ii] * margvars[ii] +
              9.0 * margvars[ii] * margvars[ii] * margvars[ii]);
          } else {
            // element phi_iik
            double temp_ii4 = m61[kkP + ii] - m21[kkP + ii] * margkurts[ii] - 4.0 * m31[kkP + ii] * margskews[ii] -
              2.0 * m11[kkP + ii] * marg5s[ii] - margvars[ii] * m41[kkP + ii] +
              12.0 * margvars[ii] * m11[kkP + ii] * margskews[ii];
            double temp_kk2 = m32[iiP + kk] - margvars[kk] * m21[kkP + ii] - margskews[kk] * margvars[ii] -
              2.0 * m21[iiP + kk] * m11[kkP + ii];
            
            rCM3vec[0] += 3.0 * r2 * (sqrt(margvars[kk] / margkurts[ii]) * temp_ii4 +
              sqrt(margkurts[ii] / margvars[kk]) * temp_kk2) / 2.0;
          }
        } else {
          if (jj == kk) {
            // element phi_ijj
            double temp_ii2 = m32[jjP + ii] - margvars[ii] * m21[iiP + jj] - margskews[ii] * margvars[jj] -
              2.0 * m21[jjP + ii] * m11[jjP + ii];
            double temp_jj4 = m61[iiP + jj] - m21[iiP + jj] * margkurts[jj] - 4.0 * m31[iiP + jj] * margskews[jj] -
              2.0 * m11[jjP + ii] * marg5s[jj] - margvars[jj] * m41[iiP + jj] +
              12.0 * margvars[jj] * m11[jjP + ii] * margskews[jj];
            
            rCM3vec[0] += 3.0 * r2 * (sqrt(margvars[ii] / margkurts[jj]) * temp_jj4 +
              sqrt(margkurts[jj] / margvars[ii]) * temp_ii2) / 2.0;
          } else {
            // element phi_ijk
            double S511 = 0.0;
            double S151 = 0.0;
            double S115 = 0.0;
            double S211 = 0.0;
            double S121 = 0.0;
            double S112 = 0.0;
            double S311 = 0.0;
            double S131 = 0.0;
            double S113 = 0.0;
            double m111 = 0.0;
            for (int tt = 0; tt < n; tt++) {
              S511 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S151 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S115 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt] * Xc[kkN + tt];
              S211 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S121 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt];
              S112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
              S311 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S131 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
              S113 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc[kkN + tt];
              m111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
            }
            m111 /= N;
            
            double temp_ii4 = S511 / N - m111 * margkurts[ii] - 4.0 * margskews[ii] * S211 / N -
              marg5s[ii] * m11[kkP + jj] - m41[jjP + ii] * m11[kkP + ii] - m41[kkP + ii] * m11[jjP + ii] +
              8.0 * margskews[ii] * m11[jjP + ii] * m11[kkP + ii] + 4.0 * margskews[ii] * margvars[ii] * m11[kkP + jj];
            double temp_jj4 = S151 / N - m111 * margkurts[jj] - 4.0 * margskews[jj] * S121 / N -
              marg5s[jj] * m11[kkP + ii] - m41[kkP + jj] * m11[iiP + jj] - m41[iiP + jj] * m11[kkP + jj] +
              8.0 * margskews[jj] * m11[kkP + jj] * m11[iiP + jj] + 4.0 * margskews[jj] * margvars[jj] * m11[kkP + ii];
            double temp_kk4 = S115 / N - m111 * margkurts[kk] - 4.0 * margskews[kk] * S112 / N -
              marg5s[kk] * m11[jjP + ii] - m41[iiP + kk] * m11[jjP + kk] - m41[jjP + kk] * m11[iiP + kk] +
              8.0 * margskews[kk] * m11[iiP + kk] * m11[jjP + kk] + 4.0 * margskews[kk] * margvars[kk] * m11[jjP + ii];
            
            double temp_ii2 = S311 / N - margvars[ii] * m111 - margskews[ii] * m11[kkP + jj] -
              m21[jjP + ii] * m11[kkP + ii] - m21[kkP + ii] * m11[jjP + ii];
            double temp_jj2 = S131 / N - margvars[jj] * m111 - margskews[jj] * m11[kkP + ii] -
              m21[iiP + jj] * m11[kkP + jj] - m21[kkP + jj] * m11[iiP + jj];
            double temp_kk2 = S113 / N - margvars[kk] * m111 - margskews[kk] * m11[jjP + ii] -
              m21[iiP + kk] * m11[jjP + kk] - m21[jjP + kk] * m11[iiP + kk];
            
            double cov_ii = sqrt(margvars[ii] * sqrt(margkurts[jj] / (margkurts[kk] * margkurts[kk] * margkurts[kk]))) * temp_kk4 / 2.0 +
              sqrt(margvars[ii] * sqrt(margkurts[kk] / (margkurts[jj] * margkurts[jj] * margkurts[jj]))) * temp_jj4 / 2.0 +
              sqrt(sqrt(margkurts[jj] * margkurts[kk]) / margvars[ii]) * temp_ii2;
            double cov_jj = sqrt(margvars[jj] * sqrt(margkurts[ii] / (margkurts[kk] * margkurts[kk] * margkurts[kk]))) * temp_kk4 / 2.0 +
              sqrt(margvars[jj] * sqrt(margkurts[kk] / (margkurts[ii] * margkurts[ii] * margkurts[ii]))) * temp_ii4 / 2.0 +
              sqrt(sqrt(margkurts[ii] * margkurts[kk]) / margvars[jj]) * temp_jj2;
            double cov_kk = sqrt(margvars[kk] * sqrt(margkurts[jj] / (margkurts[ii] * margkurts[ii] * margkurts[ii]))) * temp_ii4 / 2.0 +
              sqrt(margvars[kk] * sqrt(margkurts[ii] / (margkurts[jj] * margkurts[jj] * margkurts[jj]))) * temp_jj4 / 2.0 +
              sqrt(sqrt(margkurts[ii] * margkurts[jj]) / margvars[kk]) * temp_kk2;
            
            rCM3vec[0] += r4 * sqrt(r5) * (cov_ii + cov_jj + cov_kk);
          }
        }
      } // loop kk
    } // loop jj
  } // loop ii
  
  rCM3vec[0] /= N;
  
  UNPROTECT(1);
  return CM3vec;
}


// // //
// // Shrinkage and structured estimators for cokurtosis
// // //

SEXP  M4_T12(SEXP mmargk_iiii, SEXP mmargk_iikk, SEXP PP){
  /*
   arguments
   mmargk_iiii : vector of length PP with marginal kurtosis values
   mmargk_iikk : vector of length PP with k_iikk values
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margk_iiii, *margk_iikk;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margk_iiii = REAL(mmargk_iiii);
  margk_iikk = REAL(mmargk_iikk);
  int P = asInteger(PP);
  
  // allocate and compute the coskewness matrix
  SEXP M4 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) * (P + 3) / 24));
  double *rM4 = REAL(M4);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        for (int ll = kk; ll < P; ll++) {
          // compute coskewness element
          double elem = 0.0;
          if (((ii == jj) & (jj == kk)) & (kk == ll)) elem = margk_iiii[ii];
          if (((ii == jj) & (kk == ll)) & (jj != kk)) elem = margk_iikk[ii] * margk_iikk[kk];
          
          // fill in the element and update the iterator
          rM4[iter] = elem;
          iter++;
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M4;
}

SEXP  M4_CCoefficients(SEXP mmargvars, SEXP mmargkurts, SEXP mmarg6s,
                       SEXP mm22, SEXP mm31, SEXP XXc, SEXP NN, SEXP PP){
  /*
   arguments
   mmargvars : vector of length PP with marginal variances
   mmargkurts : vector of length PP with marginal kurtosis values
   mmarg6s   : vector of length PP with marginal 6th order moments
   mm22      : t(Xc^2) %*% Xc^2
   mm31      : t(Xc^3) %*% Xc
   XXc       : numeric vector with centered observations
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margvars, *margkurts, *marg6s, *m22, *m31, *Xc;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margvars = REAL(mmargvars);
  margkurts = REAL(mmargkurts);
  marg6s = REAL(mmarg6s);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  Xc = REAL(XXc);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  double p = asReal(PP);
  
  // allocate and compute the generalized correlation coefficients
  SEXP CCoef = PROTECT(allocVector(REALSXP, 4));
  double *rCCoef = REAL(CCoef);
  
  // compute the generalized correlation coefficients
  rCCoef[0] = 0.0;
  rCCoef[1] = 0.0;
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      rCCoef[0] += m31[jjP + ii] / sqrt(marg6s[ii] * margvars[jj]);
      rCCoef[1] += m22[jjP + ii] / sqrt(margkurts[ii] * margkurts[jj]);
    }
  }
  rCCoef[0] *= 2.0 / (p * (p - 1.0));
  rCCoef[1] *= 2.0 / (p * (p - 1.0));
  
  rCCoef[2] = 0.0;
  rCCoef[3] = 0.0;
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * n;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjN = jj * n;
      for (int kk = jj + 1; kk < P; kk++) {
        int kkN = kk * n;
        double m211 = 0.0;
        for (int tt = 0; tt < n; tt++) {
          m211 += Xc[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
        }
        m211 /= N;
        rCCoef[2] += m211 / sqrt(margkurts[ii] * rCCoef[1] * sqrt(margkurts[jj] * margkurts[kk]));
        
        for (int ll = kk + 1; ll < P; ll++) {
          int llN = ll * n;
          double m1111 = 0.0;
          for (int tt = 0; tt < n; tt++) {
            m1111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
          }
          m1111 /= N;
          rCCoef[3] += m1111 / (rCCoef[1] * sqrt(sqrt(margkurts[ii] * margkurts[jj] * margkurts[kk] * margkurts[ll])));
        }
      }
    }
  }
  rCCoef[2] *= 6.0 / (p * (p - 1.0) * (p - 2.0));
  rCCoef[3] *= 24.0 / (p * (p - 1.0) * (p - 2.0) * (p - 3.0));
  
  UNPROTECT(1);
  return CCoef;
}

SEXP  M4_CC(SEXP mmargvars, SEXP mmargkurts, SEXP mmarg6s,
            SEXP rr3, SEXP rr5, SEXP rr6, SEXP rr7, SEXP PP){
  /*
   arguments
   mmargvars : vector of length PP with marginal variances
   mmargkurts : vector of length PP with marginal kurtosis values
   mmarg6s   : vector of length PP with marginal 6th order central moments
   rr3       : generalized correlation of order 3
   rr5       : generalized correlation of order 5
   rr6       : generalized correlation of order 6
   rr7       : generalized correlation of order 7
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margvars, *margkurts, *marg6s;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margvars = REAL(mmargvars);
  margkurts = REAL(mmargkurts);
  marg6s = REAL(mmarg6s);
  double r3 = asReal(rr3);
  double r5 = asReal(rr5);
  double r6 = asReal(rr6);
  double r7 = asReal(rr7);
  int P = asInteger(PP);
  
  // allocate and compute the cokurtosis matrix
  SEXP M4 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) * (P + 3) / 24));
  double *rM4 = REAL(M4);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        for (int ll = kk; ll < P; ll++) {
          // compute and fill cokurtosis element
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                rM4[iter] = margkurts[ii];
              } else {
                // psi_iiil
                rM4[iter] = r3 * sqrt(marg6s[ii] * margvars[ll]);
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                rM4[iter] = r5 * sqrt(margkurts[ii] * margkurts[kk]);
              } else {
                // psi_iikl
                rM4[iter] = r6 * sqrt(margvars[ii] * r5 * sqrt(margkurts[kk] * margkurts[ll]));
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                rM4[iter] = r3 * sqrt(margvars[ii] * marg6s[jj]);
              } else {
                // psi_ijjl
                rM4[iter] = r6 * sqrt(margvars[jj] * r5 * sqrt(margkurts[ii] * margkurts[ll]));
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                rM4[iter] = r6 * sqrt(margvars[kk] * r5 * sqrt(margkurts[ii] * margkurts[jj]));
              } else {
                // psi_ijkl
                rM4[iter] = r7 * r5 * sqrt(sqrt(margkurts[ii] * margkurts[jj] * margkurts[kk] * margkurts[ll]));
              }
            }
          }
          iter++;
        } // loop ii
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M4;
}

SEXP  M4_1f(SEXP mmargkurts, SEXP ffvar, SEXP ffkurt, SEXP eepsvars, SEXP bbeta, SEXP PP){
  /*
   arguments
   mmargkurts : vector of length PP with marginal kurtosis values
   ffvar     : double with the factor variance
   ffkurt    : vector with the factor fourth order central moment
   eepsvars  : numeric vector with the variances of the idiosyncratic term
   bbeta     : regression coefficients (factor loadings)
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *margkurts, *epsvars, *beta;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  margkurts = REAL(mmargkurts);
  epsvars = REAL(eepsvars);
  beta = REAL(bbeta);
  double fvar = asReal(ffvar);
  double fkurt = asReal(ffkurt);
  int P = asInteger(PP);
  
  // allocate and compute the cokurtosis matrix
  SEXP M4 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) * (P + 3) / 24));
  double *rM4 = REAL(M4);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        for (int ll = kk; ll < P; ll++) {
          // compute and fill cokurtosis element
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                rM4[iter] = margkurts[ii];
              } else {
                // psi_iiil
                rM4[iter] = beta[ii] * beta[ii] * beta[ii] * beta[ll] * fkurt +
                  3.0 * beta[ii] * beta[ll] * fvar * epsvars[ii];
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                rM4[iter] = beta[ii] * beta[ii] * beta[kk] * beta[kk] * fkurt +
                  fvar * (beta[ii] * beta[ii] * epsvars[kk] + beta[kk] * beta[kk] * epsvars[ii]) +
                  epsvars[ii] * epsvars[kk];
              } else {
                // psi_iikl
                rM4[iter] = beta[ii] * beta[ii] * beta[kk] * beta[ll] * fkurt +
                  beta[kk] * beta[ll] * fvar * epsvars[ii];
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                rM4[iter] = beta[ii] * beta[jj] * beta[jj] * beta[jj] * fkurt +
                  3.0 * beta[ii] * beta[jj] * fvar * epsvars[jj];
              } else {
                // psi_ijjl
                rM4[iter] = beta[ii] * beta[jj] * beta[jj] * beta[ll] * fkurt +
                  beta[ii] * beta[ll] * fvar * epsvars[jj];
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                rM4[iter] = beta[ii] * beta[jj] * beta[kk] * beta[kk] * fkurt +
                  beta[ii] * beta[jj] * fvar * epsvars[kk];
              } else {
                // psi_ijkl
                rM4[iter] = beta[ii] * beta[jj] * beta[kk] * beta[ll] * fkurt;
              }
            }
          }
          iter++;
        } // loop ii
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M4;
}

SEXP  M4_MFresid(SEXP SStransf, SEXP eepsvars, SEXP PP){
  /*
  arguments
  SStransf  : contains the vectorized matrix B %*% S_F %*% t(B)
  eepsvars  : numeric vector with the variances of the idiosyncratic term
  PP        : integer, number of assets
  
  Written by Dries Cornilly
  */
  
  // // declare pointers for the vector
  double *Stransf, *epsvars;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  epsvars = REAL(eepsvars);
  Stransf = REAL(SStransf);
  int P = asInteger(PP);
  
  // allocate and compute the cokurtosis matrix
  SEXP M4 = PROTECT(allocVector(REALSXP, P * (P + 1) * (P + 2) * (P + 3) / 24));
  double *rM4 = REAL(M4);
  
  int iter = 0;
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        for (int ll = kk; ll < P; ll++) {
          // compute and fill cokurtosis element
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                rM4[iter] = 6.0 * Stransf[iiP + ii] * epsvars[ii];
              } else {
                // psi_iiil
                rM4[iter] = 3.0 * Stransf[iiP + ll] * epsvars[ii];
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                rM4[iter] = Stransf[iiP + ii] * epsvars[kk] + Stransf[kkP + kk] * epsvars[ii];
              } else {
                // psi_iikl
                rM4[iter] = Stransf[kkP + ll] * epsvars[ii];
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                rM4[iter] = 3.0 * Stransf[iiP + jj] * epsvars[jj];
              } else {
                // psi_ijjl
                rM4[iter] = Stransf[iiP + ll] * epsvars[jj];
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                rM4[iter] = Stransf[iiP + jj] * epsvars[kk];
              } else {
                // psi_ijkl
                rM4[iter] = 0.0;
              }
            }
          }
          iter++;
        } // loop ii
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return M4;
}

SEXP  VM4(SEXP XXc, SEXP XXc2, SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm31,
          SEXP mm32, SEXP mm41, SEXP mm42, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   mm32      : numeric vector of t(Xc^3) %*% Xc^2 / NN
   mm41      : numeric vector of t(Xc^4) %*% Xc / NN
   mm42      : numeric vector of t(Xc^4) %*% Xc^2 / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m22, *m31, *m32, *m41, *m42;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  m32 = REAL(mm32);
  m41 = REAL(mm41);
  m42 = REAL(mm42);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  double p = asReal(PP);
  
  // allocate and compute the sample variance and covariances
  SEXP VM4vec = PROTECT(allocVector(REALSXP, 3));
  double *rVM4vec = REAL(VM4vec);
  for (int ii = 0; ii < 3; ii++) {
    rVM4vec[ii] = 0.0;
  }
  
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        for (int ll = kk; ll < P; ll++) {
          int llP = ll * P;
          int llN = ll * n;
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                double S8 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S8 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt];
                }
                
                double temp = (S8 / N - m31[iiP + ii] * m31[iiP + ii] -
                               8.0 * m32[iiP + ii] * m21[iiP + ii] +
                               16.0 * m11[iiP + ii] * m21[iiP + ii] * m21[iiP + ii]) / N;
                rVM4vec[0] += temp;
                rVM4vec[1] += temp / p; // cov with T (equal marginals)
                rVM4vec[2] += temp; // cov with T(unequal marginals)
              } else {
                // psi_iiil
                double S62 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S62 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[llN + tt];
                }
                
                rVM4vec[0] += 4.0 * (S62 / N - m31[llP + ii] * m31[llP + ii] -
                  6.0 * m41[llP + ii] * m21[llP + ii] - 2.0 * m32[llP + ii] * m21[iiP + ii] +
                  6.0 * m11[llP + ii] * m21[llP + ii] * m21[iiP + ii] +
                  6.0 * m11[iiP + ii] * m21[llP + ii] * m21[llP + ii] +
                  m21[iiP + ii] * m21[iiP + ii] * m11[llP + ll] +
                  3.0 * m21[llP + ii] * m21[llP + ii] * m11[iiP + ii]) / N;
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                double S44 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S44 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt];
                }
                
                rVM4vec[0] += 6.0 * (S44 / N - m22[kkP + ii] * m22[kkP + ii] -
                  4.0 * m32[kkP + ii] * m21[iiP + kk] - 4.0 * m32[iiP + kk] * m21[kkP + ii] +
                  8.0 * m11[kkP + ii] * m21[iiP + kk] * m21[kkP + ii] +
                  4.0 * m21[kkP + ii] * m21[kkP + ii] * m11[kkP + kk] +
                  4.0 * m21[iiP + kk] * m21[iiP + kk] * m11[iiP + ii]) / N;
                
                // cov with T (equal marginals)
                double temp = 0.0;
                for (int mm = 0; mm < P; mm++) {
                  int mmP = mm * P;
                  int mmN = mm * n;
                  double S222 = 0.0;
                  for (int tt = 0; tt < N; tt++) {
                    S222 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[mmN + tt];
                  }
                  temp += 2.0 * m11[mmP + mm] * (S222 / N - m11[mmP + mm] * m22[kkP + ii] -
                    2.0 * m21[iiP + mm] * m21[iiP + kk] - 2.0 * m21[kkP + mm] * m21[kkP + ii]) / N;
                }
                rVM4vec[1] += 6.0 * temp / p;
                
                // cov with T (unequal marginals)
                double temp_ii = m42[kkP + ii] - m11[iiP + ii] * m22[kkP + ii] -
                  2.0 * m21[iiP + ii] * m21[iiP + kk] - 2.0 * m21[kkP + ii] * m21[kkP + ii];
                double temp_kk = m42[iiP + kk] - m11[kkP + kk] * m22[iiP + kk] -
                  2.0 * m21[kkP + kk] * m21[kkP + ii] - 2.0 * m21[iiP + kk] * m21[iiP + kk];
                rVM4vec[2] += 6.0 * (m11[kkP + kk] * temp_ii + m11[iiP + ii] * temp_kk) / N;
              } else {
                // psi_iikl
                double S422 = 0.0;
                double S311 = 0.0;
                double S221 = 0.0;
                double S212 = 0.0;
                double m211 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S422 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[llN + tt];
                  S311 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S221 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc[llN + tt];
                  S212 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc2[llN + tt];
                  m211 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m111 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                }
                m211 /= N;
                m111 /= N;
                
                rVM4vec[0] += 12.0 * (S422 / N - m211 * m211 - 4.0 * S311 / N * m111 -
                  2.0 * S221 / N * m21[llP + ii] - 2.0 * S212 / N * m21[kkP + ii] +
                  4.0 * m11[llP + ii] * m21[kkP + ii] * m111 + 4.0 * m11[kkP + ii] * m111 * m21[llP + ii] +
                  2.0 * m11[llP + kk] * m21[llP + ii] * m21[kkP + ii] +
                  4.0 * m11[iiP + ii] * m111 * m111 + m21[kkP + ii] * m21[kkP + ii] * m11[llP + ll] +
                  m21[llP + ii] * m21[llP + ii] * m11[kkP + kk]) / N;
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                double S62 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S62 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[iiN + tt];
                }
                
                rVM4vec[0] += 4.0 * (S62 / N - m31[iiP + jj] * m31[iiP + jj] -
                  6.0 * m41[iiP + jj] * m21[iiP + jj] - 2.0 * m32[iiP + jj] * m21[jjP + jj] +
                  6.0 * m11[iiP + jj] * m21[iiP + jj] * m21[jjP + jj] +
                  6.0 * m11[jjP + jj] * m21[iiP + jj] * m21[iiP + jj] +
                  m21[jjP + jj] * m21[jjP + jj] * m11[iiP + ii] +
                  3.0 * m21[iiP + jj] * m21[iiP + jj] * m11[jjP + jj]) / N;
              } else {
                // psi_ijjl
                double S422 = 0.0;
                double S311 = 0.0;
                double S221 = 0.0;
                double S212 = 0.0;
                double m211 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S422 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[iiN + tt] * Xc2[llN + tt];
                  S311 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  S221 += Xc2[jjN + tt] * Xc2[iiN + tt] * Xc[llN + tt];
                  S212 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc2[llN + tt];
                  m211 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  m111 += Xc[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                }
                m211 /= N;
                m111 /= N;
                
                rVM4vec[0] += 12.0 * (S422 / N - m211 * m211 - 4.0 * S311 / N * m111 -
                  2.0 * S221 / N * m21[llP + jj] - 2.0 * S212 / N * m21[iiP + jj] +
                  4.0 * m11[llP + jj] * m21[iiP + jj] * m111 + 4.0 * m11[iiP + jj] * m111 * m21[llP + jj] +
                  2.0 * m11[llP + ii] * m21[llP + jj] * m21[iiP + jj] +
                  4.0 * m11[jjP + jj] * m111 * m111 + m21[iiP + jj] * m21[iiP + jj] * m11[llP + ll] +
                  m21[llP + jj] * m21[llP + jj] * m11[iiP + ii]) / N;
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                double S422 = 0.0;
                double S311 = 0.0;
                double S221 = 0.0;
                double S212 = 0.0;
                double m211 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S422 += Xc2[kkN + tt] * Xc2[kkN + tt] * Xc2[iiN + tt] * Xc2[jjN + tt];
                  S311 += Xc2[kkN + tt] * Xc[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                  S221 += Xc2[kkN + tt] * Xc2[iiN + tt] * Xc[jjN + tt];
                  S212 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc2[jjN + tt];
                  m211 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                  m111 += Xc[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                }
                m211 /= N;
                m111 /= N;
                
                rVM4vec[0] += 12.0 * (S422 / N - m211 * m211 - 4.0 * S311 / N * m111 -
                  2.0 * S221 / N * m21[jjP + kk] - 2.0 * S212 / N * m21[iiP + kk] +
                  4.0 * m11[jjP + kk] * m21[iiP + kk] * m111 + 4.0 * m11[iiP + kk] * m111 * m21[jjP + kk] +
                  2.0 * m11[jjP + ii] * m21[jjP + kk] * m21[iiP + kk] +
                  4.0 * m11[kkP + kk] * m111 * m111 + m21[iiP + kk] * m21[iiP + kk] * m11[jjP + jj] +
                  m21[jjP + kk] * m21[jjP + kk] * m11[iiP + ii]) / N;
              } else {
                // psi_ijkl
                double S2222 = 0.0;
                double S2111 = 0.0;
                double S1211 = 0.0;
                double S1121 = 0.0;
                double S1112 = 0.0;
                double m1111 = 0.0;
                double m0111 = 0.0;
                double m1011 = 0.0;
                double m1101 = 0.0;
                double m1110 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S2222 += Xc2[iiN + tt] * Xc2[jjN + tt] * Xc2[kkN + tt] * Xc2[llN + tt];
                  S2111 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1211 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1121 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc[llN + tt];
                  S1112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc2[llN + tt];
                  m1111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m0111 += Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m1011 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m1101 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[llN + tt];
                  m1110 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
                }
                m1111 /= N;
                m0111 /= N;
                m1011 /= N;
                m1101 /= N;
                m1110 /= N;
                
                rVM4vec[0] += 24.0 * (S2222 / N - m1111 * m1111 - 2.0 * S2111 / N * m0111 -
                  2.0 * S1211 / N * m1011 - 2.0 * S1121 / N * m1101 - 2.0 * S1112 / N * m1110 +
                  2.0 * m11[llP + ii] * m1110 * m0111 + 2.0 * m11[llP + jj] * m1011 * m1110 +
                  2.0 * m11[llP + kk] * m1101 * m1110 + 2.0 * m11[kkP + ii] * m0111 * m1101 +
                  2.0 * m11[kkP + jj] * m1011 * m1101 + 2.0 * m11[jjP + ii] * m0111 * m1011 +
                  m1110 * m1110 * m11[llP + ll] + m1101 * m1101 * m11[kkP + kk] +
                  m1011 * m1011 * m11[jjP + jj] + m0111 * m0111 * m11[iiP + ii]) / N;
              }
            }
          }
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii
  
  // cov with T (equal marginals)
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii + 1; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      double S44 = 0.0;
      for (int tt = 0; tt < n; tt++) {
        S44 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt];
      }
      rVM4vec[1] += 2 * (S44 / N - m22[iiP + ii] * m22[jjP + jj] -
        4.0 * m41[jjP + ii] * m21[jjP + jj] - 4.0 * m41[iiP + jj] * m21[iiP + ii] +
        16.0 * m21[iiP + ii] * m21[jjP + jj] * m11[jjP + ii]) / (N * p);
    } // loop over jj
  } // loop over ii
  
  UNPROTECT(1);
  return VM4vec;
}

SEXP  CM4_CC(SEXP XXc, SEXP XXc2, SEXP mm11, SEXP mm21, SEXP mm22,
             SEXP mm31, SEXP mm32, SEXP mm33, SEXP mm41, SEXP rr3,
             SEXP rr5, SEXP rr6, SEXP rr7, SEXP mmarg6s, SEXP mmarg7s,
             SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   mm32      : numeric vector of t(Xc^3) %*% Xc^2 / NN
   mm33      : numeric vector of t(Xc^3) %*% Xc^3 / NN
   mm41      : numeric vector of t(Xc^4) %*% Xc / NN
   rr3       : double with generalized correlation coefficient of order 3
   rr5       : double with generalized correlation coefficient of order 5
   rr6       : double with generalized correlation coefficient of order 6
   rr7       : double with generalized correlation coefficient of order 7
   mmarg6s   : numeric vector with marginal 6th order central moments
   mmarg7s   : numeric vector with marginal 7th order central moments
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *m11, *m21, *m22, *m31, *m32, *m33, *m41, *marg6s, *marg7s;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  m32 = REAL(mm32);
  m33 = REAL(mm33);
  m41 = REAL(mm41);
  marg6s = REAL(mmarg6s);
  marg7s = REAL(mmarg7s);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  double r3 = asReal(rr3);
  double r5 = asReal(rr5);
  double r6 = asReal(rr6);
  double r7 = asReal(rr7);
  
  // allocate and compute the covariance
  SEXP CM4 = PROTECT(allocVector(REALSXP, 1));
  double *rCM4 = REAL(CM4);
  rCM4[0] = 0.0;
  
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        for (int ll = kk; ll < P; ll++) {
          int llP = ll * P;
          int llN = ll * n;
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                double S8 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S8 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt];
                }
                
                rCM4[0] += (S8 / N - m31[iiP + ii] * m31[iiP + ii] -
                  8.0 * m41[iiP + ii] * m21[iiP + ii] +
                  16.0 * m11[iiP + ii] * m21[iiP + ii] * m21[iiP + ii]) / N;
              } else {
                // psi_iiil
                double S91 = 0.0;
                double S62 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S91 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] *
                    Xc2[iiN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  S62 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[llN + tt];
                }
                
                double temp_ii = S91 / N - m31[llP + ii] * marg6s[ii] -
                  3.0 * m21[llP + ii] * marg7s[ii] - m21[iiP + ii] * S62 / N -
                  6.0 * m41[iiP + ii] * m41[llP + ii] +
                  18.0 * m41[iiP + ii] * m21[llP + ii] * m11[iiP + ii] +
                  6.0 * m41[iiP + ii] * m21[iiP + ii] * m11[llP + ii];
                double temp_ll = m33[llP + ii] - m31[llP + ii] * m11[llP + ll] -
                  3.0 * m21[iiP + ll] * m21[llP + ii] - m21[iiP + ii] * m21[llP + ll];
                
                rCM4[0] += 2.0 * r3 * (sqrt(m11[llP + ll] / marg6s[ii]) * temp_ii +
                  sqrt(marg6s[ii] / m11[llP + ll]) * temp_ll) / N;
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                double S62 = 0.0;
                double S26 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S62 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[kkN + tt];
                  S26 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt];
                }
                
                double temp_ii = S62 / N - m22[kkP + ii] * m22[iiP + ii] -
                  4.0 * m32[kkP + ii] * m21[iiP + ii] - 2.0 * m21[iiP + kk] * m32[iiP + ii] -
                  2.0 * m21[kkP + ii] * m41[kkP + ii] + 8.0 * m21[kkP + ii] * m21[iiP + ii] * m11[kkP + ii] +
                  8.0 * m21[iiP + kk] * m21[iiP + ii] * m11[iiP + ii];
                double temp_kk = S26 / N - m22[iiP + kk] * m22[kkP + kk] -
                  4.0 * m32[iiP + kk] * m21[kkP + kk] - 2.0 * m21[kkP + ii] * m32[kkP + kk] -
                  2.0 * m21[iiP + kk] * m41[iiP + kk] + 8.0 * m21[iiP + kk] * m21[kkP + kk] * m11[iiP + kk] +
                  8.0 * m21[kkP + ii] * m21[kkP + kk] * m11[kkP + kk];
                
                rCM4[0] += 3.0 * r5 * (sqrt(m22[kkP + kk] / m22[iiP + ii]) * temp_ii +
                  sqrt(m22[iiP + ii] / m22[kkP + kk]) * temp_kk) / N;
              } else {
                // psi_iikl
                double S611 = 0.0;
                double S251 = 0.0;
                double S215 = 0.0;
                double S311 = 0.0;
                double S221 = 0.0;
                double S212 = 0.0;
                double m211 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S611 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S251 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S215 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc2[llN + tt] * Xc2[llN + tt] * Xc[llN + tt];
                  S311 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S221 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc[llN + tt];
                  S212 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc2[llN + tt];
                  m211 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m111 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                }
                m211 /= N;
                m111 /= N;
                
                double temp_ii = S611 / N - m211 * m22[iiP + ii] - 4.0 * S311 / N * m21[iiP + ii] -
                  2.0 * m111 * m32[iiP + ii] - m21[llP + ii] * m41[kkP + ii] - m21[kkP + ii] * m41[llP + ii] +
                  4.0 * m21[kkP + ii] * m21[iiP + ii] * m11[llP + ii] + 4.0 * m21[llP + ii] * m21[iiP + ii] * m11[kkP + ii] +
                  8.0 * m111 * m21[iiP + ii] * m11[iiP + ii];
                double temp_kk = S251 / N - m211 * m22[kkP + kk] - 4.0 * S221 / N * m21[kkP + kk] -
                  m21[llP + ii] * m32[kkP + kk] - 2.0 * m111 * m41[iiP + kk] - m21[kkP + ii] * m41[llP + kk] +
                  4.0 * m21[kkP + ii] * m21[kkP + kk] * m11[llP + kk] + 8.0 * m111 * m21[kkP + kk] * m11[kkP + ii] +
                  4.0 * m21[llP + ii] * m11[kkP + kk] * m21[kkP + kk];
                double temp_ll = S215 / N - m211 * m22[llP + ll] - 4.0 * S212 / N * m21[llP + ll] -
                  m21[kkP + ii] * m32[llP + ll] - 2.0 * m111 * m41[iiP + ll] - m21[llP + ii] * m41[kkP + ll] +
                  4.0 * m21[llP + ii] * m21[llP + ll] * m11[kkP + ll] + 8.0 * m111 * m21[llP + ll] * m11[llP + ii] +
                  4.0 * m21[kkP + ii] * m11[llP + ll] * m21[llP + ll];
                
                rCM4[0] += 3.0 * r6 * sqrt(r5) *
                  (2.0 * sqrt(sqrt(m22[kkP + kk] * m22[llP + ll]) / m22[iiP + ii]) * temp_ii +
                  sqrt(m22[iiP + ii] * sqrt(m22[llP + ll] / (m22[kkP + kk] * m22[kkP + kk] * m22[kkP + kk]))) * temp_kk +
                  sqrt(m22[iiP + ii] * sqrt(m22[kkP + kk] / (m22[llP + ll] * m22[llP + ll] * m22[llP + ll]))) * temp_ll) / N;
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                double S91 = 0.0;
                double S62 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S91 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] *
                    Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt];
                  S62 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[iiN + tt];
                }
                
                double temp_jj = S91 / N - m31[iiP + jj] * marg6s[jj] -
                  3.0 * m21[iiP + jj] * marg7s[jj] - m21[jjP + jj] * S62 / N -
                  6.0 * m41[jjP + jj] * m41[iiP + jj] +
                  18.0 * m41[jjP + jj] * m21[iiP + jj] * m11[jjP + jj] +
                  6.0 * m41[jjP + jj] * m21[jjP + jj] * m11[iiP + jj];
                double temp_ii = m33[iiP + jj] - m31[iiP + jj] * m11[iiP + ii] -
                  3.0 * m21[jjP + ii] * m21[iiP + jj] - m21[jjP + jj] * m21[iiP + ii];
                
                rCM4[0] += 2.0 * r3 * (sqrt(m11[iiP + ii] / marg6s[jj]) * temp_jj +
                  sqrt(marg6s[jj] / m11[iiP + ii]) * temp_ii) / N;
              } else {
                // psi_ijjl
                double S161 = 0.0;
                double S521 = 0.0;
                double S125 = 0.0;
                double S131 = 0.0;
                double S221 = 0.0;
                double S122 = 0.0;
                double m121 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S161 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[llN + tt];
                  S521 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt] * Xc2[jjN + tt] * Xc[llN + tt];
                  S125 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[llN + tt] * Xc2[llN + tt] * Xc[llN + tt];
                  S131 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc[llN + tt];
                  S221 += Xc2[iiN + tt] * Xc2[jjN + tt] * Xc[llN + tt];
                  S122 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[llN + tt];
                  m121 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[llN + tt];
                  m111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[llN + tt];
                }
                m121 /= N;
                m111 /= N;
                
                double temp_jj = S161 / N - m121 * m22[jjP + jj] - 4.0 * S131 / N * m21[jjP + jj] -
                  2.0 * m111 * m32[jjP + jj] - m21[llP + jj] * m41[iiP + jj] - m21[iiP + jj] * m41[llP + jj] +
                  4.0 * m21[iiP + jj] * m21[jjP + jj] * m11[llP + jj] + 4.0 * m21[llP + jj] * m21[jjP + jj] * m11[iiP + jj] +
                  8.0 * m111 * m21[jjP + jj] * m11[jjP + jj];
                double temp_ii = S521 / N - m121 * m22[iiP + ii] - 4.0 * S221 / N * m21[iiP + ii] -
                  m21[llP + jj] * m32[iiP + ii] - 2.0 * m111 * m41[jjP + ii] - m21[iiP + jj] * m41[llP + ii] +
                  4.0 * m21[iiP + jj] * m21[iiP + ii] * m11[llP + ii] + 8.0 * m111 * m21[iiP + ii] * m11[jjP + ii] +
                  4.0 * m21[llP + jj] * m11[iiP + ii] * m21[iiP + ii];
                double temp_ll = S125 / N - m121 * m22[llP + ll] - 4.0 * S122 / N * m21[llP + ll] -
                  m21[iiP + jj] * m32[llP + ll] - 2.0 * m111 * m41[jjP + ll] - m21[llP + jj] * m41[iiP + ll] +
                  4.0 * m21[llP + jj] * m21[llP + ll] * m11[iiP + ll] + 8.0 * m111 * m21[llP + ll] * m11[jjP + ll] +
                  4.0 * m21[iiP + jj] * m11[llP + ll] * m21[llP + ll];
                
                rCM4[0] += 3.0 * r6 * sqrt(r5) *
                  (2.0 * sqrt(sqrt(m22[iiP + ii] * m22[llP + ll]) / m22[jjP + jj]) * temp_jj +
                  sqrt(m22[jjP + jj] * sqrt(m22[llP + ll] / (m22[iiP + ii] * m22[iiP + ii] * m22[iiP + ii]))) * temp_ii +
                  sqrt(m22[jjP + jj] * sqrt(m22[iiP + ii] / (m22[llP + ll] * m22[llP + ll] * m22[llP + ll]))) * temp_ll) / N;
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                double S116 = 0.0;
                double S152 = 0.0;
                double S512 = 0.0;
                double S113 = 0.0;
                double S122 = 0.0;
                double S212 = 0.0;
                double m112 = 0.0;
                double m111 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S116 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt];
                  S152 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
                  S512 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
                  S113 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc[kkN + tt];
                  S122 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[kkN + tt];
                  S212 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
                  m112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt];
                  m111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
                }
                m112 /= N;
                m111 /= N;
                
                double temp_kk = S116 / N - m112 * m22[kkP + kk] - 4.0 * S113 / N * m21[kkP + kk] -
                  2.0 * m111 * m32[kkP + kk] - m21[jjP + kk] * m41[iiP + kk] - m21[iiP + kk] * m41[jjP + kk] +
                  4.0 * m21[iiP + kk] * m21[kkP + kk] * m11[jjP + kk] + 4.0 * m21[jjP + kk] * m21[kkP + kk] * m11[iiP + kk] +
                  8.0 * m111 * m21[kkP + kk] * m11[kkP + kk];
                double temp_ii = S512 / N - m112 * m22[iiP + ii] - 4.0 * S212 / N * m21[iiP + ii] -
                  m21[jjP + kk] * m32[iiP + ii] - 2.0 * m111 * m41[kkP + ii] - m21[iiP + kk] * m41[jjP + ii] +
                  4.0 * m21[iiP + kk] * m21[iiP + ii] * m11[jjP + ii] + 8.0 * m111 * m21[iiP + ii] * m11[kkP + ii] +
                  4.0 * m21[jjP + kk] * m11[iiP + ii] * m21[iiP + ii];
                double temp_jj = S152 / N - m112 * m22[jjP + jj] - 4.0 * S122 / N * m21[jjP + jj] -
                  m21[iiP + kk] * m32[jjP + jj] - 2.0 * m111 * m41[kkP + jj] - m21[jjP + kk] * m41[iiP + jj] +
                  4.0 * m21[jjP + kk] * m21[jjP + jj] * m11[iiP + jj] + 8.0 * m111 * m21[jjP + jj] * m11[kkP + jj] +
                  4.0 * m21[iiP + kk] * m11[jjP + jj] * m21[jjP + jj];
                
                rCM4[0] += 3.0 * r6 * sqrt(r5) *
                  (2.0 * sqrt(sqrt(m22[iiP + ii] * m22[jjP + jj]) / m22[kkP + kk]) * temp_kk +
                  sqrt(m22[kkP + kk] * sqrt(m22[jjP + jj] / (m22[iiP + ii] * m22[iiP + ii] * m22[iiP + ii]))) * temp_ii +
                  sqrt(m22[kkP + kk] * sqrt(m22[iiP + ii] / (m22[jjP + jj] * m22[jjP + jj] * m22[jjP + jj]))) * temp_jj) / N;
              } else {
                // psi_ijkl
                double S5111 = 0.0;
                double S1511 = 0.0;
                double S1151 = 0.0;
                double S1115 = 0.0;
                double S2111 = 0.0;
                double S1211 = 0.0;
                double S1121 = 0.0;
                double S1112 = 0.0;
                double m1111 = 0.0;
                double m0111 = 0.0;
                double m1011 = 0.0;
                double m1101 = 0.0;
                double m1110 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S5111 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1511 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1151 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1115 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc2[llN + tt] * Xc2[llN + tt] * Xc[llN + tt];
                  S2111 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1211 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S1121 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc[llN + tt];
                  S1112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc2[llN + tt];
                  m1111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m0111 += Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m1011 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m1101 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[llN + tt];
                  m1110 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
                }
                m1111 /= N;
                m0111 /= N;
                m1011 /= N;
                m1101 /= N;
                m1110 /= N;
                
                double temp_ii = S5111 / N - m1111 / N * m22[iiP + ii] - 4.0 * S2111 / N * m21[iiP + ii] -
                  m0111 * m41[iiP + ii] - m1011 * m41[jjP + ii] - m1101 * m41[kkP + ii] -
                  m1110 * m41[llP + ii] + 4.0 * m1110 * m21[iiP + ii] * m11[llP + ii] +
                  4.0 * m1101 * m21[iiP + ii] * m11[kkP + ii] + 4.0 * m1011 * m21[iiP + ii] * m11[jjP + ii] +
                  4.0 * m0111 * m11[iiP + ii] * m21[iiP + ii];
                double temp_jj = S1511 / N - m1111 / N * m22[jjP + jj] - 4.0 * S1211 / N * m21[jjP + jj] -
                  m0111 * m41[iiP + jj] - m1011 * m41[jjP + jj] - m1101 * m41[kkP + jj] -
                  m1110 * m41[llP + jj] + 4.0 * m1110 * m21[jjP + jj] * m11[llP + jj] +
                  4.0 * m1101 * m21[jjP + jj] * m11[kkP + jj] + 4.0 * m1011 * m21[jjP + jj] * m11[jjP + jj] +
                  4.0 * m0111 * m11[iiP + jj] * m21[jjP + jj];
                double temp_kk = S1151 / N - m1111 / N * m22[kkP + kk] - 4.0 * S1121 / N * m21[kkP + kk] -
                  m0111 * m41[iiP + kk] - m1011 * m41[jjP + kk] - m1101 * m41[kkP + kk] -
                  m1110 * m41[llP + kk] + 4.0 * m1110 * m21[kkP + kk] * m11[llP + kk] +
                  4.0 * m1101 * m21[kkP + kk] * m11[kkP + kk] + 4.0 * m1011 * m21[kkP + kk] * m11[jjP + kk] +
                  4.0 * m0111 * m11[iiP + kk] * m21[kkP + kk];
                double temp_ll = S1115 / N - m1111 / N * m22[llP + ll] - 4.0 * S1112 / N * m21[llP + ll] -
                  m0111 * m41[iiP + ll] - m1011 * m41[jjP + ll] - m1101 * m41[kkP + ll] -
                  m1110 * m41[llP + ll] + 4.0 * m1110 * m21[llP + ll] * m11[llP + ll] +
                  4.0 * m1101 * m21[llP + ll] * m11[kkP + ll] + 4.0 * m1011 * m21[llP + ll] * m11[jjP + ll] +
                  4.0 * m0111 * m11[iiP + ll] * m21[llP + ll];
                
                rCM4[0] += 6.0 * r7 * r5 *
                  (sqrt(sqrt((m22[jjP + jj] * m22[kkP + kk] * m22[llP + ll]) / (m22[iiP + ii] * m22[iiP + ii] * m22[iiP + ii]))) * temp_ii +
                  sqrt(sqrt((m22[iiP + ii] * m22[kkP + kk] * m22[llP + ll]) / (m22[jjP + jj] * m22[jjP + jj] * m22[jjP + jj]))) * temp_jj +
                  sqrt(sqrt((m22[iiP + ii] * m22[jjP + jj] * m22[llP + ll]) / (m22[kkP + kk] * m22[kkP + kk] * m22[kkP + kk]))) * temp_kk +
                  sqrt(sqrt((m22[iiP + ii] * m22[jjP + jj] * m22[kkP + kk]) / (m22[llP + ll] * m22[llP + ll] * m22[llP + ll]))) * temp_ll) / N;
              }
            }
          }
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return CM4;
}

SEXP  CM4_1F(SEXP XXc, SEXP XXc2, SEXP ffcobs, SEXP ffvar, SEXP ffskew, SEXP ffkurt,
             SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm31, SEXP NN, SEXP PP){
  /*
   arguments
   XXc       : numeric vector with centered observations
   XXc2      : numeric vector with centered and squared observations
   ffcobs    : centered factor observations
   ffvar     : variance of the factor
   ffskew    : third order central moment of the factor
   ffkurt    : fourth order central moment of the factor
   mm11      : numeric vector of t(Xc) %*% Xc / NN
   mm21      : numeric vector of t(Xc^2) %*% Xc / NN
   mm22      : numeric vector of t(Xc^2) %*% Xc^2 / NN
   mm31      : numeric vector of t(Xc^3) %*% Xc / NN
   NN        : integer, number of observations
   PP        : integer, number of assets
   
   Written by Dries Cornilly
   */
  
  // // declare pointers for the vector
  double *Xc, *Xc2, *fcobs, *m11, *m21, *m22, *m31;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  Xc = REAL(XXc);
  Xc2 = REAL(XXc2);
  fcobs = REAL(ffcobs);
  m11 = REAL(mm11);
  m21 = REAL(mm21);
  m22 = REAL(mm22);
  m31 = REAL(mm31);
  double N = asReal(NN);
  int n = asInteger(NN);
  int P = asInteger(PP);
  double fvar = asReal(ffvar);
  double fskew = asReal(ffskew);
  double fkurt = asReal(ffkurt);
  
  // allocate and compute the covariance
  SEXP CM4 = PROTECT(allocVector(REALSXP, 1));
  double *rCM4 = REAL(CM4);
  rCM4[0] = 0.0;
  
  // compute some helper variables for later on
  double fvar2 = fvar * fvar;
  double fvar3 = fvar2 * fvar;
  double fvar4 = fvar2 * fvar2;
  double fvar5 = fvar2 * fvar3;
  double covXf[P];
  double X1f2[P];
  double X1f4[P];
  double X11f1[P * P];
  for (int ii = 0; ii < P; ii++) {
    int iiN = ii * n;
    covXf[ii] = 0.0;
    X1f2[ii] = 0.0;
    X1f4[ii] = 0.0;
    for (int tt = 0; tt < n; tt++) {
      covXf[ii] += Xc[iiN + tt] * fcobs[tt];
      X1f2[ii] += Xc[iiN + tt] * fcobs[tt] * fcobs[tt];
      X1f4[ii] += Xc[iiN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
    }
    covXf[ii] /= N;
    X1f2[ii] /= N;
    X1f4[ii] /= N;
    for (int jj = ii; jj < P; jj++) {
      int jjN = jj * n;
      double elem = 0.0;
      for (int tt = 0; tt < n; tt++) {
        elem += Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
      }
      elem /= N;
      X11f1[ii * P + jj] = elem;
      X11f1[jj * P + ii] = elem;
    }
  }
  
  // loop over the unique indices i <= j <= k <= l
  for (int ii = 0; ii < P; ii++) {
    int iiP = ii * P;
    int iiN = ii * n;
    for (int jj = ii; jj < P; jj++) {
      int jjP = jj * P;
      int jjN = jj * n;
      for (int kk = jj; kk < P; kk++) {
        int kkP = kk * P;
        int kkN = kk * n;
        for (int ll = kk; ll < P; ll++) {
          int llP = ll * P;
          int llN = ll * n;
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                double S8 = 0.0;
                double S5 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S8 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[iiN + tt];
                  S5 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt];
                }
                
                rCM4[0] += (S8 / N - m31[iiP + ii] * m31[iiP + ii] -
                  8.0 * S5 / N * m21[iiP + ii] +
                  16.0 * m11[iiP + ii] * m21[iiP + ii] * m21[iiP + ii]) / N;
              } else {
                // psi_iiil
                double S411 = 0.0;
                double S321 = 0.0;
                double S51 = 0.0;
                double S312 = 0.0;
                double S314 = 0.0;
                double S311 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S411 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[llN + tt] * fcobs[tt];
                  S321 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc2[llN + tt] * fcobs[tt];
                  S51 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  S312 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt];
                  S314 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S311 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt];
                }
                
                double temp_i0 = S411 / N - m31[llP + ii] * covXf[ii] - 3.0 * m21[llP + ii] * X11f1[iiP + ii] -
                  m21[iiP + ii] * X11f1[llP + ii];
                double temp_l0 = S321 / N - m31[llP + ii] * covXf[ll] - 3.0 * m21[llP + ii] * X11f1[llP + ii] -
                  m21[iiP + ii] * X11f1[llP + ll];
                double temp_ii = S51 / N - m31[llP + ii] * m11[iiP + ii] - 4.0 * m21[llP + ii] * m21[iiP + ii];
                double temp_var = S312 / N - m31[llP + ii] * fvar - 3.0 * m21[llP + ii] * X1f2[ii] -
                  m21[iiP + ii] * X1f2[ll];
                double temp_kurt = S314 / N - m31[llP + ii] * fkurt - 4.0 * S311 / N * fkurt +
                  4.0 * fskew * (3.0 * m21[llP + ii] * covXf[ii] + m21[iiP + ii] * covXf[ll]);
                
                rCM4[0] += 4.0 * ((3.0 * covXf[ii] * covXf[ii] * covXf[ll] * fkurt / fvar4 -
                  9.0 * covXf[ii] * covXf[ii] * covXf[ll] / fvar2 + 3.0 * covXf[ll] * m11[iiP + ii] / fvar) * temp_i0 +
                  (covXf[ii] * covXf[ii] * covXf[ii] * fkurt / fvar4 +
                  3.0 * covXf[ii] * (m11[iiP + ii] - covXf[ii] * covXf[ii] / fvar) / fvar) * temp_l0 +
                  3.0 * covXf[ii] * covXf[ll] / fvar * temp_ii +
                  (-4.0 * covXf[ii] * covXf[ii] * covXf[ii] * covXf[ll] * fkurt / fvar5 -
                  3.0 * covXf[ii] * covXf[ll] * m11[iiP + ii] / fvar2 +
                  6.0 * covXf[ii] * covXf[ii] * covXf[ii] * covXf[ll] / fvar3) * temp_var +
                  covXf[ii] * covXf[ii] * covXf[ii] * covXf[ll] * temp_kurt / fvar4) / N;
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                double S321 = 0.0;
                double S231 = 0.0;
                double S420 = 0.0;
                double S240 = 0.0;
                double S222 = 0.0;
                double S224 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S321 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc2[kkN + tt] * fcobs[tt];
                  S231 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc[kkN + tt] * fcobs[tt];
                  S420 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc2[kkN + tt];
                  S240 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc2[kkN + tt];
                  S222 += Xc2[iiN + tt] * Xc2[kkN + tt] * fcobs[tt] * fcobs[tt];
                  S224 += Xc2[iiN + tt] * Xc2[kkN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                }
                
                double temp_i0 = S321 / N - m22[kkP + ii] * covXf[ii] - 2.0 * m21[iiP + kk] * X11f1[iiP + ii] -
                  2.0 * m21[kkP + ii] * X11f1[kkP + ii];
                double temp_k0 = S231 / N - m22[iiP + kk] * covXf[kk] - 2.0 * m21[kkP + ii] * X11f1[kkP + kk] -
                  2.0 * m21[iiP + kk] * X11f1[iiP + kk];
                double temp_ii = S420 / N - m22[kkP + ii] * m11[iiP + ii] - 2.0 * m21[iiP + kk] * m21[iiP + ii] -
                  2.0 * m21[kkP + ii] * m21[kkP + ii];
                double temp_kk = S240 / N - m22[iiP + kk] * m11[kkP + kk] - 2.0 * m21[kkP + ii] * m21[kkP + kk] -
                  2.0 * m21[iiP + kk] * m21[iiP + kk];
                double temp_var = S222 / N - m22[kkP + ii] * fvar - 2.0 * m21[iiP + kk] * X1f2[ii] -
                  2.0 * m21[kkP + ii] * X1f2[kk];
                double temp_kurt = S224 / N - m22[kkP + ii] * fkurt - 2.0 * m21[iiP + kk] * X1f4[ii] -
                  2.0 * m21[kkP + ii] * X1f4[kk] + 8.0 * fskew * (m21[iiP + kk] * covXf[ii] + m21[kkP + ii] * covXf[kk]);
                
                rCM4[0] += 6.0 * ((2.0 * covXf[ii] * covXf[kk] * covXf[kk] * fkurt / fvar4 -
                  2.0 * covXf[ii] * covXf[kk] * covXf[kk] / fvar2) * temp_i0 +
                  (2.0 * covXf[ii] * covXf[ii] * covXf[kk] * fkurt / fvar4 -
                  2.0 * covXf[ii] * covXf[ii] * covXf[kk] / fvar2) * temp_k0 +
                  m11[kkP + kk] * temp_ii + m11[iiP + ii] * temp_kk +
                  (-4.0 * covXf[ii] * covXf[ii] * covXf[kk] * covXf[kk] * fkurt / fvar5 +
                  2.0 * covXf[ii] * covXf[ii] * covXf[kk] * covXf[kk] / fvar3) * temp_var +
                  covXf[ii] * covXf[ii] * covXf[kk] * covXf[kk] * temp_kurt / fvar4) / N;
              } else {
                // psi_iikl
                double S3111 = 0.0;
                double S2211 = 0.0;
                double S2121 = 0.0;
                double S4110 = 0.0;
                double S2112 = 0.0;
                double S2114 = 0.0;
                double S2111 = 0.0;
                double m2110 = 0.0;
                double m1110 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S3111 += Xc2[iiN + tt] * Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S2211 += Xc2[iiN + tt] * Xc2[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S2121 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc2[llN + tt] * fcobs[tt];
                  S4110 += Xc2[iiN + tt] * Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  S2112 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt];
                  S2114 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S2111 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  m2110 += Xc2[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m1110 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                }
                m2110 /= N;
                m1110 /= N;
                
                double temp_i0 = S3111 / N - m2110 * covXf[ii] - 2.0 * m1110 * X11f1[iiP + ii] -
                  m21[llP + ii] * X11f1[kkP + ii] - m21[kkP + ii] * X11f1[llP + ii];
                double temp_k0 = S2211 / N - m2110 * covXf[kk] - 2.0 * m1110 * X11f1[kkP + ii] -
                  m21[llP + ii] * X11f1[kkP + kk] - m21[kkP + ii] * X11f1[llP + kk];
                double temp_l0 = S2121 / N - m2110 * covXf[ll] - 2.0 * m1110 * X11f1[llP + ii] -
                  m21[kkP + ii] * X11f1[llP + ll] - m21[llP + ii] * X11f1[kkP + ll];
                double temp_ii = S4110 / N - m2110 * m11[iiP + ii] - 2.0 * m1110 * m21[iiP + ii] -
                  2.0 * m21[llP + ii] * m21[kkP + ii];
                double temp_var = S2112 / N - m2110 * fvar - 2.0 * m1110 * X1f2[ii] -
                  m21[llP + ii] * X1f2[kk] - m21[kkP + ii] * X1f2[ll];
                double temp_kurt = S2114 / N - m2110 * fkurt - 2.0 * m1110 * X1f4[ii] -
                  m21[llP + ii] * X1f4[kk] - m21[kkP + ii] * X1f4[ll] - 4.0 * S2111 / N * fskew +
                  4.0 * fskew * (2.0 * m1110 * covXf[ii] + m21[llP + ii] * covXf[kk] + m21[kkP + ii] * covXf[ll]);
                
                rCM4[0] += 12.0 * ((2.0 * covXf[ii] * covXf[kk] * covXf[ll] * fkurt / fvar4 -
                  2.0 * covXf[ii] * covXf[kk] * covXf[ll] / fvar2) * temp_i0 +
                  (covXf[ii] * covXf[ii] * covXf[ll] * fkurt / fvar4 +
                  covXf[ll] * (m11[iiP + ii] - covXf[ii] * covXf[ii] / fvar) / fvar) * temp_k0 +
                  (covXf[ii] * covXf[ii] * covXf[kk] * fkurt / fvar4 +
                  covXf[kk] * (m11[iiP + ii] - covXf[ii] * covXf[ii] / fvar) / fvar) * temp_l0 +
                  covXf[kk] * covXf[ll] / fvar * temp_ii +
                  (-4.0 * covXf[ii] * covXf[ii] * covXf[kk] * covXf[ll] * fkurt / fvar5 +
                  2.0 * covXf[kk] * covXf[ll] * covXf[ii] * covXf[ii] / fvar3 -
                  covXf[kk] * covXf[ll] * m11[iiP + ii] / fvar2) * temp_var +
                  covXf[ii] * covXf[ii] * covXf[kk] * covXf[ll] * temp_kurt / fvar4) / N;
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                double S411 = 0.0;
                double S321 = 0.0;
                double S51 = 0.0;
                double S312 = 0.0;
                double S314 = 0.0;
                double S311 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S411 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[iiN + tt] * fcobs[tt];
                  S321 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc2[iiN + tt] * fcobs[tt];
                  S51 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt];
                  S312 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt] * fcobs[tt] * fcobs[tt];
                  S314 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S311 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt] * fcobs[tt];
                }
                
                double temp_j0 = S411 / N - m31[iiP + jj] * covXf[jj] - 3.0 * m21[iiP + jj] * X11f1[jjP + jj] -
                  m21[jjP + jj] * X11f1[iiP + jj];
                double temp_i0 = S321 / N - m31[iiP + jj] * covXf[ii] - 3.0 * m21[iiP + jj] * X11f1[iiP + jj] -
                  m21[jjP + jj] * X11f1[iiP + ii];
                double temp_jj = S51 / N - m31[iiP + jj] * m11[jjP + jj] - 4.0 * m21[iiP + jj] * m21[jjP + jj];
                double temp_var = S312 / N - m31[iiP + jj] * fvar - 3.0 * m21[iiP + jj] * X1f2[jj] -
                  m21[jjP + jj] * X1f2[ii];
                double temp_kurt = S314 / N - m31[iiP + jj] * fkurt - 4.0 * S311 / N * fkurt +
                  4.0 * fskew * (3.0 * m21[iiP + jj] * covXf[jj] + m21[jjP + jj] * covXf[ii]);
                
                rCM4[0] += 4.0 * ((3.0 * covXf[jj] * covXf[jj] * covXf[ii] * fkurt / fvar4 -
                  9.0 * covXf[jj] * covXf[jj] * covXf[ii] / fvar2 + 3.0 * covXf[ii] * m11[jjP + jj] / fvar) * temp_j0 +
                  (covXf[jj] * covXf[jj] * covXf[jj] * fkurt / fvar4 +
                  3.0 * covXf[jj] * (m11[jjP + jj] - covXf[jj] * covXf[jj] / fvar) / fvar) * temp_i0 +
                  3.0 * covXf[jj] * covXf[ii] / fvar * temp_jj +
                  (-4.0 * covXf[jj] * covXf[jj] * covXf[jj] * covXf[ii] * fkurt / fvar5 -
                  3.0 * covXf[jj] * covXf[ii] * m11[jjP + jj] / fvar2 +
                  6.0 * covXf[jj] * covXf[jj] * covXf[jj] * covXf[ii] / fvar3) * temp_var +
                  covXf[jj] * covXf[jj] * covXf[jj] * covXf[ii] * temp_kurt / fvar4) / N;
              } else {
                // psi_ijjl
                double S3111 = 0.0;
                double S2211 = 0.0;
                double S2121 = 0.0;
                double S4110 = 0.0;
                double S2112 = 0.0;
                double S2114 = 0.0;
                double S2111 = 0.0;
                double m2110 = 0.0;
                double m1110 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S3111 += Xc2[jjN + tt] * Xc[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt];
                  S2211 += Xc2[jjN + tt] * Xc2[iiN + tt] * Xc[llN + tt] * fcobs[tt];
                  S2121 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc2[llN + tt] * fcobs[tt];
                  S4110 += Xc2[jjN + tt] * Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  S2112 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt];
                  S2114 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S2111 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt] * fcobs[tt];
                  m2110 += Xc2[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                  m1110 += Xc[jjN + tt] * Xc[iiN + tt] * Xc[llN + tt];
                }
                m2110 /= N;
                m1110 /= N;
                
                double temp_j0 = S3111 / N - m2110 * covXf[jj] - 2.0 * m1110 * X11f1[jjP + jj] -
                  m21[llP + jj] * X11f1[iiP + jj] - m21[iiP + jj] * X11f1[llP + jj];
                double temp_i0 = S2211 / N - m2110 * covXf[ii] - 2.0 * m1110 * X11f1[iiP + jj] -
                  m21[llP + jj] * X11f1[iiP + ii] - m21[iiP + jj] * X11f1[llP + ii];
                double temp_l0 = S2121 / N - m2110 * covXf[ll] - 2.0 * m1110 * X11f1[llP + jj] -
                  m21[iiP + jj] * X11f1[llP + ll] - m21[llP + jj] * X11f1[iiP + ll];
                double temp_jj = S4110 / N - m2110 * m11[jjP + jj] - 2.0 * m1110 * m21[jjP + jj] -
                  2.0 * m21[llP + jj] * m21[iiP + jj];
                double temp_var = S2112 / N - m2110 * fvar - 2.0 * m1110 * X1f2[jj] -
                  m21[llP + jj] * X1f2[ii] - m21[iiP + jj] * X1f2[ll];
                double temp_kurt = S2114 / N - m2110 * fkurt - 2.0 * m1110 * X1f4[jj] -
                  m21[llP + jj] * X1f4[ii] - m21[iiP + jj] * X1f4[ll] - 4.0 * S2111 / N * fskew +
                  4.0 * fskew * (2.0 * m1110 * covXf[jj] + m21[llP + jj] * covXf[ii] + m21[iiP + jj] * covXf[ll]);
                
                rCM4[0] += 12.0 * ((2.0 * covXf[jj] * covXf[ii] * covXf[ll] * fkurt / fvar4 -
                  2.0 * covXf[jj] * covXf[ii] * covXf[ll] / fvar2) * temp_j0 +
                  (covXf[jj] * covXf[jj] * covXf[ll] * fkurt / fvar4 +
                  covXf[ll] * (m11[jjP + jj] - covXf[jj] * covXf[jj] / fvar) / fvar) * temp_i0 +
                  (covXf[jj] * covXf[jj] * covXf[ii] * fkurt / fvar4 +
                  covXf[ii] * (m11[jjP + jj] - covXf[jj] * covXf[jj] / fvar) / fvar) * temp_l0 +
                  covXf[ii] * covXf[ll] / fvar * temp_jj +
                  (-4.0 * covXf[jj] * covXf[jj] * covXf[ii] * covXf[ll] * fkurt / fvar5 +
                  2.0 * covXf[ii] * covXf[ll] * covXf[jj] * covXf[jj] / fvar3 -
                  covXf[ii] * covXf[ll] * m11[jjP + jj] / fvar2) * temp_var +
                  covXf[jj] * covXf[jj] * covXf[ii] * covXf[ll] * temp_kurt / fvar4) / N;
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                double S3111 = 0.0;
                double S2211 = 0.0;
                double S2121 = 0.0;
                double S4110 = 0.0;
                double S2112 = 0.0;
                double S2114 = 0.0;
                double S2111 = 0.0;
                double m2110 = 0.0;
                double m1110 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S3111 += Xc2[kkN + tt] * Xc[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
                  S2211 += Xc2[kkN + tt] * Xc2[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
                  S2121 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc2[jjN + tt] * fcobs[tt];
                  S4110 += Xc2[kkN + tt] * Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                  S2112 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt] * fcobs[tt];
                  S2114 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S2111 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt] * fcobs[tt];
                  m2110 += Xc2[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                  m1110 += Xc[kkN + tt] * Xc[iiN + tt] * Xc[jjN + tt];
                }
                m2110 /= N;
                m1110 /= N;
                
                double temp_k0 = S3111 / N - m2110 * covXf[kk] - 2.0 * m1110 * X11f1[kkP + kk] -
                  m21[jjP + kk] * X11f1[iiP + kk] - m21[iiP + kk] * X11f1[jjP + kk];
                double temp_i0 = S2211 / N - m2110 * covXf[ii] - 2.0 * m1110 * X11f1[iiP + kk] -
                  m21[jjP + kk] * X11f1[iiP + ii] - m21[iiP + kk] * X11f1[jjP + ii];
                double temp_j0 = S2121 / N - m2110 * covXf[jj] - 2.0 * m1110 * X11f1[jjP + kk] -
                  m21[iiP + kk] * X11f1[jjP + jj] - m21[jjP + kk] * X11f1[iiP + jj];
                double temp_kk = S4110 / N - m2110 * m11[kkP + kk] - 2.0 * m1110 * m21[kkP + kk] -
                  2.0 * m21[jjP + kk] * m21[iiP + kk];
                double temp_var = S2112 / N - m2110 * fvar - 2.0 * m1110 * X1f2[kk] -
                  m21[jjP + kk] * X1f2[ii] - m21[iiP + kk] * X1f2[jj];
                double temp_kurt = S2114 / N - m2110 * fkurt - 2.0 * m1110 * X1f4[kk] -
                  m21[jjP + kk] * X1f4[ii] - m21[iiP + kk] * X1f4[jj] - 4.0 * S2111 / N * fskew +
                  4.0 * fskew * (2.0 * m1110 * covXf[kk] + m21[jjP + kk] * covXf[ii] + m21[iiP + kk] * covXf[jj]);
                
                rCM4[0] += 12.0 * ((2.0 * covXf[kk] * covXf[ii] * covXf[jj] * fkurt / fvar4 -
                  2.0 * covXf[kk] * covXf[ii] * covXf[jj] / fvar2) * temp_k0 +
                  (covXf[kk] * covXf[kk] * covXf[jj] * fkurt / fvar4 +
                  covXf[jj] * (m11[kkP + kk] - covXf[kk] * covXf[kk] / fvar) / fvar) * temp_i0 +
                  (covXf[kk] * covXf[kk] * covXf[ii] * fkurt / fvar4 +
                  covXf[ii] * (m11[kkP + kk] - covXf[kk] * covXf[kk] / fvar) / fvar) * temp_j0 +
                  covXf[ii] * covXf[jj] / fvar * temp_kk +
                  (-4.0 * covXf[kk] * covXf[kk] * covXf[ii] * covXf[jj] * fkurt / fvar5 +
                  2.0 * covXf[ii] * covXf[jj] * covXf[kk] * covXf[kk] / fvar3 -
                  covXf[ii] * covXf[jj] * m11[kkP + kk] / fvar2) * temp_var +
                  covXf[kk] * covXf[kk] * covXf[ii] * covXf[jj] * temp_kurt / fvar4) / N;
              } else {
                // psi_ijkl
                double S11114 = 0.0;
                double S11112 = 0.0;
                double S11111 = 0.0;
                double S21111 = 0.0;
                double S12111 = 0.0;
                double S11211 = 0.0;
                double S11121 = 0.0;
                double m1111 = 0.0;
                double m01110 = 0.0;
                double m10110 = 0.0;
                double m11010 = 0.0;
                double m11100 = 0.0;
                for (int tt = 0; tt < n; tt++) {
                  S11114 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] *
                    fcobs[tt] * fcobs[tt] * fcobs[tt] * fcobs[tt];
                  S11112 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt] * fcobs[tt];
                  S11111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S21111 += Xc2[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S12111 += Xc[iiN + tt] * Xc2[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S11211 += Xc[iiN + tt] * Xc[jjN + tt] * Xc2[kkN + tt] * Xc[llN + tt] * fcobs[tt];
                  S11121 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc2[llN + tt] * fcobs[tt];
                  m1111 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m01110 += Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m10110 += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
                  m11010 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[llN + tt];
                  m11100 += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
                }
                m1111 /= N;
                m01110 /= N;
                m10110 /= N;
                m11010 /= N;
                m11100 /= N;
                
                double temp_ii = S21111 / N - m1111 * covXf[ii] - m01110 * X11f1[iiP + ii] - m10110 * X11f1[jjP + ii] -
                  m11010 * X11f1[kkP + ii] - m11100 * X11f1[llP + ii];
                double temp_jj = S12111 / N - m1111 * covXf[jj] - m01110 * X11f1[iiP + jj] - m10110 * X11f1[jjP + jj] -
                  m11010 * X11f1[kkP + jj] - m11100 * X11f1[llP + jj];
                double temp_kk = S11211 / N - m1111 * covXf[kk] - m01110 * X11f1[iiP + kk] - m10110 * X11f1[jjP + kk] -
                  m11010 * X11f1[kkP + kk] - m11100 * X11f1[llP + kk];
                double temp_ll = S11121 / N - m1111 * covXf[ll] - m01110 * X11f1[iiP + ll] - m10110 * X11f1[jjP + ll] -
                  m11010 * X11f1[kkP + ll] - m11100 * X11f1[llP + ll];
                double temp_kurt = S11114 / N - m1111 * fkurt - m01110 * X1f4[ii] - m10110 * X1f4[jj] -
                  m11010 * X1f4[kk] - m11100 * X1f4[ll] - 4.0 * S11111 / N * fskew +
                  4.0 * fskew * (m01110 * covXf[ii] + m10110 * covXf[jj] + m11010 * covXf[kk] + m11100 * covXf[ll]);
                double temp_var = S11112 / N - m1111 * fvar - m01110 * X1f2[ii] - m10110 * X1f2[jj] -
                  m11010 * X1f2[kk] - m11100 * X1f2[ll];
                
                rCM4[0] += 24.0 * ((covXf[jj] * covXf[kk] * covXf[ll] * temp_ii +
                  covXf[ii] * covXf[kk] * covXf[ll] * temp_jj +
                  covXf[ii] * covXf[jj] * covXf[ll] * temp_kk +
                  covXf[ii] * covXf[jj] * covXf[kk] * temp_ll) * fkurt +
                  covXf[ii] * covXf[jj] * covXf[kk] * covXf[ll] * temp_kurt -
                  4.0 * covXf[ii] * covXf[jj] * covXf[kk] * covXf[ll] * fkurt * temp_var / fvar) / (N * fvar4);
              }
            }
          }
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii
  
  UNPROTECT(1);
  return CM4;
}
