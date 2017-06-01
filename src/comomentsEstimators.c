
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

SEXP  VM3(SEXP XXc, SEXP XXc2,
          SEXP mm11, SEXP mm21, SEXP mm22, SEXP mm31,
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

SEXP  CM3_1F(SEXP XXc, SEXP XXc2, SEXP ffcobs,
             SEXP ffvar, SEXP ffskew,
             SEXP mm11, SEXP mm21, SEXP mm22,
             SEXP mm42, SEXP NN, SEXP PP){
  /*
  arguments
  XXc       : numeric vector with centered observations
  XXc2      : numeric vector with centered and squared observations
  ccovXf    : numeric vector with covariances between the assets and the factor
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
            rCM3vec[0] += (m42[iiP + ii] - m21[iiP + ii] * m21[iiP + ii] - 6.0 * m22[iiP + ii] * m11[iiP + ii] +
              9.0 * m11[iiP + ii] * m11[iiP + ii] * m11[iiP + ii]);
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