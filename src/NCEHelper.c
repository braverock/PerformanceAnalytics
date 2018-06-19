
#include <R.h>
#include <Rinternals.h>


// // //
// // Gradient of the model moment with respect to the model parameters
// // //

SEXP  mod2grad(SEXP PP, SEXP KK, SEXP BB, SEXP BBinclude, SEXP eepsvarinclude){
  /*
   arguments
   PP        : integer, number of dimensions
   KK        : integer, dimension of the latent factors
   BB        : numeric vector with (vectorized) B matrix
   BBinclude : integer, optimize over B or not
   eepsvarinclude : integer, optimize over epsvar or not
   
   computes the gradient of the multi-factor model covariances with respect to its parameters
   
   Written by Dries Cornilly
   */
  
  // declare pointers for the vectors
  double *B;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  B = REAL(BB);
  int P = asInteger(PP);
  int K = asInteger(KK);
  int Binclude = asInteger(BBinclude);
  int epsvarinclude = asInteger(eepsvarinclude);
  
  // allocate and initialize the gradient
  int ncov = P * (P + 1) / 2;
  int nparam = P * K * Binclude + P * epsvarinclude;
  SEXP grad = PROTECT(allocMatrix(REALSXP, nparam, ncov));
  double *rgrad = REAL(grad);
  for (int ii = 0; ii < ncov * nparam; ii++) {
    rgrad[ii] = 0.0;
  }
  
  // derivative with respect to B
  int iter = 0;
  if (Binclude) {
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int qq = 0; qq < K; qq++) {
          rgrad[iter * nparam + qq * P + ii] += B[qq * P + jj];
          rgrad[iter * nparam + qq * P + jj] += B[qq * P + ii];
        }
        iter++;
      }
    }
  }
  
  // derivative with respect to epsvar
  iter = 0;
  if (epsvarinclude > 0) {
    int PKB = P * K * Binclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        if (ii == jj) rgrad[iter * nparam + PKB + ii] += 1.0;
        iter++;
      }
    }
  }
  
  UNPROTECT(1);
  return grad;
}


SEXP  mod3grad(SEXP PP, SEXP KK, SEXP BB, SEXP BBinclude, SEXP eepsvarinclude,
               SEXP ffskew, SEXP ffskewinclude, SEXP eepsskewinclude){
  /*
   arguments
   PP        : integer, number of dimensions
   KK        : integer, dimension of the latent factors
   BB        : numeric vector with (vectorized) B matrix
   BBinclude : integer, optimize over B or not
   eepsvarinclude : integer, optimize over epsvar or not
   ffskew    : numeric vector with skewness of the factors
   ffskewinclude : integer, optimize over fskew or not
   eepsskewinclude : integer, optimize over epsskew or not
   
   computes the gradient of the multi-factor model coskewness with respect to its parameters
   
   Written by Dries Cornilly
   */
  
  // declare pointers for the vectors
  double *B, *fskew;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  B = REAL(BB);
  fskew = REAL(ffskew);
  int P = asInteger(PP);
  int K = asInteger(KK);
  int Binclude = asInteger(BBinclude);
  int epsvarinclude = asInteger(eepsvarinclude);
  int fskewinclude = asInteger(ffskewinclude);
  int epsskewinclude = asInteger(eepsskewinclude);
  
  // allocate and initialize the gradient
  int ncosk = P * (P + 1) * (P + 2) / 6;
  int nparam = P * K * Binclude + P * epsvarinclude + K * fskewinclude + P * epsskewinclude;
  SEXP grad = PROTECT(allocMatrix(REALSXP, nparam, ncosk));
  double *rgrad = REAL(grad);
  for (int ii = 0; ii < ncosk * nparam; ii++) {
    rgrad[ii] = 0.0;
  }
  
  // derivative with respect to B
  int iter = 0;
  if (Binclude) {
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int qq = 0; qq < K; qq++) {
            rgrad[iter * nparam + qq * P + ii] += B[qq * P + jj] * B[qq * P + kk] * fskew[qq];
            rgrad[iter * nparam + qq * P + jj] += B[qq * P + ii] * B[qq * P + kk] * fskew[qq];
            rgrad[iter * nparam + qq * P + kk] += B[qq * P + ii] * B[qq * P + jj] * fskew[qq];
          }
          iter++;
        }
      }
    }
  }
  
  // derivative with respect to fskew
  iter = 0;
  if (fskewinclude) {
    int PKB = P * K * Binclude + P * epsvarinclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int qq = 0; qq < K; qq++) {
            rgrad[iter * nparam + PKB + qq] += B[qq * P + ii] * B[qq * P + jj] * B[qq * P + kk];
          }
          iter++;
        }
      }
    }
  }
  
  // derivative with respect to epsskew
  iter = 0;
  if (epsskewinclude > 0) {
    int PKB = P * K * Binclude + P * epsvarinclude + K * fskewinclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          if ((ii == jj) & (jj == kk)) rgrad[iter * nparam + PKB + ii] += 1.0;
          iter++;
        }
      }
    }
  }
  
  UNPROTECT(1);
  return grad;
}


SEXP  mod4grad(SEXP PP, SEXP KK, SEXP BB, SEXP BBinclude, SEXP eepsvarinclude,
               SEXP eepsvar, SEXP ffskewinclude, SEXP eepsskewinclude,
               SEXP ffkurt, SEXP ffkurtinclude, SEXP eepskurt, SEXP eepskurtinclude){
  /*
   arguments
   PP        : integer, number of dimensions
   KK        : integer, dimension of the latent factors
   BB        : numeric vector with (vectorized) B matrix
   BBinclude : integer, optimize over B or not
   eepsvarinclude : integer, optimize over epsvar or not
   eepsvar   : numeric vector with variances of the idiosyncratic term
   ffskewinclude : integer, optimize over fskew or not
   eepsskewinclude : integer, optimize over epsskew or not
   ffkurt    : numeric vector with kurtosis of the factors
   ffkurtinclude : integer, optimize over fkurt or not
   eepskurt  : numeric vector with kurtosis of the idiosyncratic term
   eepskurtinclude : integer, optimize over epskurt or not
   
   computes the gradient of the multi-factor model cokurtosis with respect to its parameters
   
   Written by Dries Cornilly
   */
  
  // declare pointers for the vectors
  double *B, *epsvar, *fkurt, *epskurt;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  B = REAL(BB);
  epsvar = REAL(eepsvar);
  fkurt = REAL(ffkurt);
  epskurt = REAL(eepskurt);
  int P = asInteger(PP);
  int K = asInteger(KK);
  int Binclude = asInteger(BBinclude);
  int epsvarinclude = asInteger(eepsvarinclude);
  int fskewinclude = asInteger(ffskewinclude);
  int epsskewinclude = asInteger(eepsskewinclude);
  int fkurtinclude = asInteger(ffkurtinclude);
  int epskurtinclude = asInteger(eepskurtinclude);
  
  // allocate and initialize the gradient
  int ncokurt = P * (P + 1) * (P + 2) * (P + 3) / 24;
  int nparam = P * K * Binclude + P * epsvarinclude + K * fskewinclude +
    P * epsskewinclude + K * fkurtinclude + P * epskurtinclude;
  SEXP grad = PROTECT(allocMatrix(REALSXP, nparam, ncokurt));
  double *rgrad = REAL(grad);
  for (int ii = 0; ii < ncokurt * nparam; ii++) {
    rgrad[ii] = 0.0;
  }
  
  // derivative with respect to B
  int iter = 0;
  if (Binclude) {
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int ll = kk; ll < P; ll++) {
            for (int qq = 0; qq < K; qq++) {
              rgrad[iter * nparam + qq * P + ii] += B[qq * P + jj] * B[qq * P + kk] * B[qq * P + ll] * fkurt[qq];
              rgrad[iter * nparam + qq * P + jj] += B[qq * P + ii] * B[qq * P + kk] * B[qq * P + ll] * fkurt[qq];
              rgrad[iter * nparam + qq * P + kk] += B[qq * P + ii] * B[qq * P + jj] * B[qq * P + ll] * fkurt[qq];
              rgrad[iter * nparam + qq * P + ll] += B[qq * P + ii] * B[qq * P + jj] * B[qq * P + kk] * fkurt[qq];
              
              for (int rr = 0; rr < qq; rr++) {
                rgrad[iter * nparam + qq * P + ii] += B[qq * P + jj] * B[rr * P + kk] * B[rr * P + ll] +
                  B[rr * P + jj] * B[qq * P + kk] * B[rr * P + ll] +
                  B[rr * P + jj] * B[rr * P + kk] * B[qq * P + ll];
                
                rgrad[iter * nparam + qq * P + jj] += B[qq * P + ii] * B[rr * P + kk] * B[rr * P + ll] +
                  B[rr * P + ii] * B[qq * P + kk] * B[rr * P + ll] +
                  B[rr * P + ii] * B[rr * P + kk] * B[qq * P + ll];
                
                rgrad[iter * nparam + qq * P + kk] += B[qq * P + ii] * B[rr * P + jj] * B[rr * P + ll] +
                  B[rr * P + ii] * B[qq * P + jj] * B[rr * P + ll] +
                  B[rr * P + ii] * B[rr * P + jj] * B[qq * P + ll];
                
                rgrad[iter * nparam + qq * P + ll] += B[qq * P + ii] * B[rr * P + jj] * B[rr * P + kk] +
                  B[rr * P + ii] * B[qq * P + jj] * B[rr * P + kk] +
                  B[rr * P + ii] * B[rr * P + jj] * B[qq * P + kk];
                
                rgrad[iter * nparam + rr * P + ii] += B[qq * P + jj] * B[qq * P + kk] * B[rr * P + ll] +
                  B[qq * P + jj] * B[rr * P + kk] * B[qq * P + ll] +
                  B[rr * P + jj] * B[qq * P + kk] * B[qq * P + ll];
                
                rgrad[iter * nparam + rr * P + jj] += B[qq * P + ii] * B[qq * P + kk] * B[rr * P + ll] +
                  B[qq * P + ii] * B[rr * P + kk] * B[qq * P + ll] +
                  B[rr * P + ii] * B[qq * P + kk] * B[qq * P + ll];
                
                rgrad[iter * nparam + rr * P + kk] += B[qq * P + ii] * B[qq * P + jj] * B[rr * P + ll] +
                  B[qq * P + ii] * B[rr * P + jj] * B[qq * P + ll] +
                  B[rr * P + ii] * B[qq * P + jj] * B[qq * P + ll];
                
                rgrad[iter * nparam + rr * P + ll] += B[qq * P + ii] * B[qq * P + jj] * B[rr * P + kk] +
                  B[qq * P + ii] * B[rr * P + jj] * B[qq * P + kk] +
                  B[rr * P + ii] * B[qq * P + jj] * B[qq * P + kk];
              }
            }
            if (ii == jj) {
              if (jj == kk) {
                if (kk == ll) {
                  // iiii
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += 12.0 * B[qq * P + ii] * epsvar[ii];
                  }
                } else {
                  // iiil
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += 3.0 * B[qq * P + ll] * epsvar[ii];
                    rgrad[iter * nparam + qq * P + ll] += 3.0 * B[qq * P + ii] * epsvar[ii];
                  }
                }
              } else {
                if (kk == ll) {
                  // iikk
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += 2.0 * B[qq * P + ii] * epsvar[kk];
                    rgrad[iter * nparam + qq * P + kk] += 2.0 * B[qq * P + kk] * epsvar[ii];
                  }
                } else {
                  // iikl
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + kk] += B[qq * P + ll] * epsvar[ii];
                    rgrad[iter * nparam + qq * P + ll] += B[qq * P + kk] * epsvar[ii];
                  }
                }
              }
            } else {
              if (jj == kk) {
                if (kk == ll) {
                  // ijjj
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += 3.0 * B[qq * P + jj] * epsvar[jj];
                    rgrad[iter * nparam + qq * P + jj] += 3.0 * B[qq * P + ii] * epsvar[jj];
                  }
                } else {
                  // ijjl
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += B[qq * P + ll] * epsvar[jj];
                    rgrad[iter * nparam + qq * P + ll] += B[qq * P + ii] * epsvar[jj];
                  }
                }
              } else {
                if (kk == ll) {
                  // ijkk
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + qq * P + ii] += B[qq * P + jj] * epsvar[kk];
                    rgrad[iter * nparam + qq * P + jj] += B[qq * P + ii] * epsvar[kk];
                  }
                }
              }
            }
            iter++;
          }
        }
      }
    }
  }
  
  // derivative with respect to epsvar
  iter = 0;
  if (epsvarinclude) {
    int PKB = P * K * Binclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int ll = kk; ll < P; ll++) {
            if (ii == jj) {
              if (jj == kk) {
                if (kk == ll) {
                  // iiii
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + ii] += 6.0 * B[qq * P + ii] * B[qq * P + ii];
                  }
                } else {
                  // iiil
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + ii] += 3.0 * B[qq * P + ii] * B[qq * P + ll];
                  }
                }
              } else {
                if (kk == ll) {
                  // iikk
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + ii] += B[qq * P + kk] * B[qq * P + kk];
                    rgrad[iter * nparam + PKB + kk] += B[qq * P + ii] * B[qq * P + ii];
                  }
                  rgrad[iter * nparam + PKB + ii] += epsvar[kk];
                  rgrad[iter * nparam + PKB + kk] += epsvar[ii];
                } else {
                  // iikl
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + ii] += B[qq * P + kk] * B[qq * P + ll];
                  }
                }
              }
            } else {
              if (jj == kk) {
                if (kk == ll) {
                  // ijjj
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + jj] += 3.0 * B[qq * P + ii] * B[qq * P + jj];
                  }
                } else {
                  // ijjl
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + jj] += B[qq * P + ii] * B[qq * P + ll];
                  }
                }
              } else {
                if (kk == ll) {
                  // ijkk
                  for (int qq = 0; qq < K; qq++) {
                    rgrad[iter * nparam + PKB + kk] += B[qq * P + ii] * B[qq * P + jj];
                  }
                }
              }
            }
            iter++;
          }
        }
      }
    }
  }
  
  // derivative with respect to fkurt
  iter = 0;
  if (fkurtinclude > 0) {
    int PKB = P * K * Binclude + P * epsvarinclude + K * fskewinclude + P * epsskewinclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int ll = kk; ll < P; ll++) {
            for (int qq = 0; qq < K; qq++) {
              rgrad[iter * nparam + PKB + qq] += B[qq * P + ii] * B[qq * P + jj] * B[qq * P + kk] * B[qq * P + ll];
            }
            iter++;
          }
        }
      }
    }
  }
  
  // derivative with respect to epskurt
  iter = 0;
  if (epskurtinclude > 0) {
    int PKB = P * K * Binclude + P * epsvarinclude + K * fskewinclude +
      P * epsskewinclude + K * fkurtinclude;
    for (int ii = 0; ii < P; ii++) {
      for (int jj = ii; jj < P; jj++) {
        for (int kk = jj; kk < P; kk++) {
          for (int ll = kk; ll < P; ll++) {
            if ((ii == jj) & ((jj == kk) & (kk == ll))) rgrad[iter * nparam + PKB + ii] += 1.0;
            iter++;
          }
        }
      }
    }
  }
  
  UNPROTECT(1);
  return grad;
}


SEXP  NCEAcov(SEXP mm11, SEXP PP, SEXP NN, SEXP XXc, 
              SEXP iincludem2, SEXP iincludem3, SEXP iincludem4) {
  
  /*
   arguments
   mm11      : numeric vector with all covariance elements
   PP        : integer, number of dimensions
   NN        : integer, number of observations
   XXc       : numeric vector with the centered observations (vectorized matrix)
   iincludem2 : integer, optimize over M2 or not
   iincludem3 : integer, optimize over M3 or not
   iincludem4 : integer, optimize over M4 or not
   
   computes asymptotic covariance matrix of the sample moments
   
   Written by Dries Cornilly
   */
  
  // declare pointers for the vectors
  double *m11, *Xc;
  
  // use REAL() to access the C array inside the numeric vector passed in from R
  m11 = REAL(mm11);
  Xc = REAL(XXc);
  int P = asInteger(PP);
  int N = asInteger(NN);
  int includem2 = asInteger(iincludem2);
  int includem3 = asInteger(iincludem3);
  int includem4 = asInteger(iincludem4);
  double n = asReal(NN);
  
  // allocate and initialize the gradient
  int ncov = P * (P + 1) / 2;
  int ncosk = P * (P + 1) * (P + 2) / 6;
  int ncokurt = P * (P + 1) * (P + 2) * (P + 3) / 24;
  int nparam = ncov * includem2 + ncosk * includem3 + ncokurt * includem4;
  SEXP mobs = PROTECT(allocMatrix(REALSXP, N, nparam));
  double *rmobs = REAL(mobs);
  
  int iter = 0;
  // generalized covariance observations
  if (includem2) {
    for (int ii = 0; ii < P; ii++) {
      int iiN = ii * N;
      for (int jj = ii; jj < P; jj++) {
        int jjN = jj * N;
        int iterN = iter * N;
        for (int tt = 0; tt < N; tt++) {
          rmobs[iterN + tt] = Xc[iiN + tt] * Xc[jjN + tt];
        }
        iter++;
      }
    }
  }
  
  // generalized coskewness observations
  if (includem3) {
    for (int ii = 0; ii < P; ii++) {
      int iiN = ii * N;
      int iiP = ii * P;
      for (int jj = ii; jj < P; jj++) {
        int jjN = jj * N;
        int jjP = jj * P;
        for (int kk = jj; kk < P; kk++) {
          int kkN = kk * N;
          int iterN = iter * N;
          for (int tt = 0; tt < N; tt++) {
            rmobs[iterN + tt] = Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] - Xc[iiN + tt] * m11[jjP + kk] -
              Xc[jjN + tt] * m11[iiP + kk] - Xc[kkN + tt] * m11[iiP + jj];
          }
          iter++;
        }
      }
    }
  }
  
  // generalized cokurtosis observations
  if (includem4) {
    for (int ii = 0; ii < P; ii++) {
      int iiN = ii * N;
      for (int jj = ii; jj < P; jj++) {
        int jjN = jj * N;
        for (int kk = jj; kk < P; kk++) {
          int kkN = kk * N;
          for (int ll = kk; ll < P; ll++) {
            int llN = ll * N;
            int iterN = iter * N;
            double phi_jkl = 0.0;
            double phi_ikl = 0.0;
            double phi_ijl = 0.0;
            double phi_ijk = 0.0;
            for (int tt = 0; tt < P; tt++) {
              phi_jkl += Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt];
              phi_ikl += Xc[iiN + tt] * Xc[kkN + tt] * Xc[llN + tt];
              phi_ijl += Xc[iiN + tt] * Xc[jjN + tt] * Xc[llN + tt];
              phi_ijk += Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt];
            }
            phi_jkl /= n;
            phi_ikl /= n;
            phi_ijl /= n;
            phi_ijk /= n;
            for (int tt = 0; tt < N; tt++) {
              rmobs[iterN + tt] = Xc[iiN + tt] * Xc[jjN + tt] * Xc[kkN + tt] * Xc[llN + tt] -
                Xc[iiN + tt] * phi_jkl - Xc[jjN + tt] * phi_ikl - Xc[kkN + tt] * phi_ijl - Xc[llN + tt] * phi_ijk;
            }
            iter++;
          }
        }
      }
    }
  }
  
  UNPROTECT(1);
  return mobs;
}



