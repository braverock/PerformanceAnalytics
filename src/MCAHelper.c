
#include <R.h>
#include <Rinternals.h>


// // //
// // Compute M3 %*% (U %x% U) or M4 %*% (U %x% U %x% U)
// // //

SEXP  M3HOOIiterator(SEXP XX, SEXP UU, SEXP PP, SEXP KK){
  /*
   arguments
   XX        : numeric vector with unique elements of a coskewness matrix
   UU        : numeric vector (vectorized matrix) with the loadings matrix
   PP        : integer, dimension of XX
   KK        : integer, number of columns of UU

   computes Phi %*% (U %x% U)

   Written by Dries Cornilly
   */

  // declare pointers for the vectors
  double *X, *U;

  // use REAL() to access the C array inside the numeric vector passed in from R
  X = REAL(XX);
  U = REAL(UU);
  int P = asInteger(PP);
  int K = asInteger(KK);

  // allocate and compute the result matrix
  SEXP Z3 = PROTECT(allocMatrix(REALSXP, P, K * K));
  double *rZ3 = REAL(Z3);
  for (int ii = 0; ii < P * K * K; ii++) {
    rZ3[ii] = 0.0;
  }

  // loop over the coskenwess elements
  int iter = 0;
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        if (ii == jj) {
          if (jj == kk) {
            // phi_iii
            for (int uu = 0; uu < K; uu++) {
              for (int vv = 0; vv < K; vv++) {
                rZ3[(vv * K + uu) * P + ii] += X[iter] * U[uu * P + ii] * U[vv * P + ii];
              }
            }
          } else {
            // phi_iik
            for (int uu = 0; uu < K; uu++) {
              int uuP = uu * P;
              for (int vv = 0; vv < K; vv++) {
                int vvP = vv * P;
                rZ3[(vv * K + uu) * P + ii] += X[iter] * (U[uuP + ii] * U[vvP + kk] + U[uuP + kk] * U[vvP + ii]);
                rZ3[(vv * K + uu) * P + kk] += X[iter] * U[uuP + ii] * U[vvP + ii];
              }
            }
          }
        } else {
          if (jj == kk) {
            // phi_ijj
            for (int uu = 0; uu < K; uu++) {
              int uuP = uu * P;
              for (int vv = 0; vv < K; vv++) {
                int vvP = vv * P;
                rZ3[(vv * K + uu) * P + ii] += X[iter] * U[uuP + jj] * U[vvP + jj];
                rZ3[(vv * K + uu) * P + jj] += X[iter] * (U[uuP + ii] * U[vvP + jj] + U[uuP + jj] * U[vvP + ii]);
              }
            }
          } else {
            //phi_ijk
            for (int uu = 0; uu < K; uu++) {
              int uuP = uu * P;
              for (int vv = 0; vv < K; vv++) {
                int vvP = vv * P;
                int vvKuuP = (vv * K + uu) * P;
                rZ3[vvKuuP + ii] += X[iter] * (U[uuP + jj] * U[vvP + kk] + U[uuP + kk] * U[vvP + jj]);
                rZ3[vvKuuP + jj] += X[iter] * (U[uuP + ii] * U[vvP + kk] + U[uuP + kk] * U[vvP + ii]);
                rZ3[vvKuuP + kk] += X[iter] * (U[uuP + ii] * U[vvP + jj] + U[uuP + jj] * U[vvP + ii]);
              }
            }
          }
        }
        iter++;
      } // loop kk
    } // loop jj
  } // loop ii

  UNPROTECT(1);
  return Z3;
}


SEXP  M4HOOIiterator(SEXP XX, SEXP UU, SEXP PP, SEXP KK){
  /*
   arguments
   XX        : numeric vector with unique elements of a cokurtosis matrix
   UU        : numeric vector (vectorized matrix) with the loadings matrix
   PP        : integer, dimension of XX
   KK        : integer, number of columns of UU

   computes Psi %*% (U %x% U %x% U)

   Written by Dries Cornilly
   */

  // declare pointers for the vectors
  double *X, *U;

  // use REAL() to access the C array inside the numeric vector passed in from R
  X = REAL(XX);
  U = REAL(UU);
  int P = asInteger(PP);
  int K = asInteger(KK);
  int K2 = K * K;

  // allocate and compute the result matrix
  SEXP Z4 = PROTECT(allocMatrix(REALSXP, P, K * K * K));
  double *rZ4 = REAL(Z4);
  for (int ii = 0; ii < P * K * K * K; ii++) {
    rZ4[ii] = 0.0;
  }

  // loop over the cokurtosis elements
  int iter = 0;
  for (int ii = 0; ii < P; ii++) {
    for (int jj = ii; jj < P; jj++) {
      for (int kk = jj; kk < P; kk++) {
        for (int ll = kk; ll < P; ll++) {
          if (ii == jj) {
            if (jj == kk) {
              if (kk == ll) {
                // psi_iiii
                for (int uu = 0; uu < K; uu++) {
                  for (int vv = 0; vv < K; vv++) {
                    for (int ww = 0; ww < K; ww++) {
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * U[uu * P + ii] * U[vv * P + ii] * U[ww * P + ii];
                    }
                  }
                }
              } else {
                // psi_iiil
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + ii] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + ll] += X[iter] * U[uuP + ii] * U[vvP + ii] * U[wwP + ii];
                    }
                  }
                }
              }
            } else {
              if (kk == ll) {
                // psi_iikk
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + ii] * U[vvP + kk] * U[wwP + kk] +
                        U[uuP + kk] * U[vvP + ii] * U[wwP + kk] + U[uuP + kk] * U[vvP + kk] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + kk] += X[iter] * (U[uuP + ii] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + ii] * U[vvP + kk] * U[wwP + ii] + U[uuP + kk] * U[vvP + ii] * U[wwP + ii]);
                    }
                  }
                }
              } else {
                // psi_iikl
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + ii] * U[vvP + kk] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + kk] + U[uuP + kk] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + kk] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + ll] * U[vvP + kk] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + kk] += X[iter] * (U[uuP + ii] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + ll] += X[iter] * (U[uuP + ii] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + ii] * U[vvP + kk] * U[wwP + ii] + U[uuP + kk] * U[vvP + ii] * U[wwP + ii]);
                    }
                  }
                }
              }
            }
          } else {
            if (jj == kk) {
              if (kk == ll) {
                // psi_ijjj
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * U[uuP + jj] * U[vvP + jj] * U[wwP + jj];
                      rZ4[(ww * K2 + vv * K + uu) * P + jj] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + jj] +
                        U[uuP + jj] * U[vvP + ii] * U[wwP + jj] + U[uuP + jj] * U[vvP + jj] * U[wwP + ii]);
                    }
                  }
                }
              } else {
                // psi_ijjl
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + jj] * U[vvP + jj] * U[wwP + ll] +
                        U[uuP + jj] * U[vvP + ll] * U[wwP + jj] + U[uuP + ll] * U[vvP + jj] * U[wwP + jj]);
                      rZ4[(ww * K2 + vv * K + uu) * P + jj] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + jj] + U[uuP + jj] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + jj] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + jj] +
                        U[uuP + ll] * U[vvP + jj] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + ll] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + jj] +
                        U[uuP + jj] * U[vvP + ii] * U[wwP + jj] + U[uuP + jj] * U[vvP + jj] * U[wwP + ii]);
                    }
                  }
                }
              }
            } else {
              if (kk == ll) {
                // psi_ijkk
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + jj] * U[vvP + kk] * U[wwP + kk] +
                        U[uuP + kk] * U[vvP + jj] * U[wwP + kk] + U[uuP + kk] * U[vvP + kk] * U[wwP + jj]);
                      rZ4[(ww * K2 + vv * K + uu) * P + jj] += X[iter] * (U[uuP + ii] * U[vvP + kk] * U[wwP + kk] +
                        U[uuP + kk] * U[vvP + ii] * U[wwP + kk] + U[uuP + kk] * U[vvP + kk] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + kk] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + kk] +
                        U[uuP + ii] * U[vvP + kk] * U[wwP + jj] + U[uuP + jj] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + jj] * U[vvP + kk] * U[wwP + ii] + U[uuP + kk] * U[vvP + ii] * U[wwP + jj] +
                        U[uuP + kk] * U[vvP + jj] * U[wwP + ii]);
                    }
                  }
                }
              } else {
                // psi_ijkl
                for (int uu = 0; uu < K; uu++) {
                  int uuP = uu * P;
                  for (int vv = 0; vv < K; vv++) {
                    int vvP = vv * P;
                    for (int ww = 0; ww < K; ww++) {
                      int wwP = ww * P;
                      rZ4[(ww * K2 + vv * K + uu) * P + ii] += X[iter] * (U[uuP + jj] * U[vvP + kk] * U[wwP + ll] +
                        U[uuP + jj] * U[vvP + ll] * U[wwP + kk] + U[uuP + kk] * U[vvP + jj] * U[wwP + ll] +
                        U[uuP + kk] * U[vvP + ll] * U[wwP + jj] + U[uuP + ll] * U[vvP + jj] * U[wwP + kk] +
                        U[uuP + ll] * U[vvP + kk] * U[wwP + jj]);
                      rZ4[(ww * K2 + vv * K + uu) * P + jj] += X[iter] * (U[uuP + ii] * U[vvP + kk] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + kk] + U[uuP + kk] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + kk] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + ll] * U[vvP + kk] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + kk] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + ll] +
                        U[uuP + ii] * U[vvP + ll] * U[wwP + jj] + U[uuP + jj] * U[vvP + ii] * U[wwP + ll] +
                        U[uuP + jj] * U[vvP + ll] * U[wwP + ii] + U[uuP + ll] * U[vvP + ii] * U[wwP + jj] +
                        U[uuP + ll] * U[vvP + jj] * U[wwP + ii]);
                      rZ4[(ww * K2 + vv * K + uu) * P + ll] += X[iter] * (U[uuP + ii] * U[vvP + jj] * U[wwP + kk] +
                        U[uuP + ii] * U[vvP + kk] * U[wwP + jj] + U[uuP + jj] * U[vvP + ii] * U[wwP + kk] +
                        U[uuP + jj] * U[vvP + kk] * U[wwP + ii] + U[uuP + kk] * U[vvP + ii] * U[wwP + jj] +
                        U[uuP + kk] * U[vvP + jj] * U[wwP + ii]);
                    }
                  }
                }
              }
            }
          }
          iter++;
        } // loop ll
      } // loop kk
    } // loop jj
  } // loop ii

  UNPROTECT(1);
  return Z4;
}
