# Diplay mesh points based on Gaussian-Hermite quadrature
# This script complements the article
#  "Fully Flexible Extreme Views"
#	by A. Meucci, D. Ardia, S. Keel
#	available at www.ssrn.com
# The most recent version of this code is available at
# MATLAB Central - File Exchange

N = 50; 
X = matrix( data=NA, nrow=N, ncol=N)

for (i in 1:N) {
  x = gaussHermiteMesh(i)
  X[1:length(x), i] = x
}

# mesh points
for (i in 1:N) {
  plot(1:N, t(X[i,]))
}