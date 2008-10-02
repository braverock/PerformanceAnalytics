\name{clean.boudt}
\alias{clean.boudt}
%\alias{Return.clean}

\title{ clean extreme observations in a time series to to provide more robust risk estimates }
\description{
Robustly clean a time series to reduce the magnitude, but not the number or direction, of observations that exceed the $1-\alpha\%$ risk threshold.
}
\usage{
clean.boudt(R, alpha = 0.01, trim = 0.001)
}

\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{alpha}{ probability to filter at 1-alpha, defaults to .01 (99\%)}
  \item{trim}{ where to set the "extremeness" of the Mahalanobis distance }
}

\details{
Many risk measures are calculated by using the first
two (four) moments of the asset or portfolio return distribution.
Portfolio moments are extremely sensitive to data spikes, and this
sensitivity is only exacerbated in a multivariate context.
For this reason, it seems appropriate to
consider estimates of the multivariate moments that are robust to
return observations that deviate extremely from the Gaussian
distribution.

There are two main approaches in defining robust
alternatives to estimate the multivariate moments by their sample
means (see e.g. Maronna[2006]). One approach is to
consider a more robust estimator than the sample means. Another one
is to first clean (in a robust way) the data and then take the
sample means and moments of the cleaned data.

Our cleaning method follows the second approach. It
is designed in such a way that, if we want to estimate downside risk
with loss probability $\alpha$, it will never clean observations
that belong to the $1-\alpha$ least extreme observations. Suppose we
have an $n$-dimensional vector time series of length $T$:
$r_1,...,r_T$. We clean this time series in three steps.

\begin{enumerate}
\item\emph{ Ranking the observations in function of their extremeness. }Denote $\mu$ and $\Sigma$ the mean and covariance matrix of the
bulk of the data and let $\lfloor \cdot \rfloor$ be the operator
that takes the integer part of its argument. As a measure of the
extremeness of the return observation $r_t$, we use its squared
Mahalanobis distance $ d^2_t = (r_t-\mu)'\Sigma^{-1}(r_t-\mu).$ We
follow Rousseeuw(1985)  by estimating $\mu$ and $\Sigma$ as
the mean vector and covariance matrix (corrected to ensure
consistency) of the subset of size $\lfloor (1-\alpha)T\rfloor$ for
which the determinant of the covariance matrix of the elements in
that subset is the smallest. These estimates will be robust against
the $\alpha$ most extreme returns. Let $d^2_{(1)},...,d^2_{(T)}$ be
the ordered sequence of the estimated squared Mahalanobis distances
such that $d^2_{(i)}\leq d^2_{(i+1)}$.

\item \emph{Outlier identification.} Return observations are qualified as outliers if their estimated
squared Mahalanobis distance $d^2_t$ is greater than the empirical
$1-\alpha$ quantile $d^2_{(\lfloor (1-\alpha)T \rfloor)}$ and
exceeds a very extreme quantile of the Chi squared distribution
function with $n$ degrees of freedom, which is the distribution
function of $d^2_t$ when the returns are normally distributed. In
the application we take the 99.9\% quantile, denoted
$\chi^2_{n,0.999}$.

\item \emph{Data cleaning. } Similarly as in Khan(2007) we only clean the returns that are identified
as outliers in step 2 by replacing these returns $r_t$ with
$$r_t\sqrt{\max(d^2_{(\lfloor
(1-\alpha)T)\rfloor},\chi^2_{n,0.999})/d^2_t}.$$ The cleaned return
vector has the same orientation as the original return vector, but
its magnitude is smaller. Khan(2007) calls this
procedure of limiting the value of $d^2_t$ to a quantile of the
$\chi^2_n$ distribution, ``multivariate Winsorization'.

\end{enumerate}

Note that the primary value of data cleaning lies in creating a more
robust and stable estimation of the distribution generating the
large majority of the return data. The increased robustness and
stability of the estimated moments utilizing cleaned data should be
used for portfolio construction. If a portfolio manager wishes to
have a more conservative risk estimate, cleaning may not be
indicated for risk monitoring. It is also important to note that the
robust method proposed here does not remove data from the series,
but only decreases the magnitude of the extreme events. It may also
be appropriate in practice to use a cleaning threshold somewhat
outside the VaR threshold that the manager wishes to consider. In
actual practice, it is probably best to back-test the results of
both cleaned and uncleaned series to see what works best with the
particular combination of assets under consideration.
}
\value{
cleaned data matrix
}
\references{
Boudt, K., Peterson, B. G., Croux, C., 2008. Estimation
and Decomposition of Downside Risk for Portfolios with Non-Normal Returns. Journal of
Risk, forthcoming.

Khan, J. A., S. Van Aelst, and R. H. Zamar (2007). Robust linear model
selection based on least angle regression. Journal of the American Statistical
Association 102.

Maronna, R. A., D. R. Martin, and V. J. Yohai (2006). Robust Statistics:
Theory and Methods. Wiley.

 Rousseeuw, P. J. (1985). Multivariate estimation with high breakdown point.
In W. Grossmann, G. Pflug, I. Vincze, and W. Wertz (Eds.), Mathematical
Statistics and Its Applications, Volume B, pp. 283?297. Dordrecht-Reidel.
}

\author{
Kris Boudt,
Brian G. Peterson
}
\note{
This function and much of this text was originally written for Boudt, et. al, 2008
}

\seealso{ \code{\link{Return.clean}} }

%\examples{
%}

\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
