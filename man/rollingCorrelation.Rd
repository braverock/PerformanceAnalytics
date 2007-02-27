\name{rollingCorrelation}
\alias{rollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ rolling training period covariance/correlation }
\description{
This is a wrapper for providing n-period trailing correlations for the data provided.
}
\usage{
rollingCorrelation(x, y, n, trim = TRUE, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data for the x axis, can take matrix,vector, or timeseries }
  \item{y}{ data for the y axis, can take matrix,vector, or timeseries }
  \item{n}{ the number of periods over which a function is to be calculated. Use the value zero (0) to roll the statistic from inception }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
A data.table of n-period trailing correlations for each column in y.
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{
Inspired by \code{\link{fMultivar::rollFun}} written by Diethelm Wurtz.

Assumes that x and y are sequenced exactly the same and are regular.

Currently assumes that BOTH x and y are provided

@todo: Allow \emph{either} a matrix or data frame for 'x' \emph{or} give both 'x' and 'y' \code{\link{cov()}}
}
\seealso{
    \code{\link{cov()}}
}
\examples{
    # Example:
    # > head(rollingCorrelation(manager.ts@Data[,1],edhec.ts@Data,n=12))
    #            Convertible Arbitrage CTA Global Distressed Securities
    # 2003-11-28             0.2591101  0.2762218             0.7516556
    # 2003-12-31             0.2162078  0.2477113             0.7452179
    # 2004-01-30             0.3918575  0.3489062             0.7562063
    # 2004-02-27             0.5331404  0.3905645             0.7088004
    # 2004-03-31             0.5730389  0.3010877             0.5694478
    # 2004-04-30             0.5146946  0.3762283             0.4374524
    #            Emerging Markets Equity Market Neutral Event Driven
    # 2003-11-28        0.6678183             0.4133534    0.6872263
    # 2003-12-31        0.7739188             0.3758590    0.8494740
    # 2004-01-30        0.7805586             0.4148372    0.8338275
    # 2004-02-27        0.7353874             0.3444552    0.8181069
    # 2004-03-31        0.7072259             0.2740104    0.6270411
    # 2004-04-30        0.4430823             0.3918143    0.4966760
    #            Fixed Income Arbitrage Funds of Funds Global Macro Long/Short Equity
    # 2003-11-28              0.6681346      0.7413791    0.5698372         0.4429661
    # 2003-12-31              0.6583804      0.7705968    0.5647109         0.6949394
    # 2004-01-30              0.7116529      0.7427369    0.5972492         0.6849833
    # 2004-02-27              0.7580839      0.6831231    0.6555057         0.6300653
    # 2004-03-31              0.7057303      0.6172516    0.6253257         0.5233462
    # 2004-04-30              0.7583853      0.5305704    0.6740996         0.3518156
    #            Merger Arbitrage Relative Value Short Selling
    # 2003-11-28        0.6890493      0.6858798   -0.23238159
    # 2003-12-31        0.7397387      0.8062422   -0.41123820
    # 2004-01-30        0.7219990      0.7883262   -0.36321115
    # 2004-02-27        0.6862517      0.7605837   -0.28807086
    # 2004-03-31        0.5921739      0.6766063   -0.24154960
    # 2004-04-30        0.5073817      0.5257858   -0.04663322
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
