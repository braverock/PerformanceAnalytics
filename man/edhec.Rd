\name{edhec}
\docType{data}
\alias{edhec}
\title{EDHEC-Risk Hedge Fund Style Indices}
\description{
EDHEC composite hedge fund style index returns.
}
\usage{edhec}
\details{
EDHEC Data used in PerformanceAnalytics and related publications with the kind permission of the EDHEC Risk and Asset Management Research Center.

The 'edhec' data set included with PerformanceAnalytics will be periodically updated (typically annually) to include additional observations.  If you intend to use this data set in automated tests, please be sure to subset your data like \code{edhec[1:120,]} to use the first ten years of observations.

From the EDHEC website:
\dQuote{The EDHEC Risk and Asset Management Research Centre plays a noted role in furthering applied financial research and systematically highlighting its practical uses. As part of its philosophy, the centre maintains a dialogue with professionals which benefits the industry as a whole. At the same time, its proprietary R&D provides sponsors with an edge over competition and joint ventures allow selected partners to develop new business opportunities.

To further assist financial institutions and investors implement the latest research advances in order to meet the challenges of the changing asset management landscape, the centre has spawned two consultancies and an executive education arm. Clients of these derivative activities include many of the leading organisations throughout Europe.}

see \url{http://www.edhec-risk.com/about_us}
}
\format{CSV conformed into an xts object with monthly observations}
\source{\url{http://www.edhec-risk.com/indexes/pure_style}}
\references{
About EDHEC Alternative Indexes. December 16, 2003. EDHEC-Risk. \cr
\url{http://www.edhec-risk.com/indexes/pure_style/about}

Vaissie Mathieu. A Detailed Analysis of the Construction Methods and Management Principles of Hedge Fund Indices. October 2003. EDHEC. \cr
\url{http://www.edhec-risk.com/site_edhecrisk/public/indexes/EDHEC_Publications/RISKReview1072705188065793513}
}
\examples{
data(edhec)

#preview the data
head(edhec)

#summary period statistics
summary(edhec)

#cumulative index returns
tail(cumprod(1+edhec),1)
}
\keyword{datasets}
