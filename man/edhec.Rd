\name{edhec}
\docType{data}
\alias{edhec}
\title{EDHEC-Risk Hedge Fund Style Indices}
\description{
EDHEC composite hedge fund style index returns.
}
\usage{data(edhec)}
\details{
EDHEC Data used in PerformanceAnalytics and related publications with the kind permission of the EDHEC Risk and Asset Management Research Center.

The 'edhec' data set included with PerformanceAnalytics will be periodically updated (typically annually) to include additional observations.  If you intend to use this data set in automated tests, please be sure to subset your data like \code{edhec[1:120,]} to use the first ten years of observations.

From the EDHEC website:
\dQuote{The EDHEC Risk and Asset Management Research Centre plays a noted role in furthering applied financial research and systematically highlighting its practical uses. As part of its philosophy, the centre maintains a dialogue with professionals which benefits the industry as a whole. At the same time, its proprietary R&D provides sponsors with an edge over competition and joint ventures allow selected partners to develop new business opportunities.

To further assist financial institutions and investors implement the latest research advances in order to meet the challenges of the changing asset management landscape, the centre has spawned two consultancies and an executive education arm. Clients of these derivative activities include many of the leading organisations throughout Europe.}

}
\format{CSV loaded into R and saved as an xts object with monthly observations. NOTE: In the era of CoVid-19, a few observations in the `Short Selling` index have not been reported. We chose to zero fill them at this time. These are observations on 2020-04-30, 2020-05-31, 2020-11-30, 2020-12-31, 2021-01-31, 2021-04-30, and 2021-05-31.}
\source{\url{https://climateimpact.edhec.edu/}}
\references{
About EDHEC Alternative Indexes. December 16, 2003. EDHEC-Risk. \cr

Vaissie Mathieu. A Detailed Analysis of the Construction Methods and Management Principles of Hedge Fund Indices. October 2003. EDHEC. \cr
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
