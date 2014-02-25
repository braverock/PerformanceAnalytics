\name{lpm}
\alias{lpm}
\title{calculate the lower partial moment of a time series}
\usage{
lpm(R, n, threshold, about_mean = FALSE)
}
\arguments{
  \item{R}{xts data}

  \item{n}{the n-th moment to return}

  \item{threshold}{threshold can be the mean or any point
  as desired}

  \item{about_mean}{TRUE/FALSE calculate LPM about the mean
  under the threshold or use the threshold to calculate the
  LPM around (if FALSE)}
}
\description{
Code to calculate the Lower Partion Moments around the mean
or a specified threshold from Huffman S,P & Moll C.R. 2011
"The impact of Asymmetry on Expected Stock Returns: An
Investigation of Alternative Risk Measures" Algorithmic
Finance 1 (2011) 79-93
}
\author{
Kyle Balkissoon \email{kylebalkissoon@gmail.com}
\email{kyle@corporateknights.com}
}

