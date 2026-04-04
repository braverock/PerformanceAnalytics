\name{DJIA}
\alias{DJIA}
\docType{data}
\title{Daily Closing Prices of Stocks in The Dow Jones 30 Industrial Index}
\description{
 This data frame contains daily closing prices of stocks in Dow Jones 30
 Industrial index. Data covers a time period from 30-08-1993 till 29-08-2003.
 The composition of DJIA 30 corresponds to September 1st, 2003.
}
\usage{data(DJIA)}
\format{
  A data frame with 2521 observations on the following 31 variables.
  \describe{
    \item{Date}{POSIXt object containing dates.}
    \item{AA}{Quotes for ALCOA INC.}
    \item{AXP}{Quotes for AMERICAN EXPRESS CO.} 
    \item{BA}{Quotes for BOEING CO.}
    \item{C}{Quotes for CITIGROUP INC.}
    \item{CAT}{Quotes for CATERPILLAR INC.}
    \item{DD}{Quotes for DUPONT E I DE NEMOURS CO.}
    \item{DIS}{Quotes for DISNEY CO WALT HLDG CO.}
    \item{EK}{Quotes for EASTMAN KODAK CO.}
    \item{GE}{Quotes for GENERAL ELECTRIC CO.}
    \item{GM}{Quotes for GENERAL MOTORS CORP.}
    \item{HD}{Quotes for HOME DEPOT INC.}
    \item{HON}{ Quotes for HONEYWELL INTERNATIONAL.}
    \item{HPQ}{ Quotes for HEWLETT-PACKARD CO.}
    \item{IBM}{Quotes for INTERNATIONAL BUSINESS MACHINES CO.}
    \item{INTC}{Quotes for INTEL CO.}
    \item{IP}{ Quotes for INTERNATIONAL PAPER CO.}
    \item{JNJ}{ Quotes for JOHNSON & JOHNSON.}
    \item{JPM}{ Quotes for JP MORGAN CHASE AND CO.}
    \item{KO}{ Quotes for COCA COLA CO.}
    \item{MCD}{ Quotes for MCDONALDS CORP.}
    \item{MMM}{ Quotes for 3M COMPANY.}
    \item{MO}{Quotes for ALTRIA GROUP INC.}
    \item{MRK}{ Quotes for MERCK & CO INC.}
    \item{MSFT}{Quotes for MiCROSOFT CORPORATION.}
    \item{PG}{ Quotes for PROCTER & GAMBLE CO.}
    \item{SBC}{ Quotes for SBC COMMUNICATIONS INC.}
    \item{T}{ Quotes for AT & T CORP.}  
    \item{UTX}{ Quotes for UNITED TECHNOLOGIES CORP.}
    \item{WMT}{ Quotes for WAL-MART STORES INC.}
    \item{XOM}{ Quotes for EXXON MOBIL CORP.}
  }
}

\details{ 
The Dow Jones 30 Industrial Index (DJIA) is prepared and published by Dow Jones & Co. It
is one of the oldest and most widely quoted of all the market indicators. 
The Dow Jones Industrial Average is comprised of 30 stocks that are major factors in their industries, 
and widely held by individuals and institutional investors. 
These 30 stocks represent about a fifth of the \$ 8 trillion-plus market value of all U.S. stocks 
and about a fourth of the value of stocks listed on the New York Stock Exchange. Please refer
for further information, for example, \url{www.nasdaq.com} or \url{www.dowjones.com}.
}

\source{
  	\url{www.nasdaq.com}
}
\examples{
 data(DJIA)
 attach(DJIA)
 plot(Date,IBM, xlab = "Time", ylab = "Close Price", main = "Stock Close Prices vs. Day", type = "l")
 axis.POSIXct(1, Date)
 detach(DJIA)
}
\keyword{datasets}
