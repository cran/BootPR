\name{Plot.PI}
\alias{Plot.PI}
\title{ Plotting prediction intervals and point forecasts }
\description{
  The function returns plots the point forecasts and prediction intervals
}
\usage{
Plot.PI(x, fore, Interval, start, end, frequency)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{fore}{ point forecasts }
  \item{Interval}{ Prediction Intervals }
  \item{start}{starting date }
  \item{end}{ending date}
  \item{frequency}{ data frequency }
}
\details{
  frequency=1 for annual data, 4 for quarterly data, 12 for monthly data

  start=c(1980,4) indicates April 1980 if frequency=12

  end = c(2000,1) indicates 1st quarter of 2000 if freqeuncy = 4}

\value{
  plot
}

\author{Jae H. Kim}

\examples{
data(IPdata)
PI <- ShamanStine.PI(IPdata,p=6,h=10,nboot=1000,prob=c(0.025,0.05,0.95,0.975),type="const+trend",correct="ssf",0)
Plot.PI(IPdata,PI$forecast,PI$PI,start=1860,end=1988,frequency=1)
}
\keyword{ ts }