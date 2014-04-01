\name{Plot.Fore}
\alias{Plot.Fore}

\title{ Plotting point forecasts }
\description{
  The function returns plots the point forecasts
}

\usage{
Plot.Fore(x, fore, start, end, frequency)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{fore}{ point forecasts }
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

\author{ Jae H. Kim }

\examples{
data(IPdata)
BootF <- BootBC(IPdata,p=1,h=10,nboot=100,type="const+trend")
Plot.Fore(IPdata,BootF$forecast,start=1860,end=1988,frequency=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }