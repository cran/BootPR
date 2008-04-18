\name{ARorder}
\alias{ARorder}
\title{ AR model order selection }
\description{
  AR model selection using AIC, BIC, HQ
}
\usage{
ARorder(x, pmax, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{pmax}{ the maximum AR order}
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}

\value{
  
  \item{ARorder }{AR orders selected by AIC, BIC and HQ}
  \item{Criteria }{the values of AIC, BIC and HQ}
}

\author{ Jae H. Kim }
\examples{
data(IPdata)
ARorder(IPdata,pmax=12,type="const+trend")}
\keyword{ ts }

