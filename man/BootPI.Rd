\name{BootPI}
\alias{BootPI}
\title{ Bootstrap prediction intevals and point forecasts with no bias-correction}
\description{This function returns bootstrap forecasts and prediction intervals with no bias-correction
}
\usage{
BootPI(x, p, h, nboot, prob, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast periods }
  \item{nboot}{number of bootstrap iterations }
  \item{prob}{a vector of probabilities }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}
\value{
  \item{PI }{ prediction intervals}
  \item{forecast }{bias-corrected point forecasts}
}
\references{
Thombs, L. A., & Schucany, W. R. (1990). Bootstrap prediction intervals for autoregression. Journal of the American Statistical Association, 85, 486-492.
 }
\author{ Jae H. Kim }

\examples{
data(IPdata)
BootPI(IPdata,p=1,h=10,nboot=100,prob=c(0.05,0.95),type="const+trend")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts}
