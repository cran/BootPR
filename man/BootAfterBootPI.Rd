\name{BootAfterBootPI}
\alias{BootAfterBootPI}
\title{ Bootstrap-after-Bootstrap Prediction }
\description{
  This function calculates bootstrap-after-bootstrap prediction intervals and bootstrap bias-corrected point forecasts
}
\usage{
BootAfterBootPI(x, p, h, nboot, prob, type)
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
Kim, J.H., 2001, Bootstrap-after-Bootstrap Prediction Intervals for Autoregressive Models, Journal of Business & Economic Statistics 19, 117-128

Kilian, L. (1998). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.


}
\author{ Jae H. Kim }



\examples{
data(IPdata)
BootAfterBootPI(IPdata,p=1,h=10,nboot=100,prob=c(0.05,0.95),type="const+trend")
}
\keyword{ ts }
