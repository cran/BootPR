\name{BootAfterBootPI}
\alias{BootAfterBootPI}
\title{ Bootstrap-after-Bootstrap Prediction }
\description{
  This function calculates bootstrap-after-bootstrap prediction intervals and bootstrap bias-corrected point forecasts
}
\usage{
BootAfterBootPI(x, p, h, nboot, prob, type, correct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast periods }
  \item{nboot}{number of bootstrap iterations }
  \item{prob}{a vector of probabilities }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{correct}{ "kilian" for Kilian's stationarity-correction, "ssf" for stationarity-correction based on stable spectral factorization}
}
\value{
  \item{PI }{ prediction intervals}
  \item{forecast }{bias-corrected point forecasts}
}
\references{    
Kim, J.H., 2001, Bootstrap-after-Bootstrap Prediction Intervals for Autoregressive Models, Journal of Business & Economic Statistics 19, 117-128

Kilian, L. (1998). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.

Kim, J.H. H. Song, K Wong., 2009, Bias-corrected Bootstrap Prediction Intervals for Autoregressive Model: New Alternatives with Applications to Tourism Forecasting, Journal of Forecasting, forthcoming.

}
\author{ Jae H. Kim }
\note{ See Kim et al. (2009) for the details of the stationarity-correction based on stable spectral factorization
}

\examples{
data(IPdata)
BootAfterBootPI(IPdata,p=6,h=10,nboot=1000,prob=c(0.5,0.95),type="const+trend",correct="ssf")
}
\keyword{ ts }
