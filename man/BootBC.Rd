\name{BootBC}
\alias{BootBC}

\title{Bootstrap bias-corrected estimation and forecasting for AR models }
\description{
 This function returns bias-corrected parameter estimates and forecasts for univariate AR models.
}
\usage{
BootBC(x, p, h, nboot, type, correct)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast period }
  \item{nboot}{number of bootstrap iterations }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  \item{correct}{ "kilian" for Kilian's stationarity-correction; "ssf" for stationarity-correction based on stable spectral factorization}
}

\value{
  \item{coef }{ Bootstrap bias-corrected parameter estimates}
  \item{resid}{ residuals }
  \item{forecast}{point forecasts from bootstrap bias-corrected parameter estimates}
}
\references{ 
Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Kilian, L. (1998a). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.

Kim, J.H. H. Song, K Wong., 2009, Bias-corrected Bootstrap Prediction Intervals for Autoregressive Model: New Alternatives with Applications to Tourism Forecasting, Journal of Forecasting, forthcoming.

 }
\author{Jae H. Kim}
\note{ See Kim et al. (2009) for the details of the stationarity-correction based on stable spectral factorization
}

\examples{
data(IPdata)
BootBC(IPdata,p=6,h=10,nboot=500,type="const+trend",correct="ssf")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
