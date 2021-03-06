\name{BootBC}
\alias{BootBC}

\title{Bootstrap bias-corrected estimation and forecasting for AR models }
\description{
 This function returns bias-corrected parameter estimates and forecasts for univariate AR models.
}
\usage{
BootBC(x, p, h, nboot, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast period }
  \item{nboot}{number of bootstrap iterations }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  }

\value{
  \item{coef }{ Bootstrap bias-corrected parameter estimates}
  \item{resid}{ residuals }
  \item{forecast}{point forecasts from bootstrap bias-corrected parameter estimates}
}
\references{ 
Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Kilian, L. (1998a). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.

 }
\author{Jae H. Kim}



\examples{
data(IPdata)
BootBC(IPdata,p=1,h=10,nboot=100,type="const+trend")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
