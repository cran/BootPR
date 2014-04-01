\name{Roy.Fuller}
\alias{Roy.Fuller}

\title{ Roy-Fuller median-unbiased estimation }
\description{
  This function returns parameter estimates and forecasts based on Roy-Fuller medin-unbiased estimator for AR models
}
\usage{
Roy.Fuller(x, p, h, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast period }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}

\value{
  \item{coef }{ Roy-Fuller parameter estimates}
  \item{resid}{ residuals }
  \item{forecast}{ point forecasts from Roy-Fuller parameter estimates}
}
\references{
Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Roy, A., & Fuller, W. A. (2001). Estimation for autoregressive time series with a root near one. Journal of Business & Economic Statistics, 19(4), 482-493.
}
\author{ Jae H. Kim }

\examples{
data(IPdata)
Roy.Fuller(IPdata,p=6,h=10,type="const+trend")
}

\keyword{ts}
