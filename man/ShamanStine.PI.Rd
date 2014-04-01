\name{ShamanStine.PI}
\alias{ShamanStine.PI}
\title{ Bootstrap prediction interval using Shaman and Stine bias formula}
\description{
  The function returns bias-corrected forecasts and bootstrap prediction intervals using Shaman and Stine bias formula for univariate AR models
}
\usage{
ShamanStine.PI(x, p, h, nboot, prob, type, pmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast periods }
  \item{nboot}{number of bootstrap iterations }
  \item{prob}{a vector of probability values }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
    \item{pmax}{ for exogenous lag order algorithm, pmax = 0, for endogenous lag order algorithm, pmax is an integer greater than 0 }
}
\value{

  \item{PI }{ prediction intervals}
  \item{forecast }{bias-corrected point forecasts}

}
\references{
Kim, J.H., 2004, Bootstrap Prediction Intervals for Autoregression using Asymptotically Mean-Unbiased Parameter Estimators, International Journal of Forecasting, 20, 85-97.

Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Shaman, P., & Stine, R. A. (1988). The bias of autoregressive coefficient estimators. Journal of the American Statistical Association, 83, 842-848.

Stine, R. A., & Shaman, P. (1989). A fixed point characterization for bias of autoregressive estimators. The Annals of Statistics,17, 1275-1284.

Kilian, L. (1998a). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.


}
\author{ Jae H. Kim }


\examples{
data(IPdata)
ShamanStine.PI(IPdata,p=1,h=10,nboot=100,prob=c(0.05,0.95),type="const+trend",pmax=0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
