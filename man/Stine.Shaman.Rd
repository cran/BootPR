\name{Stine.Shaman}
\alias{Stine.Shaman}

\title{ bias-corrected estimation based on Shaman-Stine formula }
\description{
 The function returns parameter estimates and bias-corrected forecasts using Shaman and Stine bias formula for univariate AR models
}
\usage{
Stine.Shaman(x, p, h, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast period }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
  }

\value{
  \item{coef }{ Bias-corrected parameter estimates using Shama-Stine formula}
  \item{resid}{ residuals }
  \item{forecast}{ point forecasts from bias-corrected parameter estimates}
}
\references{ 
Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Shaman, P., & Stine, R. A. (1988). The bias of autoregressive coefficient estimators. Journal of the American Statistical Association, 83, 842-848.

Stine, R. A., & Shaman, P. (1989). A fixed point characterization for bias of autoregressive estimators. The Annals of Statistics,17, 1275-1284.

Kilian, L. (1998a). Small sample confidence intervals for impulse response functions. The Review of Economics and Statistics, 80,218-230.


}
\author{ Jae H. Kim }

\examples{
data(IPdata)
Stine.Shaman(IPdata,p=6,h=10,type="const+trend")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts }
