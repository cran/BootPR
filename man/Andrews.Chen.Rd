\name{Andrews.Chen}
\alias{Andrews.Chen}
\title{ Andrews-Chen median-unbiased estimation for AR models }
\description{
  This function returns the Andrews-Chen estimates for AR coefficients, residuals, and AR forecasts generated using the Andrews-Chen estimates
}
\usage{
Andrews.Chen(x, p, h, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast periods }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}

\value{

  \item{coef }{ Andrews-Chen median-unbiased estimates}
  \item{ecm.coef }{ the coefficients in the ADF form}
  \item{resid}{ residuals }
  \item{forecast}{ point forecasts from Andrews-Chen estimates}
 }
\references{ 

Kim, J.H., 2003, Forecasting Autoregressive Time Series with Bias-Corrected Parameter Estimators, International Journal of Forecasting, 19, 493-502. 

Andrews, D.W. K. (1993). Exactly median-unbiased estimation of first order autoregressive / unit root models. Econometrica, 61, 139-165.

Andrews, D.W. K., & Chen, H. -Y. (1994). Approximate median unbiased estimation of autoregressive models. Journal of Business & Economic Statistics, 12, 187-204.

 }
\author{ Jae H. Kim }
\note{ 
The Andrew-Chen estimator may break down when the AR order is very high. I recommend that AR order be kept low
}
\examples{
data(IPdata)
BootBC(IPdata,p=1,h=10,nboot=200,type="const+trend")
}
\keyword{ ts }
