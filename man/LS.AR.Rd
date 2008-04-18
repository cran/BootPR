\name{LS.AR}
\alias{LS.AR}

\title{ OLS parameter estimates and forecasts, no bias-correction}
\description{
  The function returns parameter estimates and forecasts from OLS estimation for AR models
}
\usage{
LS.AR(x, p, h, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time series data set}
  \item{p}{ AR order }
  \item{h}{ the number of forecast period }
  \item{type}{ "const" for the AR model with intercept only, "const+trend" for the AR model with intercept and trend }
}
\value{
  \item{coef }{ OLS parameter estimates}
  \item{resid}{ OLS residuals }
  \item{forecast}{ point forecasts from OLS parameter estimates}
}

\author{ Jae H. Kim }

\examples{
data(IPdata)
LS.AR(IPdata,p=6,h=10,type="const+trend")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{ ts }