\name{changeAnomalyDetection}
\alias{changeAnomalyDetection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Anomaly Detection of change
}
\description{
Calculate the score of the degree of behavior change in time series.
}
\usage{
changeAnomalyDetection(x, term = 30, smooth.n = 7, order = c(1, 0, 0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Time-series numeric data
}
  \item{term}{
  training period.
}
  \item{smooth.n}{
  smoothing period.
}
  \item{order}{
  order parameter for arima function.
}
  \item{\dots}{
  parameter for arima function.
}
}
\value{
anomaly description score of change
}
\author{
Yohei Sato
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(ttrc)
x <- ttrc[1:100,"Close"]

change.score <- changeAnomalyDetection(x, term=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

