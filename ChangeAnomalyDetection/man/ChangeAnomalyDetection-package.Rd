\name{ChangeAnomalyDetection-package}
\alias{ChangeAnomalyDetection-package}
\alias{ChangeAnomalyDetection}
\docType{package}
\title{
ChangeAnomalyDetection Package
}
\description{
It is a R package for the detection of the point of a sharp turn of the behavior of the time series.
}
\details{
\tabular{ll}{
Package: \tab ChangeAnomalyDetection\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2012-09-29\cr
License: \tab What license is it under?\cr
}
}
\author{
Maintainer: Yohei Sato <yokkuns@tkul.jp>
}
\keyword{ package }
\examples{
data(ttrc)
x <- ttrc[1:100,"Close"]

change.score <- changeAnomalyDetection(x, term=10)
}
