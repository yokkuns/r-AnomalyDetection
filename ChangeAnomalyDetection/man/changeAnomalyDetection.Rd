\name{changeAnomalyDetection}
\alias{changeAnomalyDetection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Anomaly Detection of change
}
\description{
Anomaly Detection of change.
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
anomaly description score of change
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Yohei Sato
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ttrc)
x <- ttrc[1:100,"Close"]

change.score <- changeAnomalyDetection(x, term=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
