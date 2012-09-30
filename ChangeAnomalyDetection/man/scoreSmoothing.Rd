\name{scoreSmoothing}
\alias{scoreSmoothing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  smoothing for score
}
\description{
  smoothing for score
}
\usage{
scoreSmoothing(x, n = 7, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    numeric
}
  \item{n}{
    smoothing period
}
  \item{\dots}{
    ...
}
}
\value{
  smoothing data
}
\author{
  Yohei Sato
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, n = 7, ...) 
{
    SMA(x, n)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
