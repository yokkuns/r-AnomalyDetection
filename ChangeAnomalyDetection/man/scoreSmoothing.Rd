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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  smoothing data
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
