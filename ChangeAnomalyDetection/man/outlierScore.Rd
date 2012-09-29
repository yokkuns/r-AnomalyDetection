\name{outlierScore}
\alias{outlierScore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
score of outlier.
}
\description{
score of outlier.
}
\usage{
outlierScore(train, x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
    train data.
}
  \item{x}{
    target data.
}
  \item{\dots}{
    ...
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  score of outlier
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
function (train, x, ...) 
{
    m <- mean(train)
    s <- sd(train)
    abs(-log(dnorm(x, m, s)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
