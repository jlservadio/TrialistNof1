\name{maxbgrRatio}
\alias{maxbgrRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
maxbgrRatio(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (x) 
{
    t1 = mcmc(x[, 1, drop = F])
    t2 = mcmc(x[, 2, drop = F])
    t3 = mcmc(x[, 3, drop = F])
    cols.to.drop = seq(dim(t1)[2])[apply(t1, 2, var) == 0]
    if (length(cols.to.drop > 0)) {
        t1 = t1[, -cols.to.drop, drop = F]
        t2 = t2[, -cols.to.drop, drop = F]
        t3 = t3[, -cols.to.drop, drop = F]
    }
    max.bgrRatio = max(gelman.diag(list(t1, t2, t3), autoburnin = F)[[1]][, 
        1])
    return(max.bgrRatio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
